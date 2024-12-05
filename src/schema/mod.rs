use std::{
    cmp::Ordering,
    iter::{once, repeat},
    ops::Deref,
};

mod value;
pub use value::*;
mod array;
pub use array::*;
mod tuple;
pub use tuple::*;
mod _struct;
pub use _struct::*;
mod map;
pub use map::*;

use nu_plugin::EngineInterface;
use nu_protocol::{
    engine::Closure, record, CustomValue, FromValue, IntRange, IntoSpanned, IntoValue, Range,
    Record, ShellError, Span, Spanned, Type, Value,
};
use serde::{
    de::{Error, Visitor},
    ser::SerializeSeq,
    Deserialize, Serialize,
};
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum SchemaError {
    #[error("Schema definition invalid at {span:?}: {msg:?}")]
    Schema { span: Span, msg: String },
    #[error("Value at {value_span:?} failed schema at {schema_span:?}: {msg:?}")]
    Value {
        schema_span: Span,
        value_span: Span,
        msg: String,
    },
    #[error("Custom schema errored")]
    Custom {
        schema_span: Span,
        value_span: Span,
        error: ShellError,
    },
    #[error(transparent)]
    Shell(#[from] ShellError),
}
impl IntoValue for SchemaError {
    #[inline]
    fn into_value(self, span: Span) -> Value {
        #[inline]
        fn string_into_value(str: impl Into<String>) -> Value {
            Value::string(str, Span::unknown())
        }
        #[inline]
        fn span_into_value(span: Span) -> Value {
            Value::record(
                record!(
                    "start" => Value::int(span.start as i64, Span::unknown()),
                    "end" => Value::int(span.end as i64, Span::unknown())
                ),
                Span::unknown(),
            )
        }
        let record = match self {
            Self::Schema { span, msg } => {
                record!(
                    "type" => string_into_value("schema"),
                    "msg" => string_into_value(msg),
                    "span" => span_into_value(span)
                )
            }
            Self::Value {
                schema_span,
                value_span,
                msg,
            } => {
                record!(
                    "type" => string_into_value("value"),
                    "schema_span" => span_into_value(schema_span),
                    "value_span" => span_into_value(value_span),
                    "msg" => string_into_value(msg)
                )
            }
            Self::Custom {
                schema_span,
                value_span,
                error,
            } => {
                record!(
                    "type" => string_into_value("custom"),
                    "schema_span" => span_into_value(schema_span),
                    "value_span" => span_into_value(value_span),
                    "msg" => string_into_value(error.to_string())
                )
            }
            Self::Shell(error) => record!(
                "type" => string_into_value("shell"),
                "msg" => string_into_value(error.to_string())
            ),
        };
        Value::record(record, span)
    }
}

/// Create a [`Type`] from the corresponding name.
/// Only types supported by [`Schema::Type`] will be converted, passing unsupported types will return `None`.
/// Specifically complex types are not supported, as they are handled by other [`Schema`] variants.
#[inline]
pub fn type_from_typename(r#type: &str) -> Option<Type> {
    match r#type {
        "binary" => Some(Type::Binary),
        "bool" => Some(Type::Bool),
        "date" => Some(Type::Date),
        "duration" => Some(Type::Duration),
        "filesize" => Some(Type::Filesize),
        "float" => Some(Type::Float),
        "int" => Some(Type::Int),
        "nothing" => Some(Type::Nothing),
        "range" => Some(Type::Range),
        "string" => Some(Type::String),
        "glob" => Some(Type::Glob),
        "cell-path" => Some(Type::CellPath),
        "closure" => Some(Type::Closure),
        // NOTE: other custom types cannot be sent to this plugin, so only schema is possible
        "schema" => Some(Schema::r#type()),
        _ => None,
    }
}

/// Representation of a schema that can be applied to a [`Value`].
#[derive(Debug, Clone)]
pub enum Schema {
    /// Test against [`Type`].
    Type(Spanned<Type>),
    /// Test against [`Value`].
    Value(Value),
    /// Ignore input and return given [`Value`].
    Fallback(Value),
    /// Test against multiple schema, will accept first passing schema.
    /// Passing no schema will cause this to always fail.
    Any(Spanned<Box<[Schema]>>),
    /// Test against multiple schema, will accept last schema when all are passing.
    /// Passing no schema will cause this to always succeed, in that case the input value is passed through.
    All(Spanned<Box<[Schema]>>),
    /// Descibes a heterogeneous list.
    Tuple {
        /// Inner schema per element.
        items: Spanned<Box<[Schema]>>,
        /// Missing elements will be treated as `null`.
        wrap_missing: Spanned<bool>,
        /// Allow wrapping single non-list, non-null value into 1-length list.
        wrap_single: Spanned<bool>,
        /// Allow treating `null` as empty list.
        wrap_null: Spanned<bool>,
        span: Span,
    },
    /// Describes a homogeneous list.
    Array {
        /// Inner schema for all elements.
        items: Spanned<Box<Schema>>,
        /// Length restriction.
        length: Spanned<IntRange>,
        /// Allow wrapping single non-list, non-null value into 1-length list.
        wrap_single: Spanned<bool>,
        /// Allow treating `null` as empty list.
        wrap_null: Spanned<bool>,
        span: Span,
    },
    /// Describes a heterogeneous record.
    Struct {
        /// Inner schema per field.
        fields: Spanned<Box<[(String, Schema)]>>,
        /// Missing fields will be treated as `null`.
        wrap_missing: Spanned<bool>,
        /// Allow wrapping lists into record in order of defined fields.
        wrap_list: Spanned<bool>,
        /// Allow wrapping non-record, non-list value into single field record.
        wrap_single: Spanned<bool>,
        /// Allow treating `null` as empty record.
        wrap_null: Spanned<bool>,
        span: Span,
    },
    /// Describes a homegeneous record
    Map {
        /// Inner schema for keys.
        keys: Option<Spanned<Box<Schema>>>,
        /// Inner schema for values.
        values: Option<Spanned<Box<Schema>>>,
        /// Length restriction.
        length: Spanned<IntRange>,
        /// Allow treating `null` as empty record.
        wrap_null: Spanned<bool>,
        span: Span,
    },
    /// Run custom code.
    /// Closure will be called equivalent to
    /// ```nu
    /// $value | do $closure
    /// ```
    /// The return value is expected to be one of
    /// - `{ok: value}`: Schema is succesfull and `value` is the result
    /// - `{err: msg}`: Schema failed with error message `msg`
    Custom(Spanned<Closure>),
}
impl PartialEq for Schema {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Type(r#type) => {
                let Self::Type(other) = other else {
                    return false;
                };
                r#type.item == other.item
            }
            Self::Value(value) => {
                let Self::Value(other) = other else {
                    return false;
                };
                value == other
            }
            Self::Fallback(value) => {
                let Self::Fallback(other) = other else {
                    return false;
                };
                value == other
            }
            Self::Any(options) => {
                let Self::Any(other) = other else {
                    return false;
                };
                options.as_ref().item == other.as_ref().item
            }
            Self::All(options) => {
                let Self::All(other) = other else {
                    return false;
                };
                options.as_ref().item == other.as_ref().item
            }
            Self::Tuple {
                items,
                wrap_single,
                wrap_null,
                ..
            } => {
                let Self::Tuple {
                    items: items_,
                    wrap_single: wrap_single_,
                    wrap_null: wrap_null_,
                    ..
                } = other
                else {
                    return false;
                };
                items.as_ref().item == items_.as_ref().item
                    && wrap_single.item == wrap_single_.item
                    && wrap_null.item == wrap_null_.item
            }
            Self::Array {
                items,
                length,
                wrap_single,
                wrap_null,
                ..
            } => {
                let Self::Array {
                    items: items_,
                    length: length_,
                    wrap_single: wrap_single_,
                    wrap_null: wrap_null_,
                    ..
                } = other
                else {
                    return false;
                };
                items.as_ref().item == items_.as_ref().item
                    && length.as_ref().item == length_.as_ref().item
                    && wrap_single.item == wrap_single_.item
                    && wrap_null.item == wrap_null_.item
            }
            Self::Struct {
                fields,
                wrap_missing,
                ..
            } => {
                let Self::Struct {
                    fields: fields_,
                    wrap_missing: wrap_missing_,
                    ..
                } = other
                else {
                    return false;
                };
                fields.as_ref().item == fields_.as_ref().item
                    && wrap_missing.item == wrap_missing_.item
            }
            Self::Map {
                keys,
                values,
                length,
                wrap_null,
                ..
            } => {
                #[inline]
                fn cmp(a: &Option<Spanned<Box<Schema>>>, b: &Option<Spanned<Box<Schema>>>) -> bool {
                    a.is_none() && b.is_none()
                        || a.as_ref().is_some_and(|a| {
                            b.as_ref()
                                .is_some_and(|b| a.as_ref().item == b.as_ref().item)
                        })
                }
                let Self::Map {
                    keys: keys_,
                    values: values_,
                    length: length_,
                    wrap_null: wrap_null_,
                    ..
                } = other
                else {
                    return false;
                };
                cmp(keys, keys_)
                    && cmp(values, values_)
                    && length.as_ref().item == length_.as_ref().item
                    && wrap_null.item == wrap_null_.item
            }
            Self::Custom(closure) => {
                let Self::Custom(other) = other else {
                    return false;
                };
                let closure = closure.as_ref().item;
                let other = other.as_ref().item;
                closure.block_id == other.block_id && closure.captures == other.captures
            }
        }
    }
}
impl PartialOrd for Schema {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.eq(other) {
            Some(Ordering::Equal)
        } else {
            None
        }
    }
}

#[inline]
fn span_fallback(a: Span, b: Span) -> Span {
    if a == Span::unknown() {
        b
    } else {
        a
    }
}

#[typetag::serde]
impl CustomValue for Schema {
    #[inline(always)]
    fn type_name(&self) -> String {
        Self::TYPE_NAME.to_string()
    }
    #[inline]
    fn clone_value(&self, span: Span) -> Value {
        Value::custom(Box::new(self.clone()), span)
    }
    fn partial_cmp(&self, other: &Value) -> Option<Ordering> {
        match other {
            Value::Custom { val, .. } => val
                .as_any()
                .downcast_ref()
                .and_then(|other| <Schema as PartialOrd>::partial_cmp(self, other)),
            _ => None,
        }
    }
    fn to_base_value(&self, span: Span) -> Result<Value, ShellError> {
        let result =
            match self {
                Self::Type(r#type) => {
                    let typename = match &r#type.item {
                        Type::Custom(subtype) if subtype.deref() == "Schema" => "schema".into(),
                        other => other.get_non_specified_string(),
                    };
                    record!("type" => Value::string(typename, r#type.span))
                        .into_spanned(span_fallback(span, r#type.span))
                }
                Self::Value(value) => record!("value" => value.clone())
                    .into_spanned(span_fallback(span, value.span())),
                Self::Fallback(value) => record!("fallback" => value.clone())
                    .into_spanned(span_fallback(span, value.span())),
                Self::Any(schemas) => record!(
                    "any" => schemas.as_ref().item.iter()
                        .map(|s| s.to_base_value(Span::unknown()))
                        .collect::<Result<Vec<Value>, _>>()?
                        .into_value(schemas.span)
                )
                .into_spanned(span_fallback(span, schemas.span)),
                Self::All(schemas) => record!(
                    "all" => schemas.as_ref().item.iter()
                        .map(|s| s.to_base_value(Span::unknown()))
                        .collect::<Result<Vec<Value>, _>>()?
                        .into_value(schemas.span)
                )
                .into_spanned(span_fallback(span, schemas.span)),
                Self::Tuple {
                    items,
                    wrap_missing,
                    wrap_single,
                    wrap_null,
                    span: s,
                } => record!(
                    "list" => "tuple".to_string().into_value(Span::unknown()),
                    "items" => items.as_ref().item.iter()
                        .map(|s| s.to_base_value(Span::unknown()))
                        .collect::<Result<Vec<Value>, _>>()?
                        .into_value(items.span),
                    "wrap_missing" => Value::bool(wrap_missing.item, wrap_missing.span),
                    "wrap_single" => Value::bool(wrap_single.item, wrap_single.span),
                    "wrap_null" => Value::bool(wrap_null.item, wrap_null.span),
                )
                .into_spanned(span_fallback(span, *s)),
                Self::Array {
                    items,
                    length,
                    wrap_single,
                    wrap_null,
                    span: s,
                } => record!(
                    "list" => Value::string("array".to_string(), Span::unknown()),
                    "items" => items.as_ref().item.clone().to_base_value(items.span)?,
                    "length" => Value::range(Range::IntRange(length.item), length.span),
                    "wrap_single" => Value::bool(wrap_single.item, wrap_single.span),
                    "wrap_null" => Value::bool(wrap_null.item, wrap_null.span),
                )
                .into_spanned(span_fallback(span, *s)),
                Self::Struct {
                    fields,
                    wrap_missing,
                    wrap_list,
                    wrap_single,
                    wrap_null,
                    span: s,
                } => {
                    let mut record = Record::new();
                    for (key, value) in fields.as_ref().item.iter() {
                        record.push(key, value.to_base_value(Span::unknown())?);
                    }
                    record!(
                        "record" => Value::string("struct".to_string(), Span::unknown()),
                        "fields" => Value::record(record, fields.span),
                        "wrap_missing" => Value::bool(wrap_missing.item, wrap_missing.span),
                        "wrap_list" => Value::bool(wrap_list.item, wrap_list.span),
                        "wrap_single" => Value::bool(wrap_single.item, wrap_single.span),
                        "wrap_null" => Value::bool(wrap_null.item, wrap_null.span),
                    )
                    .into_spanned(span_fallback(span, *s))
                }
                Self::Map {
                    keys,
                    values,
                    length,
                    wrap_null,
                    span: s,
                } => record!(
                    "record" => Value::string("map".to_string(), Span::unknown()),
                    "keys" => keys.as_ref()
                        .map(|s| s.as_ref().item.to_base_value(Span::unknown()))
                        .transpose()?
                        .unwrap_or(Value::nothing(Span::unknown())),
                    "values" => values.as_ref()
                        .map(|s| s.as_ref().item.to_base_value(Span::unknown()))
                        .transpose()?
                        .unwrap_or(Value::nothing(Span::unknown())),
                    "length" => Value::range(Range::IntRange(length.item), length.span),
                    "wrap_null" => Value::bool(wrap_null.item, wrap_null.span),
                )
                .into_spanned(span_fallback(span, *s)),
                Self::Custom(closure) => record!(
                    "custom" => Value::closure(closure.item.clone(), closure.span),
                )
                .into_spanned(span_fallback(span, closure.span)),
            };
        Ok(Value::record(result.item, result.span))
    }
    #[inline(always)]
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    #[inline(always)]
    fn as_mut_any(&mut self) -> &mut dyn std::any::Any {
        self
    }
}
#[inline]
pub fn unbounded() -> Spanned<IntRange> {
    IntRange::new(
        Value::int(0, Span::unknown()),
        Value::nothing(Span::unknown()),
        Value::nothing(Span::unknown()),
        nu_protocol::ast::RangeInclusion::RightExclusive,
        Span::unknown(),
    )
    .unwrap()
    .into_spanned(Span::unknown())
}

impl FromValue for Schema {
    fn from_value(v: Value) -> Result<Self, ShellError> {
        #[inline]
        fn convert_error(from: &str, span: Span, help: &str) -> ShellError {
            ShellError::CantConvert {
                to_type: Schema::TYPE_NAME.to_string(),
                from_type: from.to_string(),
                span,
                help: Some(help.to_string()),
            }
        }
        #[inline]
        fn to_bool(value: Option<&Value>, default: bool) -> Result<Spanned<bool>, ShellError> {
            value.map_or_else(
                || Ok(default.into_spanned(Span::unknown())),
                |inner| -> Result<_, ShellError> {
                    let span = inner.span();
                    Ok(inner.as_bool()?.into_spanned(span))
                },
            )
        }
        #[inline]
        fn to_single(value: &Value) -> Result<Spanned<Box<Schema>>, ShellError> {
            let span = value.span();
            Ok(Box::new(Schema::from_value(value.clone())?).into_spanned(span))
        }
        #[inline]
        fn to_list(value: &Value) -> Result<Spanned<Box<[Schema]>>, ShellError> {
            let span = value.span();
            Ok(value
                .as_list()?
                .iter()
                .cloned()
                .map(Schema::from_value)
                .collect::<Result<Box<[_]>, _>>()?
                .into_spanned(span))
        }
        #[inline]
        fn to_range(value: &Value) -> Result<Spanned<IntRange>, ShellError> {
            let span = value.span();
            let Range::IntRange(range) = value.as_range()? else {
                return Err(convert_error(
                    "range",
                    span,
                    "only integer ranges allowed for bounds",
                ));
            };
            Ok(range.into_spanned(span))
        }
        match v {
            Value::Custom { val, internal_span } => {
                if val.type_name() == Self::TYPE_NAME {
                    // SAFETY: val will always be Schema
                    Ok(val.as_any().downcast_ref::<Schema>().unwrap().clone())
                } else {
                    Err(convert_error("custom", internal_span, "wrong custom type"))
                }
            }
            Value::Record { val, internal_span } => {
                // TODO: construct Schema without copying v
                if val.len() == 1 {
                    // SAFETY: get_index: val has exactly 1 field
                    let (key, value) = val.get_index(0).unwrap();
                    let span = value.span();
                    match key.deref() {
                        "type" => type_from_typename(value.as_str()?).map_or_else(
                            || {
                                Err(convert_error(
                                    "record",
                                    internal_span,
                                    "unknown type constraint",
                                ))
                            },
                            |r#type| Ok(Self::Type(r#type.into_spanned(span))),
                        ),
                        "value" => Ok(Self::Value(value.clone())),
                        "fallback" => Ok(Self::Fallback(value.clone())),
                        "any" => to_list(value).map(Self::Any),
                        "all" => to_list(value).map(Self::All),
                        "custom" => value
                            .as_closure()
                            .cloned()
                            .map(|closure| Self::Custom(closure.into_spanned(span))),
                        _ => Err(convert_error(
                            "record",
                            internal_span,
                            "unknown schema definition",
                        )),
                    }
                } else if let Some(list) = val.get("list") {
                    match list.as_str()? {
                        "tuple" => {
                            let items = val.get("items").map_or_else(
                                || {
                                    Err(convert_error(
                                        "record",
                                        internal_span,
                                        "tuple schema needs `items` field",
                                    ))
                                },
                                to_list,
                            )?;
                            let wrap_missing = to_bool(val.get("wrap_missing"), false)?;
                            let wrap_single = to_bool(val.get("wrap_single"), false)?;
                            let wrap_null = to_bool(val.get("wrap_null"), false)?;
                            Ok(Self::Tuple {
                                items,
                                wrap_missing,
                                wrap_single,
                                wrap_null,
                                span: internal_span,
                            })
                        }
                        "array" => {
                            let items = val.get("items").map_or_else(
                                || {
                                    Err(convert_error(
                                        "record",
                                        internal_span,
                                        "array schema needs `items` field",
                                    ))
                                },
                                to_single,
                            )?;
                            let length = val
                                .get("length")
                                .map(to_range)
                                .transpose()?
                                .unwrap_or_else(unbounded);
                            let wrap_single = to_bool(val.get("wrap_single"), false)?;
                            let wrap_null = to_bool(val.get("wrap_null"), false)?;
                            Ok(Self::Array {
                                items,
                                length,
                                wrap_single,
                                wrap_null,
                                span: internal_span,
                            })
                        }
                        _ => Err(convert_error("record", internal_span, "unknown list type")),
                    }
                } else if let Some(record) = val.get("record") {
                    match record.as_str()? {
                        "struct" => {
                            let Some(fields) = val.get("fields") else {
                                return Err(convert_error(
                                    "record",
                                    internal_span,
                                    "struct schema needs `fields` field",
                                ));
                            };
                            let span = fields.span();
                            let fields = fields
                                .as_record()?
                                .iter()
                                .map(|(key, value)| {
                                    Ok((key.clone(), Schema::from_value(value.clone())?))
                                })
                                .collect::<Result<Box<[_]>, ShellError>>()?
                                .into_spanned(span);
                            let wrap_missing = to_bool(val.get("wrap_missing"), false)?;
                            let wrap_list = to_bool(val.get("wrap_list"), false)?;
                            let wrap_single = to_bool(val.get("wrap_single"), false)?;
                            let wrap_null = to_bool(val.get("wrap_null"), false)?;
                            Ok(Self::Struct {
                                fields,
                                wrap_missing,
                                wrap_list,
                                wrap_single,
                                wrap_null,
                                span: internal_span,
                            })
                        }
                        "map" => {
                            let keys = val.get("keys").map(to_single).transpose()?;
                            let values = val.get("values").map(to_single).transpose()?;
                            let length = val
                                .get("length")
                                .map(to_range)
                                .transpose()?
                                .unwrap_or_else(unbounded);
                            let wrap_null = to_bool(val.get("wrap_null"), false)?;
                            Ok(Self::Map {
                                keys,
                                values,
                                length,
                                wrap_null,
                                span: internal_span,
                            })
                        }
                        _ => Err(convert_error(
                            "record",
                            internal_span,
                            "unknown record type",
                        )),
                    }
                } else {
                    Err(convert_error(
                        "record",
                        internal_span,
                        "unknown schema definition",
                    ))
                }
            }
            v => Err(convert_error(
                &v.get_type().to_string(),
                v.span(),
                "direct conversion only works with records (see also schema command)",
            )),
        }
    }
}
impl IntoValue for Schema {
    #[inline]
    fn into_value(self, span: Span) -> Value {
        Value::custom(Box::new(self), span)
    }
}
impl Schema {
    /// Name used in [`Type::Custom`].
    pub const TYPE_NAME: &'static str = "Schema";
    /// Return the corresponding [`Type::Custom`].
    pub fn r#type() -> Type {
        Type::Custom(Self::TYPE_NAME.into())
    }
    #[inline]
    pub fn apply_type(r#type: &Spanned<Type>, value: Value) -> Result<Value, SchemaError> {
        if value.get_type() == r#type.item {
            Ok(value)
        } else {
            Err(SchemaError::Value {
                schema_span: r#type.span,
                value_span: value.span(),
                msg: "type test failed".to_string(),
            })
        }
    }
    #[inline]
    pub fn apply_value(test_value: &Value, value: Value) -> Result<Value, SchemaError> {
        if value == *test_value {
            Ok(value)
        } else {
            Err(SchemaError::Value {
                schema_span: test_value.span(),
                value_span: value.span(),
                msg: "static value test failed".to_string(),
            })
        }
    }
    #[inline]
    pub fn apply_fallback(default: &Value, _value: Value) -> Result<Value, SchemaError> {
        Ok(default.clone())
    }
    #[inline]
    pub fn apply_any(
        engine: Option<&EngineInterface>,
        schemas: &Spanned<Box<[Schema]>>,
        value: Value,
    ) -> Result<Value, SchemaError> {
        let inner = schemas.as_ref().item;
        if inner.is_empty() {
            return Err(SchemaError::Value {
                schema_span: schemas.span,
                value_span: value.span(),
                msg: "empty any".to_string(),
            });
        }
        // SAFETY: length cannot be 0
        if let Some(value) = inner
            .iter()
            .take(inner.len() - 1)
            .map(|s| s.apply(engine, value.clone()))
            .find_map(|v| v.ok())
        {
            Ok(value)
        } else {
            inner.last().unwrap().apply(engine, value)
        }
    }
    #[inline]
    pub fn apply_all(
        engine: Option<&EngineInterface>,
        schemas: &Spanned<Box<[Schema]>>,
        value: Value,
    ) -> Result<Value, SchemaError> {
        schemas
            .as_ref()
            .item
            .iter()
            .try_fold(value, |v, s| s.apply(engine, v))
    }
    #[inline]
    pub fn apply_tuple(
        engine: Option<&EngineInterface>,
        items: &Spanned<Box<[Schema]>>,
        wrap_missing: &Spanned<bool>,
        wrap_single: &Spanned<bool>,
        wrap_null: &Spanned<bool>,
        span: Span,
        value: Value,
    ) -> Result<Value, SchemaError> {
        let inner = items.as_ref().item;
        match value {
            Value::List {
                vals,
                internal_span,
            } => {
                let diff = vals.len().checked_sub(inner.len());
                if diff != Some(0) && !(wrap_missing.item && diff.is_none()) {
                    return Err(SchemaError::Value {
                        schema_span: items.span,
                        value_span: internal_span,
                        msg: "tuple length mismatch".to_string(),
                    });
                }
                inner
                    .iter()
                    .zip(
                        vals.into_iter()
                            .chain(repeat(Value::nothing(Span::unknown()))),
                    )
                    .map(|(s, v)| s.apply(engine, v))
                    .collect::<Result<Vec<_>, _>>()
                    .map(|vals| Value::list(vals, internal_span))
            }
            Value::Nothing { internal_span } if inner.len() == 0 || wrap_missing.item => {
                if wrap_null.item {
                    if inner.is_empty() {
                        Ok(Value::list(Vec::new(), internal_span))
                    } else if wrap_missing.item {
                        inner
                            .iter()
                            .map(|s| s.apply(engine, Value::nothing(Span::unknown())))
                            .collect::<Result<Vec<_>, _>>()
                            .map(|vals| Value::list(vals, internal_span))
                    } else {
                        Err(SchemaError::Value {
                            schema_span: span_fallback(wrap_missing.span, span),
                            value_span: internal_span,
                            msg: "not enough elements".to_string(),
                        })
                    }
                } else {
                    Err(SchemaError::Value {
                        schema_span: span_fallback(wrap_null.span, span),
                        value_span: internal_span,
                        msg: "wrapping null not allowed".to_string(),
                    })
                }
            }
            value if inner.len() == 1 || wrap_missing.item => {
                if wrap_single.item {
                    if inner.len() == 1 {
                        inner[0].apply(engine, value).map(|value| {
                            let span = value.span();
                            Value::list(vec![value], span)
                        })
                    } else if wrap_missing.item && inner.len() != 0 {
                        let span = value.span();
                        inner
                            .iter()
                            .zip(once(value).chain(repeat(Value::nothing(Span::unknown()))))
                            .map(|(s, v)| s.apply(engine, v))
                            .collect::<Result<Vec<_>, _>>()
                            .map(|vals| Value::list(vals, span))
                    } else {
                        Err(SchemaError::Value {
                            schema_span: span_fallback(wrap_missing.span, span),
                            value_span: value.span(),
                            msg: "wrong number of elements".to_string(),
                        })
                    }
                } else {
                    Err(SchemaError::Value {
                        schema_span: span_fallback(wrap_single.span, span),
                        value_span: value.span(),
                        msg: "wrapping single value not allowed".to_string(),
                    })
                }
            }
            value => Err(SchemaError::Value {
                schema_span: span,
                value_span: value.span(),
                msg: "non-list value".to_string(),
            }),
        }
    }
    #[inline]
    pub fn apply_array(
        engine: Option<&EngineInterface>,
        items: &Spanned<Box<Schema>>,
        length: &Spanned<IntRange>,
        wrap_single: &Spanned<bool>,
        wrap_null: &Spanned<bool>,
        span: Span,
        value: Value,
    ) -> Result<Value, SchemaError> {
        let inner = items.as_ref().item;
        let len = length.as_ref().item;
        match value {
            Value::List {
                vals,
                internal_span,
            } => {
                if !len.contains(vals.len() as i64) {
                    return Err(SchemaError::Value {
                        schema_span: length.span,
                        value_span: internal_span,
                        msg: "array length mismatch".to_string(),
                    });
                }
                vals.into_iter()
                    .map(|v| inner.apply(engine, v))
                    .collect::<Result<Vec<Value>, _>>()
                    .map(|vals| Value::list(vals, internal_span))
            }
            Value::Nothing { internal_span } if len.contains(0) => {
                if wrap_null.item {
                    Ok(Value::list(Vec::new(), internal_span))
                } else {
                    Err(SchemaError::Value {
                        schema_span: span_fallback(wrap_null.span, span),
                        value_span: internal_span,
                        msg: "wrapping null not allowed".to_string(),
                    })
                }
            }
            value if len.contains(1) => {
                if wrap_single.item {
                    inner.apply(engine, value).map(|value| {
                        let span = value.span();
                        Value::list(vec![value], span)
                    })
                } else {
                    Err(SchemaError::Value {
                        schema_span: span_fallback(wrap_single.span, span),
                        value_span: value.span(),
                        msg: "wrapping single value not allowed".to_string(),
                    })
                }
            }
            value => Err(SchemaError::Value {
                schema_span: span,
                value_span: value.span(),
                msg: "non-list value".to_string(),
            }),
        }
    }
    #[allow(clippy::too_many_arguments)]
    #[inline]
    pub fn apply_struct(
        engine: Option<&EngineInterface>,
        fields: &Spanned<Box<[(String, Schema)]>>,
        wrap_missing: &Spanned<bool>,
        wrap_list: &Spanned<bool>,
        wrap_single: &Spanned<bool>,
        wrap_null: &Spanned<bool>,
        span: Span,
        value: Value,
    ) -> Result<Value, SchemaError> {
        let value_span = value.span();
        let inner = fields.as_ref().item;
        let record = match value {
            Value::Record { val, .. } => {
                if val.len() > inner.len() {
                    return Err(SchemaError::Value {
                        schema_span: fields.span,
                        value_span,
                        msg: "too many fields".to_string(),
                    });
                }
                let mut record = val.into_owned();
                for (field, schema) in inner.iter() {
                    // TODO: do this without having to clone all values
                    let value = if let Some(value) = record.get(field) {
                        value.clone()
                    } else if wrap_missing.item {
                        Value::nothing(wrap_missing.span)
                    } else {
                        return Err(SchemaError::Value {
                            schema_span: span_fallback(wrap_missing.span, span),
                            value_span,
                            msg: "missing fields not allowed".to_string(),
                        });
                    };
                    match schema.apply(engine, value) {
                        Ok(value) => {
                            record.insert(field, value);
                        }
                        Err(error) => return Err(error),
                    }
                }
                record
            }
            Value::List { vals, .. } if wrap_list.item => {
                if vals.len() > inner.len() {
                    return Err(SchemaError::Value {
                        schema_span: fields.span,
                        value_span,
                        msg: "too many fields".to_string(),
                    });
                }
                let mut record = Record::with_capacity(inner.len());
                for ((field, schema), value) in inner.iter().zip(
                    vals.into_iter()
                        .chain(repeat(Value::nothing(Span::unknown()))),
                ) {
                    match schema.apply(engine, value) {
                        Ok(value) => _ = record.insert(field, value),
                        Err(error) => return Err(error),
                    }
                }
                record
            }
            Value::Nothing { .. } if wrap_null.item => {
                let mut record = Record::with_capacity(inner.len());
                for (field, schema) in inner.iter() {
                    match schema.apply(engine, Value::nothing(Span::unknown())) {
                        Ok(value) => _ = record.insert(field, value),
                        Err(error) => return Err(error),
                    }
                }
                record
            }
            value if wrap_single.item => {
                if inner.len() == 0 {
                    return Err(SchemaError::Value {
                        schema_span: fields.span,
                        value_span,
                        msg: "too many fields".to_string(),
                    });
                }
                let mut record = Record::with_capacity(inner.len());
                for ((field, schema), value) in inner
                    .iter()
                    .zip(once(value).chain(repeat(Value::nothing(Span::unknown()))))
                {
                    match schema.apply(engine, value) {
                        Ok(value) => _ = record.insert(field, value),
                        Err(error) => return Err(error),
                    }
                }
                record
            }
            _ => {
                return Err(SchemaError::Value {
                    schema_span: span,
                    value_span,
                    msg: "non-record value".to_string(),
                })
            }
        };
        Ok(Value::record(record, value_span))
    }
    #[inline]
    pub fn apply_map(
        engine: Option<&EngineInterface>,
        keys: &Option<Spanned<Box<Schema>>>,
        values: &Option<Spanned<Box<Schema>>>,
        length: &Spanned<IntRange>,
        wrap_null: &Spanned<bool>,
        span: Span,
        value: Value,
    ) -> Result<Value, SchemaError> {
        let len = length.as_ref().item;
        let value_span = value.span();
        match value {
            Value::Record { val, internal_span } => {
                if !len.contains(val.len() as i64) {
                    return Err(SchemaError::Value {
                        schema_span: length.span,
                        value_span,
                        msg: "map length mismatch".to_string(),
                    });
                }
                if let Some(key_schema) = keys.as_ref() {
                    let inner_key = key_schema.as_ref().item;
                    let mut record = Record::new();
                    for (key, value) in val.into_owned().into_iter() {
                        let key = Value::string(key, Span::unknown());
                        let key = match inner_key.apply(engine, key) {
                            Ok(key) => match key {
                                Value::String { val, .. } => val,
                                _ => {
                                    return Err(SchemaError::Schema {
                                        span: key_schema.span,
                                        msg: "invalid key schema, has to return string".to_string(),
                                    })
                                }
                            },
                            Err(error) => return Err(error),
                        };
                        if let Some(schema) = values.as_ref() {
                            let inner = schema.as_ref().item;
                            match inner.apply(engine, value) {
                                Ok(value) => {
                                    record.insert(key, value);
                                }
                                Err(error) => return Err(error),
                            }
                        } else {
                            record.insert(key, value);
                        }
                    }
                    Ok(Value::record(record, internal_span))
                } else if let Some(value_schema) = values.as_ref() {
                    let inner = value_schema.as_ref().item;
                    let mut record = val.into_owned();
                    // TODO: instead of iterating (and cloning value) take/replace values instead
                    for (_, value) in record.iter_mut() {
                        match inner.apply(engine, value.clone()) {
                            Ok(result) => {
                                *value = result;
                            }
                            Err(error) => return Err(error),
                        }
                    }
                    Ok(Value::record(record, value_span))
                } else {
                    Ok(Value::Record { val, internal_span })
                }
            }
            Value::Nothing { internal_span } if len.contains(0) => {
                if wrap_null.item {
                    Ok(Value::record(Record::new(), internal_span))
                } else {
                    Err(SchemaError::Value {
                        schema_span: span_fallback(wrap_null.span, span),
                        value_span: internal_span,
                        msg: "wrapping null not allowed".to_string(),
                    })
                }
            }
            _ => Err(SchemaError::Value {
                schema_span: span,
                value_span,
                msg: "non-record value".to_string(),
            }),
        }
    }
    #[inline]
    pub fn apply_custom(
        engine: Option<&EngineInterface>,
        closure: &Spanned<Closure>,
        value: Value,
    ) -> Result<Value, SchemaError> {
        let value_span = value.span();
        let Some(engine) = engine else {
            return Err(SchemaError::Schema {
                span: closure.span,
                msg: "custom closures are not supported in this context".to_string(),
            });
        };
        let mut result = engine
            .eval_closure(closure, Vec::new(), Some(value))
            .map_err(|error| SchemaError::Custom {
                schema_span: closure.span,
                value_span,
                error,
            })?
            .into_record()
            .map_err(|_error| SchemaError::Schema {
                span: closure.span,
                msg: "custom closure has to return a record".to_string(),
            })?;
        if result.len() != 1 {
            return Err(SchemaError::Schema {
                span: closure.span,
                msg: "custom closure return record should only have a single field".to_string(),
            });
        }
        if let Some(value) = result.remove("ok") {
            Ok(value)
        } else if let Some(msg) = result.remove("err") {
            Err(SchemaError::Value {
                schema_span: closure.span,
                value_span,
                msg: msg
                    .coerce_into_string()
                    .map_err(|_error| SchemaError::Schema {
                        span: closure.span,
                        msg: "could not convert custom error message to string".to_string(),
                    })?,
            })
        } else {
            Err(SchemaError::Schema {
                span: closure.span,
                msg: "custom closure return record should either have a ok or err field"
                    .to_string(),
            })
        }
    }
    /// Apply the schema to a [`Value`].
    /// `engine` is required to test against [`Schema::Custom`], can be `None` when not needed.
    pub fn apply(
        &self,
        engine: Option<&EngineInterface>,
        value: Value,
    ) -> Result<Value, SchemaError> {
        match self {
            Self::Type(r#type) => Self::apply_type(r#type, value),
            Self::Value(test_value) => Self::apply_value(test_value, value),
            Self::Fallback(default) => Self::apply_fallback(default, value),
            Self::Any(schemas) => Self::apply_any(engine, schemas, value),
            Self::All(schemas) => Self::apply_all(engine, schemas, value),
            Self::Tuple {
                items,
                wrap_missing,
                wrap_single,
                wrap_null,
                span,
            } => Self::apply_tuple(
                engine,
                items,
                wrap_missing,
                wrap_single,
                wrap_null,
                *span,
                value,
            ),
            Self::Array {
                items,
                length,
                wrap_single,
                wrap_null,
                span,
            } => Self::apply_array(engine, items, length, wrap_single, wrap_null, *span, value),
            Self::Struct {
                fields,
                wrap_missing,
                wrap_list,
                wrap_single,
                wrap_null,
                span,
            } => Self::apply_struct(
                engine,
                fields,
                wrap_missing,
                wrap_list,
                wrap_single,
                wrap_null,
                *span,
                value,
            ),
            Self::Map {
                keys,
                values,
                length,
                wrap_null,
                span,
            } => Self::apply_map(engine, keys, values, length, wrap_null, *span, value),
            Self::Custom(closure) => Self::apply_custom(engine, closure, value),
        }
    }
    const VAR_TYPE: u8 = 0;
    const VAR_VALUE: u8 = 1;
    const VAR_FALLBACK: u8 = 2;
    const VAR_ANY: u8 = 3;
    const VAR_ALL: u8 = 4;
    const VAR_TUPLE: u8 = 5;
    const VAR_ARRAY: u8 = 6;
    const VAR_STRUCT: u8 = 7;
    const VAR_MAP: u8 = 8;
    const VAR_CUSTOM: u8 = 9;
}
macro_rules! serialize_seq {
    ($seq:ident, $value:expr) => {
        $seq.serialize_element($value)?;
    };
    ($seq:ident, $first:expr, $( $rest:expr ),+) => {
        serialize_seq!($seq, $first);
        serialize_seq!($seq, $( $rest ),*);
    }
}
impl Serialize for Schema {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut seq = serializer.serialize_seq(None)?;
        match self {
            Schema::Type(r#type) => {
                serialize_seq!(seq, &Self::VAR_TYPE, r#type);
            }
            Schema::Value(value) => {
                serialize_seq!(seq, &Self::VAR_VALUE, value);
            }
            Schema::Fallback(value) => {
                serialize_seq!(seq, &Self::VAR_FALLBACK, value);
            }
            Schema::Any(options) => {
                serialize_seq!(seq, &Self::VAR_ANY, options);
            }
            Schema::All(options) => {
                serialize_seq!(seq, &Self::VAR_ALL, options);
            }
            Schema::Tuple {
                items,
                wrap_missing,
                wrap_single,
                wrap_null,
                span,
            } => {
                serialize_seq!(
                    seq,
                    &Self::VAR_TUPLE,
                    items,
                    wrap_missing,
                    wrap_single,
                    wrap_null,
                    span
                );
            }
            Schema::Array {
                items,
                length,
                wrap_single,
                wrap_null,
                span,
            } => {
                serialize_seq!(
                    seq,
                    &Self::VAR_ARRAY,
                    items,
                    length,
                    wrap_single,
                    wrap_null,
                    span
                );
            }
            Schema::Struct {
                fields,
                wrap_missing,
                wrap_list,
                wrap_single,
                wrap_null,
                span,
            } => {
                serialize_seq!(
                    seq,
                    &Self::VAR_STRUCT,
                    fields,
                    wrap_missing,
                    wrap_list,
                    wrap_single,
                    wrap_null,
                    span
                );
            }
            Schema::Map {
                keys,
                values,
                length,
                wrap_null,
                span,
            } => {
                serialize_seq!(seq, &Self::VAR_MAP, keys, values, length, wrap_null, span);
            }
            Schema::Custom(closure) => {
                serialize_seq!(seq, &Self::VAR_CUSTOM, closure);
            }
        }
        seq.end()
    }
}
macro_rules! deserialize_seq {
    ($seq:ident, $error:ty, $name:ident) => {
        let Some($name) = $seq.next_element()? else {
            return Err(<$error>::custom("sequence not long enough"))
        };
    };
    ($seq:ident, $error:ty, $first:ident, $( $rest:ident ),+) => {
        deserialize_seq!($seq, $error, $first);
        deserialize_seq!($seq, $error, $( $rest ),*)
    }
}
struct SchemaVisiter;
impl<'de> Visitor<'de> for SchemaVisiter {
    type Value = Schema;
    #[inline]
    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("a Schema instance")
    }
    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        deserialize_seq!(seq, A::Error, variant);
        match variant {
            Schema::VAR_TYPE => {
                deserialize_seq!(seq, A::Error, r#type);
                Ok(Schema::Type(r#type))
            }
            Schema::VAR_VALUE => {
                deserialize_seq!(seq, A::Error, value);
                Ok(Schema::Value(value))
            }
            Schema::VAR_FALLBACK => {
                deserialize_seq!(seq, A::Error, value);
                Ok(Schema::Fallback(value))
            }
            Schema::VAR_ANY => {
                deserialize_seq!(seq, A::Error, options);
                Ok(Schema::Any(options))
            }
            Schema::VAR_ALL => {
                deserialize_seq!(seq, A::Error, options);
                Ok(Schema::All(options))
            }
            Schema::VAR_TUPLE => {
                deserialize_seq!(
                    seq,
                    A::Error,
                    items,
                    wrap_missing,
                    wrap_single,
                    wrap_null,
                    span
                );
                Ok(Schema::Tuple {
                    items,
                    wrap_missing,
                    wrap_single,
                    wrap_null,
                    span,
                })
            }
            Schema::VAR_ARRAY => {
                deserialize_seq!(seq, A::Error, items, length, wrap_single, wrap_null, span);
                Ok(Schema::Array {
                    items,
                    length,
                    wrap_single,
                    wrap_null,
                    span,
                })
            }
            Schema::VAR_STRUCT => {
                deserialize_seq!(
                    seq,
                    A::Error,
                    fields,
                    wrap_missing,
                    wrap_list,
                    wrap_single,
                    wrap_null,
                    span
                );
                Ok(Schema::Struct {
                    fields,
                    wrap_missing,
                    wrap_list,
                    wrap_single,
                    wrap_null,
                    span,
                })
            }
            Schema::VAR_MAP => {
                deserialize_seq!(seq, A::Error, keys, values, length, wrap_null, span);
                Ok(Schema::Map {
                    keys,
                    values,
                    length,
                    wrap_null,
                    span,
                })
            }
            Schema::VAR_CUSTOM => {
                deserialize_seq!(seq, A::Error, closure);
                Ok(Schema::Custom(closure))
            }
            _ => Err(A::Error::custom("unknown variant")),
        }
    }
}
impl<'de> Deserialize<'de> for Schema {
    #[inline(always)]
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_seq(SchemaVisiter)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn assert_schema(
        schema: Schema,
        r#in: Value,
        out: Option<Option<Value>>,
        msg: &'static str,
    ) -> Result<(), String> {
        let out = if let Some(out) = out {
            out
        } else {
            Some(r#in.clone())
        };
        let value = schema.apply(None, r#in);
        if let Some(out) = out {
            match value {
                Ok(value) if value == out => Ok(()),
                Ok(value) => {
                    println!("Wrong value returned");
                    println!("  Expected: {:?}", out);
                    println!("  Got: {:?}", value);
                    Err(format!("failed assertion: {}", msg))
                }
                Err(error) => {
                    println!("Unexpected Error: {}", error);
                    Err(format!("failed assertion: {}", msg))
                }
            }
        } else if let Ok(value) = value {
            println!("Expected Error");
            println!("Got Value: {:?}", value);
            Err(format!("failed assertion: {}", msg))
        } else {
            Ok(())
        }
    }

    fn make_range(start: i64, end: Option<i64>) -> IntRange {
        IntRange::new(
            Value::test_int(start),
            Value::test_int(start + 1),
            end.map_or(Value::test_nothing(), Value::test_int),
            nu_protocol::ast::RangeInclusion::Inclusive,
            Span::test_data(),
        )
        .unwrap()
    }

    fn schema_type(r#type: Type) -> Schema {
        Schema::Type(r#type.into_spanned(Span::test_data()))
    }
    fn schema_value(value: Value) -> Schema {
        Schema::Value(value)
    }
    fn schema_fallback(default: Value) -> Schema {
        Schema::Fallback(default)
    }
    fn schema_any(options: Vec<Schema>) -> Schema {
        Schema::Any(options.into_boxed_slice().into_spanned(Span::test_data()))
    }
    fn schema_all(options: Vec<Schema>) -> Schema {
        Schema::All(options.into_boxed_slice().into_spanned(Span::test_data()))
    }
    fn schema_tuple(
        elements: Vec<Schema>,
        wrap_missing: bool,
        wrap_single: bool,
        wrap_null: bool,
    ) -> Schema {
        Schema::Tuple {
            items: elements.into_boxed_slice().into_spanned(Span::test_data()),
            wrap_missing: wrap_missing.into_spanned(Span::test_data()),
            wrap_single: wrap_single.into_spanned(Span::test_data()),
            wrap_null: wrap_null.into_spanned(Span::test_data()),
            span: Span::test_data(),
        }
    }
    fn schema_array(
        elements: Schema,
        length: Option<IntRange>,
        wrap_single: bool,
        wrap_null: bool,
    ) -> Schema {
        Schema::Array {
            items: Box::new(elements).into_spanned(Span::test_data()),
            length: length
                .unwrap_or(make_range(0, None))
                .into_spanned(Span::test_data()),
            wrap_single: wrap_single.into_spanned(Span::test_data()),
            wrap_null: wrap_null.into_spanned(Span::test_data()),
            span: Span::test_data(),
        }
    }
    fn schema_struct(
        fields: Vec<(&'static str, Schema)>,
        wrap_missing: bool,
        wrap_list: bool,
        wrap_single: bool,
        wrap_null: bool,
    ) -> Schema {
        Schema::Struct {
            fields: fields
                .into_iter()
                .map(|(field, schema)| (field.to_string(), schema))
                .collect::<Box<[_]>>()
                .into_spanned(Span::test_data()),
            wrap_missing: wrap_missing.into_spanned(Span::test_data()),
            wrap_list: wrap_list.into_spanned(Span::test_data()),
            wrap_single: wrap_single.into_spanned(Span::test_data()),
            wrap_null: wrap_null.into_spanned(Span::test_data()),
            span: Span::test_data(),
        }
    }
    fn schema_map(
        keys: Option<Schema>,
        values: Option<Schema>,
        length: Option<IntRange>,
        wrap_null: bool,
    ) -> Schema {
        Schema::Map {
            keys: keys.map(|s| Box::new(s).into_spanned(Span::test_data())),
            values: values.map(|s| Box::new(s).into_spanned(Span::test_data())),
            length: length
                .unwrap_or(make_range(0, None))
                .into_spanned(Span::test_data()),
            wrap_null: wrap_null.into_spanned(Span::test_data()),
            span: Span::test_data(),
        }
    }

    #[test]
    fn test_type() -> Result<(), String> {
        assert_schema(
            schema_type(Type::Int),
            Value::test_int(42),
            None,
            "matching value",
        )?;
        assert_schema(
            schema_type(Type::String),
            Value::test_int(42),
            Some(None),
            "wrong value",
        )?;
        Ok(())
    }
    #[test]
    fn test_value() -> Result<(), String> {
        let value = Value::test_int(42);
        assert_schema(
            schema_value(value.clone()),
            value.clone(),
            None,
            "matching value",
        )?;
        assert_schema(
            schema_value(Value::test_int(43)),
            value.clone(),
            Some(None),
            "wrong value",
        )?;
        assert_schema(
            schema_value(Value::test_float(1.0)),
            value,
            Some(None),
            "wrong type",
        )?;
        Ok(())
    }
    #[test]
    fn test_fallback() -> Result<(), String> {
        let value = Value::test_int(42);
        assert_schema(
            schema_fallback(value.clone()),
            Value::test_nothing(),
            Some(Some(value)),
            "fallback",
        )
    }
    #[test]
    fn test_any() -> Result<(), String> {
        let schema_failable = schema_any(vec![schema_type(Type::Int), schema_type(Type::Float)]);
        let schema_fallback = schema_any(vec![
            schema_value(Value::test_int(0)),
            schema_fallback(Value::test_int(1)),
        ]);
        assert_schema(
            schema_failable.clone(),
            Value::test_int(0),
            None,
            "matching branch",
        )?;
        assert_schema(
            schema_failable,
            Value::test_nothing(),
            Some(None),
            "no matching branch",
        )?;
        assert_schema(
            schema_fallback.clone(),
            Value::test_int(0),
            None,
            "branch before fallback",
        )?;
        assert_schema(
            schema_fallback,
            Value::test_int(2),
            Some(Some(Value::test_int(1))),
            "fallback",
        )?;
        assert_schema(
            schema_any(vec![]),
            Value::test_nothing(),
            Some(None),
            "empty any",
        )?;
        Ok(())
    }
    #[test]
    fn test_all() -> Result<(), String> {
        let schema = schema_all(vec![
            schema_type(Type::Int),
            schema_value(Value::test_int(0)),
        ]);
        assert_schema(schema.clone(), Value::test_int(0), None, "matching value")?;
        assert_schema(
            schema.clone(),
            Value::test_int(1),
            Some(None),
            "partial match",
        )?;
        assert_schema(schema, Value::test_nothing(), Some(None), "no match")?;
        assert_schema(schema_all(vec![]), Value::test_nothing(), None, "empty all")?;
        Ok(())
    }
    #[test]
    fn test_tuple() -> Result<(), String> {
        let schema_basic = schema_tuple(
            vec![schema_type(Type::Int), schema_type(Type::Int)],
            false,
            false,
            false,
        );
        let schema_single = schema_tuple(vec![schema_value(Value::test_int(0))], false, true, true);
        let schema_empty = schema_tuple(vec![], false, true, true);
        let schema_optional = schema_tuple(
            vec![schema_any(vec![
                schema_type(Type::Int),
                schema_type(Type::Nothing),
            ])],
            true,
            false,
            true,
        );
        assert_schema(
            schema_basic.clone(),
            Value::test_list(vec![Value::test_int(0), Value::test_int(1)]),
            None,
            "matching value",
        )?;
        assert_schema(
            schema_basic.clone(),
            Value::test_list(vec![]),
            Some(None),
            "too short",
        )?;
        assert_schema(
            schema_basic,
            Value::test_list(vec![Value::test_nothing(), Value::test_nothing()]),
            Some(None),
            "wrong types",
        )?;
        assert_schema(
            schema_single.clone(),
            Value::test_int(0),
            Some(Some(Value::test_list(vec![Value::test_int(0)]))),
            "wrap single",
        )?;
        assert_schema(
            schema_single,
            Value::test_nothing(),
            Some(None),
            "null as single",
        )?;
        assert_schema(
            schema_empty.clone(),
            Value::test_int(0),
            Some(None),
            "too long",
        )?;
        assert_schema(
            schema_empty,
            Value::test_nothing(),
            Some(Some(Value::test_list(vec![]))),
            "wrap null",
        )?;
        assert_schema(
            schema_optional,
            Value::test_nothing(),
            Some(Some(Value::test_list(vec![Value::test_nothing()]))),
            "fill missing",
        )?;
        Ok(())
    }
    #[test]
    fn test_array() -> Result<(), String> {
        let schema_basic = schema_array(schema_type(Type::Int), None, true, true);
        let schema_fixed = schema_array(
            schema_type(Type::Int),
            Some(make_range(2, Some(2))),
            true,
            true,
        );
        let list_ = Value::test_list(vec![]);
        let list_0 = Value::test_list(vec![Value::test_int(0)]);
        let list_01 = Value::test_list(vec![Value::test_int(0), Value::test_int(1)]);
        assert_schema(
            schema_basic.clone(),
            list_01.clone(),
            None,
            "matching value",
        )?;
        assert_schema(
            schema_basic.clone(),
            Value::test_int(0),
            Some(Some(list_0.clone())),
            "wrap single",
        )?;
        assert_schema(
            schema_basic.clone(),
            Value::test_nothing(),
            Some(Some(list_.clone())),
            "wrap null",
        )?;
        assert_schema(
            schema_basic,
            Value::test_float(1.0),
            Some(None),
            "wrong type",
        )?;
        assert_schema(schema_fixed.clone(), list_0, Some(None), "too short")?;
        assert_schema(schema_fixed, list_01, None, "matching length")?;
        Ok(())
    }
    #[test]
    fn test_struct() -> Result<(), String> {
        let schema_strict = schema_struct(
            vec![("x", schema_type(Type::Int)), ("y", schema_type(Type::Int))],
            false,
            false,
            false,
            false,
        );
        let schema_open = schema_struct(
            vec![
                ("x", schema_type(Type::Int)),
                (
                    "y",
                    schema_any(vec![
                        schema_all(vec![
                            schema_type(Type::Nothing),
                            schema_fallback(Value::test_int(1)),
                        ]),
                        schema_type(Type::Int),
                    ]),
                ),
                (
                    "z",
                    schema_any(vec![
                        schema_all(vec![
                            schema_type(Type::Nothing),
                            schema_fallback(Value::test_int(0)),
                        ]),
                        schema_type(Type::Int),
                    ]),
                ),
            ],
            true,
            true,
            true,
            false,
        );
        let schema_empty = schema_struct(vec![], false, false, false, true);
        let xy = Value::test_record(record!("x" => Value::test_int(0), "y" => Value::test_int(1)));
        let xy_list = Value::test_list(vec![Value::test_int(0), Value::test_int(1)]);
        let x_null = Value::test_record(record!("x" => Value::test_nothing()));
        let xyz = Value::test_record(
            record!("x" => Value::test_int(0), "y" => Value::test_int(1), "z" => Value::test_int(0)),
        );
        let xyz_null = Value::test_record(
            record!("x" => Value::test_int(0), "y" => Value::test_int(1), "z" => Value::test_nothing()),
        );
        let empty = Value::test_record(record!());
        assert_schema(schema_strict.clone(), xy.clone(), None, "maching value")?;
        assert_schema(
            schema_strict.clone(),
            x_null.clone(),
            Some(None),
            "wrong type",
        )?;
        assert_schema(schema_strict, xyz.clone(), Some(None), "too many fields")?;
        assert_schema(
            schema_open.clone(),
            xy,
            Some(Some(xyz.clone())),
            "default field",
        )?;
        assert_schema(
            schema_open.clone(),
            xyz_null,
            Some(Some(xyz.clone())),
            "treat null like missing",
        )?;
        assert_schema(
            schema_open.clone(),
            x_null,
            Some(None),
            "missing required field",
        )?;
        assert_schema(
            schema_open.clone(),
            xy_list,
            Some(Some(xyz.clone())),
            "treat list as ordered fields",
        )?;
        assert_schema(
            schema_open,
            Value::test_int(0),
            Some(Some(xyz)),
            "treat single value as first field",
        )?;
        assert_schema(
            schema_empty,
            Value::test_nothing(),
            Some(Some(empty)),
            "treat null as empty record",
        )?;
        Ok(())
    }
    #[test]
    fn test_map() -> Result<(), String> {
        let schema_values = schema_map(None, Some(schema_type(Type::Int)), None, true);
        let schema_keys = schema_map(
            Some(schema_value(Value::test_string("x"))),
            None,
            None,
            false,
        );
        let schema_fixed = schema_map(
            Some(schema_any(vec![
                schema_value(Value::test_string("x")),
                schema_value(Value::test_string("y")),
            ])),
            Some(schema_type(Type::Int)),
            Some(make_range(2, Some(2))),
            false,
        );
        let map_ = Value::test_record(record!());
        let map_x = Value::test_record(record!("x" => Value::test_int(0)));
        let map_xy =
            Value::test_record(record!("x" => Value::test_int(0), "y" => Value::test_int(1)));
        let map_xy_null =
            Value::test_record(record!("x" => Value::test_int(0), "y" => Value::test_nothing()));
        assert_schema(schema_values.clone(), map_.clone(), None, "empty values")?;
        assert_schema(
            schema_values.clone(),
            map_xy.clone(),
            None,
            "matching values",
        )?;
        assert_schema(
            schema_values.clone(),
            map_xy_null.clone(),
            Some(None),
            "wrong value",
        )?;
        assert_schema(
            schema_values,
            Value::test_nothing(),
            Some(Some(map_.clone())),
            "wrap null",
        )?;
        assert_schema(schema_keys.clone(), map_.clone(), None, "empty keys")?;
        assert_schema(schema_keys.clone(), map_x.clone(), None, "matching keys")?;
        assert_schema(schema_keys.clone(), map_xy.clone(), Some(None), "wrong key")?;
        assert_schema(
            schema_keys,
            Value::test_nothing(),
            Some(None),
            "disallow wrap null",
        )?;
        assert_schema(schema_fixed.clone(), map_x.clone(), Some(None), "too short")?;
        assert_schema(
            schema_fixed.clone(),
            map_xy.clone(),
            None,
            "matching length",
        )?;
        assert_schema(schema_fixed, map_xy_null.clone(), Some(None), "too long")?;
        Ok(())
    }

    fn assert_conversion_roundtrip(schema: Schema, msg: &str) -> Result<(), String> {
        let value = schema
            .to_base_value(Span::test_data())
            .map_err(|error| format!("failed conversion to value: {}", error))?;
        let result = Schema::from_value(value)
            .map_err(|error| format!("failed conversion from value {}", error))?;
        if schema != result {
            println!("conversion mismatch");
            println!("  Expected: {:?}", schema);
            println!("  Got: {:?}", result);
            return Err(format!("conversion failed: {}", msg));
        }
        Ok(())
    }

    #[test]
    fn test_convert_type() -> Result<(), String> {
        assert_conversion_roundtrip(schema_type(Type::Int), "type")?;
        assert_conversion_roundtrip(schema_value(Value::test_int(42)), "value")?;
        assert_conversion_roundtrip(schema_fallback(Value::test_nothing()), "fallback")?;
        assert_conversion_roundtrip(schema_any(vec![]), "any")?;
        assert_conversion_roundtrip(schema_all(vec![]), "all")?;
        assert_conversion_roundtrip(schema_tuple(vec![], true, true, true), "tuple")?;
        assert_conversion_roundtrip(
            schema_array(
                schema_type(Type::Int),
                Some(make_range(1, Some(10))),
                true,
                true,
            ),
            "array",
        )?;
        assert_conversion_roundtrip(schema_struct(vec![], true, false, false, false), "struct")?;
        assert_conversion_roundtrip(
            schema_map(
                Some(schema_all(vec![])),
                Some(schema_type(Type::Int)),
                None,
                false,
            ),
            "map",
        )?;
        Ok(())
    }
}
