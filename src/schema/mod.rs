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
    engine::Closure, record, CustomValue, IntRange, IntoSpanned, IntoValue, Range, Record,
    ShellError, Span, Spanned, Type, Value,
};
use serde::{Deserialize, Serialize};
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

// TODO: carry span from original user input around
/// Representation of a schema that can be applied to a [`Value`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Schema {
    /// Test against type
    Type(Spanned<Type>),
    /// Test against value
    Value(Value),
    /// Ignore input and return given value
    Fallback(Value),
    /// Test against multiple schema, will accept first passing schema
    Any(Spanned<Box<[Schema]>>),
    /// Test against multiple schema, will accept last schema when all are passing
    All(Spanned<Box<[Schema]>>),
    /// Descibes a heterogeneous list
    Tuple {
        items: Spanned<Box<[Schema]>>,
        /// Allow wrapping single non-list, non-null value into 1-length list
        wrap_single: Spanned<bool>,
        /// Allow treating `null` as empty list
        wrap_null: Spanned<bool>,
        span: Span,
    },
    /// Describes a homogeneous list
    Array {
        /// Inner schema, multiple will get repeated
        items: Spanned<Box<[Schema]>>,
        /// Length restriction
        length: Spanned<IntRange>,
        /// Allow wrapping single non-list, non-null value into 1-length list
        wrap_single: Spanned<bool>,
        /// Allow treating `null` as empty list
        wrap_null: Spanned<bool>,
        span: Span,
    },
    /// Describes a heterogeneous record
    Struct {
        /// Inner schema per field
        fields: Spanned<Box<[(String, Schema)]>>,
        /// Missing fields will be treated as `null`
        wrap_missing: Spanned<bool>,
        span: Span,
    },
    /// Describes a homegeneous record
    Map {
        /// Inner schema for keys
        keys: Option<Spanned<Box<Schema>>>,
        /// Inner schema for values
        values: Option<Spanned<Box<Schema>>>,
        /// Length restriction
        length: Spanned<IntRange>,
        /// Allow treating `null` as empty record
        wrap_null: Spanned<bool>,
        span: Span,
    },
    /// Run custom code (`value | closure [value] -> result`)
    /// result is encoded into nu using `{ok: value}` and `{err: error}`
    Custom(Spanned<Closure>),
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
    fn type_name(&self) -> String {
        "Schema".to_string()
    }
    fn clone_value(&self, span: Span) -> Value {
        Value::custom(Box::new(self.clone()), span)
    }
    fn to_base_value(&self, span: Span) -> Result<Value, ShellError> {
        let result =
            match self {
                Schema::Type(r#type) => {
                    record!("type" => Value::string(r#type.item.to_string(), r#type.span))
                        .into_spanned(span_fallback(span, r#type.span))
                }
                Schema::Value(value) => record!("value" => value.clone())
                    .into_spanned(span_fallback(span, value.span())),
                Schema::Fallback(value) => record!("fallback" => value.clone())
                    .into_spanned(span_fallback(span, value.span())),
                Schema::Any(schemas) => record!(
                    "any" => schemas.as_ref().item.iter()
                        .map(|s| s.to_base_value(Span::unknown()))
                        .collect::<Result<Vec<Value>, _>>()?
                        .into_value(schemas.span)
                )
                .into_spanned(span_fallback(span, schemas.span)),
                Schema::All(schemas) => record!(
                    "all" => schemas.as_ref().item.iter()
                        .map(|s| s.to_base_value(Span::unknown()))
                        .collect::<Result<Vec<Value>, _>>()?
                        .into_value(schemas.span)
                )
                .into_spanned(span_fallback(span, schemas.span)),
                Schema::Tuple {
                    items,
                    wrap_single,
                    wrap_null,
                    span: s,
                } => record!(
                    "list" => "tuple".to_string().into_value(Span::unknown()),
                    "items" => items.as_ref().item.iter()
                        .map(|s| s.to_base_value(Span::unknown()))
                        .collect::<Result<Vec<Value>, _>>()?
                        .into_value(items.span),
                    "wrap_single" => Value::bool(wrap_single.item, wrap_single.span),
                    "wrap_null" => Value::bool(wrap_null.item, wrap_null.span),
                )
                .into_spanned(span_fallback(span, *s)),
                Schema::Array {
                    items,
                    length,
                    wrap_single,
                    wrap_null,
                    span: s,
                } => record!(
                    "list" => Value::string("array".to_string(), Span::unknown()),
                    "items" => items.as_ref().item.iter()
                        .map(|s| s.to_base_value(Span::unknown()))
                        .collect::<Result<Vec<Value>, _>>()?
                        .into_value(items.span),
                    "length" => Value::range(Range::IntRange(length.item.clone()), length.span),
                    "wrap_single" => Value::bool(wrap_single.item, wrap_single.span),
                    "wrap_null" => Value::bool(wrap_null.item, wrap_null.span),
                )
                .into_spanned(span_fallback(span, *s)),
                Schema::Struct {
                    fields,
                    wrap_missing,
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
                    )
                    .into_spanned(span_fallback(span, *s))
                }
                Schema::Map {
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
                    "length" => Value::range(Range::IntRange(length.item.clone()), length.span),
                    "wrap_null" => Value::bool(wrap_null.item, wrap_null.span),
                )
                .into_spanned(span_fallback(span, *s)),
                Schema::Custom(closure) => record!(
                    "custom" => Value::closure(closure.item.clone(), closure.span),
                )
                .into_spanned(span_fallback(span, closure.span)),
            };
        Ok(Value::record(result.item, result.span))
    }
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    fn as_mut_any(&mut self) -> &mut dyn std::any::Any {
        self
    }
}
impl Schema {
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
            return Err(SchemaError::Schema {
                span: schemas.span,
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
        wrap_single: &Spanned<bool>,
        wrap_null: &Spanned<bool>,
        span: Span,
        value: Value,
    ) -> Result<Value, SchemaError> {
        let inner = items.as_ref().item;
        // TODO: find a way to not have to clone value
        let cloned = value.clone();
        match value {
            Value::List {
                vals,
                internal_span,
            } => {
                if vals.len() != inner.len() {
                    return Err(SchemaError::Value {
                        schema_span: items.span,
                        value_span: internal_span,
                        msg: "tuple length mismatch".to_string(),
                    });
                }
                if let Some(error) = inner
                    .iter()
                    .zip(vals)
                    .find_map(|(s, v)| s.apply(engine, v).err())
                {
                    Err(error)
                } else {
                    Ok(cloned)
                }
            }
            Value::Nothing { internal_span } if inner.len() == 0 => {
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
            value if inner.len() == 1 => {
                if wrap_single.item {
                    inner[0].apply(engine, value)
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
        items: &Spanned<Box<[Schema]>>,
        length: &Spanned<IntRange>,
        wrap_single: &Spanned<bool>,
        wrap_null: &Spanned<bool>,
        span: Span,
        value: Value,
    ) -> Result<Value, SchemaError> {
        let inner = items.as_ref().item;
        if inner.is_empty() {
            return Err(SchemaError::Schema {
                span: span_fallback(items.span, span),
                msg: "array needs at least one schema".to_string(),
            });
        }
        let len = length.as_ref().item;
        // TODO: find a way to not have to clone value
        let cloned = value.clone();
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
                if let Some(error) = inner
                    .iter()
                    .cycle()
                    .zip(vals)
                    .find_map(|(s, v)| s.apply(engine, v).err())
                {
                    Err(error)
                } else {
                    Ok(cloned)
                }
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
                    // SAFETY: unwrap: items can't be empty
                    inner[0].apply(engine, value)
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
    pub fn apply_struct(
        engine: Option<&EngineInterface>,
        fields: &Spanned<Box<[(String, Schema)]>>,
        wrap_missing: &Spanned<bool>,
        span: Span,
        value: Value,
    ) -> Result<Value, SchemaError> {
        let value_span = value.span();
        let Ok(mut record) = value.into_record() else {
            return Err(SchemaError::Value {
                schema_span: span,
                value_span,
                msg: "non-record value".to_string(),
            });
        };
        for (field, schema) in fields.as_ref().item.iter() {
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
            // TODO: forward internal error
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
    pub fn apply(
        &self,
        engine: Option<&EngineInterface>,
        value: Value,
    ) -> Result<Value, SchemaError> {
        match self {
            Schema::Type(r#type) => Self::apply_type(r#type, value),
            Schema::Value(test_value) => Self::apply_value(test_value, value),
            Schema::Fallback(default) => Self::apply_fallback(default, value),
            Schema::Any(schemas) => Self::apply_any(engine, schemas, value),
            Schema::All(schemas) => Self::apply_all(engine, schemas, value),
            Schema::Tuple {
                items,
                wrap_single,
                wrap_null,
                span,
            } => Self::apply_tuple(engine, items, wrap_single, wrap_null, *span, value),
            Schema::Array {
                items,
                length,
                wrap_single,
                wrap_null,
                span,
            } => Self::apply_array(engine, items, length, wrap_single, wrap_null, *span, value),
            Schema::Struct {
                fields,
                wrap_missing,
                span,
            } => Self::apply_struct(engine, fields, wrap_missing, *span, value),
            Schema::Map {
                keys,
                values,
                length,
                wrap_null,
                span,
            } => Self::apply_map(engine, keys, values, length, wrap_null, *span, value),
            Schema::Custom(closure) => Self::apply_custom(engine, closure, value),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn assert_schema(schema: Schema, r#in: Value, out: Option<Option<Value>>) {
        let out = if let Some(out) = out {
            out
        } else {
            Some(r#in.clone())
        };
        assert_eq!(schema.apply(None, r#in).ok(), out)
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

    #[test]
    fn test_type() {
        assert_schema(schema_type(Type::Int), Value::test_int(42), None);
        assert_schema(schema_type(Type::String), Value::test_int(42), Some(None));
    }
    #[test]
    fn test_value() {
        let value = Value::test_int(42);
        assert_schema(schema_value(value.clone()), value.clone(), None);
        assert_schema(schema_value(Value::test_int(43)), value.clone(), Some(None));
        assert_schema(schema_value(Value::test_float(1.0)), value, Some(None));
    }
    #[test]
    fn test_fallback() {
        let value = Value::test_int(42);
        assert_schema(
            schema_fallback(value.clone()),
            Value::test_nothing(),
            Some(Some(value)),
        );
    }

    // TODO: test compund schema
}
