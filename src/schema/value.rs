use std::ops::Deref;

use nu_plugin::SimplePluginCommand;
use nu_protocol::{
    Example, FromValue, IntoSpanned, IntoValue, LabeledError, Signature, Type, Value,
};

use crate::{schema::Schema, SchemaPlugin};

use super::type_from_typename;

pub struct ValueCmd;
impl ValueCmd {
    #[inline]
    pub fn run_direct(input: &Value) -> Result<Schema, LabeledError> {
        match input {
            Value::Custom { .. } | Value::Record { .. } => Ok(Schema::from_value(input.clone())?),
            Value::String { val, internal_span } => {
                let Some(r#type) = type_from_typename(val) else {
                    return Err(LabeledError::new("unsupported type constraint")
                        .with_label("type", *internal_span));
                };
                Ok(Schema::Type(r#type.into_spanned(*internal_span)))
            }
            Value::List {
                vals,
                internal_span,
            } => {
                let options =
                    vals.iter()
                        .map(|val| {
                            if let Value::List {
                                vals: vals_,
                                internal_span: span_,
                            } = val
                            {
                                let options_ = vals_
                                    .iter()
                                    .map(Self::run_direct)
                                    .collect::<Result<Box<[Schema]>, _>>()?;
                                Ok(Schema::All(options_.into_spanned(*span_)))
                            } else {
                                Self::run_direct(val)
                            }
                        })
                        .collect::<Result<Box<[_]>, _>>()?;
                Ok(Schema::Any(options.into_spanned(*internal_span)))
            }
            Value::Closure { val, internal_span } => Ok(Schema::Custom(
                val.deref().clone().into_spanned(*internal_span),
            )),
            value => Err(LabeledError::new("invalid input type").with_label("input", value.span())),
        }
    }
}
impl SimplePluginCommand for ValueCmd {
    type Plugin = SchemaPlugin;
    #[inline(always)]
    fn name(&self) -> &str {
        "schema"
    }
    #[inline(always)]
    fn usage(&self) -> &str {
        "create a schema for a single value"
    }
    #[inline]
    fn signature(&self) -> Signature {
        let out = Schema::r#type();
        Signature::build(self.name()).input_output_types(vec![
            (Type::Record(vec![].into_boxed_slice()), out.clone()),
            (Type::String, out.clone()),
            (Type::List(Type::Any.into()), out.clone()),
            (Type::Closure, out),
        ])
    }
    // TODO: add explicit results to examples
    #[inline]
    fn examples(&self) -> Vec<Example> {
        vec![
            Example {
                example: "0 | wrap value | schema",
                description: "create schema from explicit record",
                result: None
                },
            Example {
                example: "'int' | schema",
                description: "create schema from type shorthand",
                result: None,
            },
            Example {
                example: "[[nothing {fallback: 0}] int] | schema",
                description: "create schema from any/all compound",
                result: None,
            },
            Example {
                example: "{ if 0 <= $in and $in <= 100 { $in | wrap ok } else { 'out of bounds' | wrap err } } | schema",
                description: "create custom schema using a closure",
                result: None
            }
        ]
    }
    #[inline]
    fn run(
        &self,
        _plugin: &Self::Plugin,
        _engine: &nu_plugin::EngineInterface,
        _call: &nu_plugin::EvaluatedCall,
        input: &Value,
    ) -> Result<Value, LabeledError> {
        Ok(Self::run_direct(input)?.into_value(input.span()))
    }
}

#[cfg(test)]
#[test]
fn test_value_examples() -> Result<(), nu_protocol::ShellError> {
    nu_plugin_test_support::PluginTest::new("schema", SchemaPlugin.into())?
        .test_command_examples(&ValueCmd)
}
