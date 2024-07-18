use nu_plugin::SimplePluginCommand;
use nu_protocol::{record, Example, IntoValue, LabeledError, Signature, Type, Value};

use crate::{SchemaError, SchemaPlugin, ValueCmd};

pub struct NormalizeCmd;
impl SimplePluginCommand for NormalizeCmd {
    type Plugin = SchemaPlugin;
    #[inline(always)]
    fn name(&self) -> &str {
        "normalize"
    }
    #[inline(always)]
    fn usage(&self) -> &str {
        "trys to normalize input using a schema or fail"
    }
    #[inline]
    fn signature(&self) -> Signature {
        Signature::build(self.name())
            .input_output_type(Type::Any, Type::Any)
            .required(
                "schema",
                Type::Any.to_shape(),
                "target schema to apply to input (see schema)",
            )
            .switch(
                "result",
                "instead of causing an error, always return a record with 'ok' or 'err' field",
                Some('r'),
            )
    }
    #[inline]
    fn examples(&self) -> Vec<Example> {
        vec![
            Example {
                example: "42 | normalize int",
                description: "validate using inline schema",
                result: Some(Value::test_int(42)),
            },
            Example {
                example: "'42' | normalize int --result | 'ok' in $in",
                description: "catch error",
                result: Some(Value::test_bool(false)),
            },
            Example {
                example: "let schema = [[nothing {fallback: 0}] int] | schema array -sn\n[1 null 3] | normalize $schema",
                description: "use previously defined schema",
                result: None,/* Some(Value::test_list(vec![
                    Value::test_int(1),
                    Value::test_int(0),
                    Value::test_int(3),
                ])), */
            },
        ]
    }
    #[inline]
    fn run(
        &self,
        _plugin: &Self::Plugin,
        engine: &nu_plugin::EngineInterface,
        call: &nu_plugin::EvaluatedCall,
        input: &Value,
    ) -> Result<Value, LabeledError> {
        let schema = ValueCmd::run_direct(&call.req(0)?)?;
        let result = schema.apply(Some(engine), input.clone());
        if call.has_flag("result")? {
            let record = result.map_or_else(
                |error| record!("err" => error.into_value(input.span())),
                |value| record!("ok" => value),
            );
            Ok(Value::record(record, input.span()))
        } else {
            result.map_err(|error| match error {
                SchemaError::Value {
                    schema_span,
                    value_span,
                    msg,
                } => LabeledError::new("schema failed")
                    .with_help("value does not fullfill schema constraints")
                    .with_label("schema", schema_span)
                    .with_label(msg, value_span),
                SchemaError::Schema { span, msg } => {
                    LabeledError::new("malformatted schema").with_label(msg, span)
                }
                SchemaError::Custom {
                    schema_span,
                    value_span,
                    error,
                } => LabeledError::new("error in closure")
                    .with_help("error during custom schema evaluation")
                    .with_label("custom schema", schema_span)
                    .with_label("value", value_span)
                    .with_inner(error),
                SchemaError::Shell(error) => LabeledError::new("internal error").with_inner(error),
            })
        }
    }
}

#[cfg(test)]
#[test]
fn test_examples() -> Result<(), nu_protocol::ShellError> {
    nu_plugin_test_support::PluginTest::new("schema", SchemaPlugin.into())?
        .test_command_examples(&NormalizeCmd)
}
