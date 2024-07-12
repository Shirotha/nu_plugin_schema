use nu_plugin::SimplePluginCommand;
use nu_protocol::{
    Example, IntoSpanned, LabeledError, Signature, Span, Spanned, SyntaxShape, Type, Value,
};

use crate::{Schema, SchemaPlugin, ValueCmd};

pub struct TupleCmd;
impl TupleCmd {
    pub fn run_direct(
        input: &Value,
        wrap_single: Option<Spanned<bool>>,
        wrap_null: Option<Spanned<bool>>,
    ) -> Result<Schema, LabeledError> {
        let items = match input {
            Value::List { vals, .. } => vals
                .iter()
                .map(ValueCmd::run_direct)
                .collect::<Result<Box<[_]>, _>>()?,
            value => vec![ValueCmd::run_direct(value)?].into_boxed_slice(),
        };
        let default = false.into_spanned(Span::unknown());
        Ok(Schema::Tuple {
            items: items.into_spanned(input.span()),
            wrap_single: wrap_single.unwrap_or(default),
            wrap_null: wrap_null.unwrap_or(default),
            span: input.span(),
        })
    }
}
impl SimplePluginCommand for TupleCmd {
    type Plugin = SchemaPlugin;
    fn name(&self) -> &str {
        "schema tuple"
    }
    fn usage(&self) -> &str {
        "create a schema for a tuple (list)"
    }
    fn signature(&self) -> Signature {
        let out = Type::Custom("Schema".into());
        Signature::build("schema tuple")
            .input_output_types(vec![
                (Type::List(Type::Any.into()), out.clone()),
                (Type::Any, out),
            ])
            .named(
                "wrap-single",
                SyntaxShape::Boolean,
                "treat non-list, non-null values as 1-tuples",
                Some('s'),
            )
            .named(
                "wrap-null",
                SyntaxShape::Boolean,
                "treat null as 0-tuple",
                Some('n'),
            )
    }
    // TODO: add explicit results to examples
    fn examples(&self) -> Vec<Example> {
        vec![
            Example {
                example: "[int int] | schema tuple",
                description: "create explicit tuple schema",
                result: None,
            },
            Example {
                example: "[[nothing {fallback: 0}] int] | schema | schema tuple --wrap-single",
                description: "create 1-tuple schema that wraps single values",
                result: None,
            },
            Example {
                example: "[] | schema tuple --wrap-null",
                description: "create 0-tuple schema that wraps null",
                result: None,
            },
        ]
    }
    fn run(
        &self,
        _plugin: &Self::Plugin,
        _engine: &nu_plugin::EngineInterface,
        call: &nu_plugin::EvaluatedCall,
        input: &Value,
    ) -> Result<Value, LabeledError> {
        Ok(Self::run_direct(
            input,
            call.get_flag("wrap-single")?,
            call.get_flag("wrap-null")?,
        )?
        .into_value(input.span()))
    }
}

#[cfg(test)]
#[test]
fn test_tuple_examples() -> Result<(), nu_protocol::ShellError> {
    nu_plugin_test_support::PluginTest::new("schema", SchemaPlugin.into())?
        .test_command_examples(&TupleCmd)
}
