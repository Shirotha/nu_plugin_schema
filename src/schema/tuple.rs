use nu_plugin::SimplePluginCommand;
use nu_protocol::{
    Example, IntoSpanned, IntoValue, LabeledError, Signature, Span, Spanned, Type, Value,
};

use crate::{get_switch_spanned, Schema, SchemaPlugin, ValueCmd};

// Plugin command to create a [`Schema`] for a tuple.
pub struct TupleCmd;
impl TupleCmd {
    #[inline]
    pub fn run_direct(
        input: &Value,
        wrap_single: Spanned<bool>,
        wrap_null: Spanned<bool>,
    ) -> Result<Schema, LabeledError> {
        let items = match input {
            Value::List { vals, .. } => vals
                .iter()
                .map(ValueCmd::run_direct)
                .collect::<Result<Box<[_]>, _>>()?,
            value => vec![ValueCmd::run_direct(value)?].into_boxed_slice(),
        };
        Ok(Schema::Tuple {
            items: items.into_spanned(input.span()),
            wrap_single,
            wrap_null,
            span: input.span(),
        })
    }
}
impl SimplePluginCommand for TupleCmd {
    type Plugin = SchemaPlugin;
    #[inline(always)]
    fn name(&self) -> &str {
        "schema tuple"
    }
    #[inline(always)]
    fn usage(&self) -> &str {
        "create a schema for a tuple (list)"
    }
    #[inline]
    fn signature(&self) -> Signature {
        let out = Schema::r#type();
        Signature::build("schema tuple")
            .input_output_types(vec![
                (Type::List(Type::Any.into()), out.clone()),
                (Type::Any, out),
            ])
            .switch(
                "wrap-single",
                "treat non-list, non-null values as 1-tuples",
                Some('s'),
            )
            .switch("wrap-null", "treat null as 0-tuple", Some('n'))
    }
    #[inline]
    fn examples(&self) -> Vec<Example> {
        vec![
            Example {
                example: "[int int] | schema tuple",
                description: "create explicit tuple schema",
                result: Some(
                    Schema::Tuple {
                        items: vec![
                            Schema::Type(Type::Int.into_spanned(Span::test_data())),
                            Schema::Type(Type::Int.into_spanned(Span::test_data())),
                        ]
                        .into_boxed_slice()
                        .into_spanned(Span::test_data()),
                        wrap_single: false.into_spanned(Span::test_data()),
                        wrap_null: false.into_spanned(Span::test_data()),
                        span: Span::test_data(),
                    }
                    .into_value(Span::test_data()),
                ),
            },
            Example {
                example: "[[nothing {fallback: 0}] int] | schema | schema tuple --wrap-single",
                description: "create 1-tuple schema that wraps single values",
                result: Some(
                    Schema::Tuple {
                        items: vec![Schema::Any(
                            vec![
                                Schema::All(
                                    vec![
                                        Schema::Type(Type::Nothing.into_spanned(Span::test_data())),
                                        Schema::Fallback(Value::test_int(0)),
                                    ]
                                    .into_boxed_slice()
                                    .into_spanned(Span::test_data()),
                                ),
                                Schema::Type(Type::Int.into_spanned(Span::test_data())),
                            ]
                            .into_boxed_slice()
                            .into_spanned(Span::test_data()),
                        )]
                        .into_boxed_slice()
                        .into_spanned(Span::test_data()),
                        wrap_single: true.into_spanned(Span::test_data()),
                        wrap_null: false.into_spanned(Span::test_data()),
                        span: Span::test_data(),
                    }
                    .into_value(Span::test_data()),
                ),
            },
            Example {
                example: "[] | schema tuple --wrap-null",
                description: "create 0-tuple schema that wraps null",
                result: Some(
                    Schema::Tuple {
                        items: vec![].into_boxed_slice().into_spanned(Span::test_data()),
                        wrap_single: false.into_spanned(Span::test_data()),
                        wrap_null: true.into_spanned(Span::test_data()),
                        span: Span::test_data(),
                    }
                    .into_value(Span::test_data()),
                ),
            },
        ]
    }
    #[inline]
    fn run(
        &self,
        _plugin: &Self::Plugin,
        _engine: &nu_plugin::EngineInterface,
        call: &nu_plugin::EvaluatedCall,
        input: &Value,
    ) -> Result<Value, LabeledError> {
        eprintln!("call {:?}", call);
        Ok(Self::run_direct(
            input,
            get_switch_spanned(call, "wrap-single")?,
            get_switch_spanned(call, "wrap-null")?,
        )?
        .into_value(input.span()))
    }
}

#[cfg(test)]
#[test]
fn test_examples() -> Result<(), nu_protocol::ShellError> {
    nu_plugin_test_support::PluginTest::new("schema", SchemaPlugin.into())?
        .test_command_examples(&TupleCmd)
}
