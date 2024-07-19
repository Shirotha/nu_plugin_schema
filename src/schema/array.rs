use nu_plugin::SimplePluginCommand;
use nu_protocol::{
    ast::RangeInclusion, Example, IntRange, IntoSpanned, IntoValue, LabeledError, Signature, Span,
    Spanned, SyntaxShape, Type, Value,
};

use crate::{get_optional_range, get_switch_spanned, unbounded, Schema, SchemaPlugin, ValueCmd};

pub struct ArrayCmd;
impl ArrayCmd {
    #[inline]
    pub fn run_direct(
        input: &Value,
        length: Spanned<IntRange>,
        wrap_single: Spanned<bool>,
        wrap_null: Spanned<bool>,
    ) -> Result<Schema, LabeledError> {
        let items = Box::new(ValueCmd::run_direct(input)?).into_spanned(input.span());
        Ok(Schema::Array {
            items,
            length,
            wrap_single,
            wrap_null,
            span: input.span(),
        })
    }
}
impl SimplePluginCommand for ArrayCmd {
    type Plugin = SchemaPlugin;
    #[inline(always)]
    fn name(&self) -> &str {
        "schema array"
    }
    #[inline(always)]
    fn usage(&self) -> &str {
        "create a new schema for an array (list)"
    }
    #[inline]
    fn signature(&self) -> nu_protocol::Signature {
        Signature::build(self.name())
            .input_output_type(Type::Any, Schema::r#type())
            .named("length", SyntaxShape::Range, "length constraint", Some('l'))
            .switch(
                "wrap-single",
                "treat non-list, non-null values as array with single element",
                Some('s'),
            )
            .switch("wrap-null", "treat null as empty array", Some('n'))
    }
    #[inline]
    fn examples(&self) -> Vec<nu_protocol::Example> {
        vec![
            Example {
                example: "'int' | schema array --length 2..10",
                description: "create a array schema that restricts the length",
                result: Some(
                    Schema::Array {
                        items: Box::new(Schema::Type(Type::Int.into_spanned(Span::test_data())))
                            .into_spanned(Span::test_data()),
                        length: IntRange::new(
                            Value::test_int(2),
                            Value::test_int(3),
                            Value::test_int(10),
                            RangeInclusion::Inclusive,
                            Span::test_data(),
                        )
                        .unwrap()
                        .into_spanned(Span::test_data()),
                        wrap_single: false.into_spanned(Span::test_data()),
                        wrap_null: false.into_spanned(Span::test_data()),
                        span: Span::test_data(),
                    }
                    .into_value(Span::test_data()),
                ),
            },
            Example {
                example: "[[nothing, {fallback: 0}] int] | schema array --wrap-null --wrap-single",
                description: "create a array schema that wraps single values and null",
                result: Some(
                    Schema::Array {
                        items: Box::new(Schema::Any(
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
                        ))
                        .into_spanned(Span::test_data()),
                        length: unbounded(),
                        wrap_single: true.into_spanned(Span::test_data()),
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
        Ok(Self::run_direct(
            input,
            get_optional_range(call, "length")?,
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
        .test_command_examples(&ArrayCmd)
}
