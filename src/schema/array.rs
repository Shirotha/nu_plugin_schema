use nu_plugin::SimplePluginCommand;
use nu_protocol::{
    Example, IntRange, IntoSpanned, IntoValue, LabeledError, Range, Signature, Span, Spanned,
    SyntaxShape, Type, Value,
};

use crate::{unbounded, Schema, SchemaPlugin, ValueCmd};

pub struct ArrayCmd;
impl ArrayCmd {
    #[inline]
    pub fn run_direct(
        input: &Value,
        length: Option<Spanned<IntRange>>,
        wrap_single: Option<Spanned<bool>>,
        wrap_null: Option<Spanned<bool>>,
    ) -> Result<Schema, LabeledError> {
        let items = Box::new(ValueCmd::run_direct(input)?).into_spanned(input.span());
        let default = false.into_spanned(Span::unknown());
        Ok(Schema::Array {
            items,
            length: length.unwrap_or_else(unbounded),
            wrap_single: wrap_single.unwrap_or(default),
            wrap_null: wrap_null.unwrap_or(default),
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
    // TODO: add explicit results to examples
    #[inline]
    fn examples(&self) -> Vec<nu_protocol::Example> {
        vec![
            Example {
                example: "'int' | schema array --length 2..10",
                description: "create a array schema that restricts the length",
                result: None,
            },
            Example {
                example: "[[nothing, {fallback: 0}] int] | schema array --wrap-null --wrap-single",
                description: "create a array schema that wraps single values and null",
                result: None,
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
            call.get_flag("length")?
                .map(|range: Spanned<Range>| {
                    let span = range.span;
                    let Range::IntRange(range) = range.item else {
                        return Err(LabeledError::new("only integer ranges are allowed")
                            .with_label("float range", span));
                    };
                    Ok(range.into_spanned(span))
                })
                .transpose()?,
            call.get_flag("wrap_single")?,
            call.get_flag("wrap_null")?,
        )?
        .into_value(input.span()))
    }
}

#[cfg(test)]
#[test]
fn test_tuple_examples() -> Result<(), nu_protocol::ShellError> {
    nu_plugin_test_support::PluginTest::new("schema", SchemaPlugin.into())?
        .test_command_examples(&ArrayCmd)
}
