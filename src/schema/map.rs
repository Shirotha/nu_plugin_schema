use nu_plugin::SimplePluginCommand;
use nu_protocol::{
    Example, IntRange, IntoSpanned, LabeledError, Range, Signature, Span, Spanned, SyntaxShape,
    Type, Value,
};

use crate::{unbounded, Schema, SchemaPlugin, ValueCmd};

pub struct MapCmd;
impl MapCmd {
    #[inline]
    pub fn run_direct(
        input: &Value,
        length: Option<Spanned<IntRange>>,
        wrap_null: Option<Spanned<bool>>,
    ) -> Result<Schema, LabeledError> {
        #[inline]
        fn into_schema(value: &Value) -> Result<Option<Spanned<Box<Schema>>>, LabeledError> {
            Ok(Some(
                Box::new(ValueCmd::run_direct(value)?).into_spanned(value.span()),
            ))
        }
        let length = length.unwrap_or_else(unbounded);
        let wrap_null = wrap_null.unwrap_or_else(|| false.into_spanned(Span::unknown()));
        match input {
            Value::List {
                vals,
                internal_span,
            } => match vals.len() {
                1 => Ok(Schema::Map {
                    keys: None,
                    values: into_schema(&vals[0])?,
                    length,
                    wrap_null,
                    span: *internal_span,
                }),
                2 => Ok(Schema::Map {
                    keys: into_schema(&vals[0])?,
                    values: into_schema(&vals[1])?,
                    length,
                    wrap_null,
                    span: *internal_span,
                }),
                _ => Err(LabeledError::new("expected list of length 1 or 2")
                    .with_label("needs to match [(keys,) values]", *internal_span)),
            },
            value => Ok(Schema::Map {
                keys: None,
                values: into_schema(value)?,
                length,
                wrap_null,
                span: input.span(),
            }),
        }
    }
}
impl SimplePluginCommand for MapCmd {
    type Plugin = SchemaPlugin;
    #[inline(always)]
    fn name(&self) -> &str {
        "schema map"
    }
    #[inline(always)]
    fn usage(&self) -> &str {
        "create a new schema for aa map (record)"
    }
    #[inline]
    fn signature(&self) -> Signature {
        let out = Type::Custom("Schema".into());
        Signature::build(self.name())
            .input_output_types(vec![
                (Type::List(Type::Any.into()), out.clone()),
                (Type::Any, out),
            ])
            .named("length", SyntaxShape::Range, "length constraint", Some('l'))
            .named(
                "wrap-null",
                SyntaxShape::Boolean,
                "treat null as empty map",
                Some('n'),
            )
    }
    #[inline]
    fn examples(&self) -> Vec<nu_protocol::Example> {
        vec![
            Example {
                example: "'int' | schema map --wrap-null",
                description: "create map schema with constraint values and wrapping null",
                result: None,
            },
            Example {
                example: "[([x y z] | each { wrap value }) int] | schema map --length 1..3",
                description: "create map schema with constraint keys and values and limited length",
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
            call.get_flag("wrap-null")?,
        )?
        .into_value(input.span()))
    }
}

#[cfg(test)]
#[test]
fn test_tuple_examples() -> Result<(), nu_protocol::ShellError> {
    nu_plugin_test_support::PluginTest::new("schema", SchemaPlugin.into())?
        .test_command_examples(&MapCmd)
}
