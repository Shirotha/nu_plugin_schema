use nu_plugin::SimplePluginCommand;
use nu_protocol::{
    ast::RangeInclusion, Example, IntRange, IntoSpanned, IntoValue, LabeledError, Signature, Span,
    Spanned, SyntaxShape, Type, Value,
};

use crate::{get_optional_range, get_switch_spanned, unbounded, Schema, SchemaPlugin, ValueCmd};

/// Plugin command to create a [`Schema`] for a map.
pub struct MapCmd;
impl MapCmd {
    #[inline]
    pub fn run_direct(
        input: &Value,
        length: Spanned<IntRange>,
        wrap_null: Spanned<bool>,
    ) -> Result<Schema, LabeledError> {
        #[inline]
        fn into_schema(value: &Value) -> Result<Option<Spanned<Box<Schema>>>, LabeledError> {
            Ok(Some(
                Box::new(ValueCmd::run_direct(value)?).into_spanned(value.span()),
            ))
        }
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
    fn description(&self) -> &str {
        "create a new schema for aa map (record)"
    }
    #[inline]
    fn signature(&self) -> Signature {
        let out = Schema::r#type();
        Signature::build(self.name())
            .input_output_types(vec![
                (Type::List(Type::Any.into()), out.clone()),
                (Type::Any, out),
            ])
            .named("length", SyntaxShape::Range, "length constraint", Some('l'))
            .switch("wrap-null", "treat null as empty map", Some('n'))
    }
    #[inline]
    fn examples(&self) -> Vec<nu_protocol::Example> {
        vec![
            Example {
                example: "'int' | schema map --wrap-null",
                description: "create map schema with constraint values and wrapping null",
                result: Some(
                    Schema::Map {
                        keys: None,
                        values: Some(
                            Box::new(Schema::Type(Type::Int.into_spanned(Span::test_data())))
                                .into_spanned(Span::test_data()),
                        ),
                        length: unbounded(),
                        wrap_null: true.into_spanned(Span::test_data()),
                        span: Span::test_data(),
                    }
                    .into_value(Span::test_data()),
                ),
            },
            Example {
                example: "[[{value: x} {value: y} {value: z}] int] | schema map --length 1..3",
                description: "create map schema with constraint keys and values and limited length",
                result: Some(
                    Schema::Map {
                        keys: Some(
                            Box::new(Schema::Any(
                                vec![
                                    Schema::Value(Value::test_string("x")),
                                    Schema::Value(Value::test_string("y")),
                                    Schema::Value(Value::test_string("z")),
                                ]
                                .into_boxed_slice()
                                .into_spanned(Span::test_data()),
                            ))
                            .into_spanned(Span::test_data()),
                        ),
                        values: Some(
                            Box::new(Schema::Type(Type::Int.into_spanned(Span::test_data())))
                                .into_spanned(Span::test_data()),
                        ),
                        length: IntRange::new(
                            Value::test_int(1),
                            Value::test_int(2),
                            Value::test_int(3),
                            RangeInclusion::Inclusive,
                            Span::test_data(),
                        )
                        .unwrap()
                        .into_spanned(Span::test_data()),
                        wrap_null: false.into_spanned(Span::test_data()),
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
            get_switch_spanned(call, "wrap-null")?,
        )?
        .into_value(input.span()))
    }
}

#[cfg(test)]
#[test]
fn test_examples() -> Result<(), nu_protocol::ShellError> {
    nu_plugin_test_support::PluginTest::new("schema", SchemaPlugin.into())?
        .test_command_examples(&MapCmd)
}
