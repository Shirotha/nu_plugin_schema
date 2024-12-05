use nu_plugin::SimplePluginCommand;
use nu_protocol::{
    Example, IntoSpanned, IntoValue, LabeledError, Signature, Span, Spanned, Type, Value,
};

use crate::{get_switch_spanned, Schema, SchemaPlugin, ValueCmd};

/// Plugin command to createa a [`Schema`] for a struct.
pub struct StructCmd;
impl StructCmd {
    #[inline]
    pub fn run_direct(
        input: &Value,
        wrap_missing: Spanned<bool>,
        wrap_list: Spanned<bool>,
        wrap_single: Spanned<bool>,
        wrap_null: Spanned<bool>,
    ) -> Result<Schema, LabeledError> {
        let fields = input.as_record()?;
        let fields = fields
            .iter()
            .map(|(field, value)| Ok((field.clone(), ValueCmd::run_direct(value)?)))
            .collect::<Result<Box<[_]>, LabeledError>>()?
            .into_spanned(input.span());
        Ok(Schema::Struct {
            fields,
            wrap_missing,
            wrap_list,
            wrap_single,
            wrap_null,
            span: input.span(),
        })
    }
}
impl SimplePluginCommand for StructCmd {
    type Plugin = SchemaPlugin;
    #[inline(always)]
    fn name(&self) -> &str {
        "schema struct"
    }
    #[inline(always)]
    fn description(&self) -> &str {
        "create a new schema for a struct (record)"
    }
    #[inline]
    fn signature(&self) -> Signature {
        Signature::build(self.name())
            .input_output_type(Type::Record(vec![].into_boxed_slice()), Schema::r#type())
            .switch("wrap-missing", "treat missing fields as null", Some('m'))
            .switch("wrap-list", "treat list as ordered fields", Some('l'))
            .switch("wrap-single", "treat list as ordered fields", Some('s'))
            .switch("wrap-null", "treat null as empty record", Some('n'))
    }
    #[inline]
    fn examples(&self) -> Vec<nu_protocol::Example> {
        vec![Example {
            example:
                "{x: int, y: int, z: [[nothing, {fallback: 0}] int]} | schema struct --wrap-missing",
            description: "create a struct schema with optional field",
            result: Some(
                Schema::Struct {
                    fields: vec![
                        (
                            "x".to_string(),
                            Schema::Type(Type::Int.into_spanned(Span::test_data())),
                        ),
                        (
                            "y".to_string(),
                            Schema::Type(Type::Int.into_spanned(Span::test_data())),
                        ),
                        (
                            "z".to_string(),
                            Schema::Any(
                                vec![
                                    Schema::All(
                                        vec![
                                            Schema::Type(
                                                Type::Nothing.into_spanned(Span::test_data()),
                                            ),
                                            Schema::Fallback(Value::test_int(0)),
                                        ]
                                        .into_boxed_slice()
                                        .into_spanned(Span::test_data()),
                                    ),
                                    Schema::Type(Type::Int.into_spanned(Span::test_data())),
                                ]
                                .into_boxed_slice()
                                .into_spanned(Span::test_data()),
                            ),
                        ),
                    ]
                    .into_boxed_slice()
                    .into_spanned(Span::test_data()),
                    wrap_missing: true.into_spanned(Span::test_data()),
                    wrap_list: false.into_spanned(Span::test_data()),
                    wrap_single: false.into_spanned(Span::test_data()),
                    wrap_null: false.into_spanned(Span::test_data()),
                    span: Span::test_data(),
                }
                .into_value(Span::test_data()),
            ),
        }, Example {
            example:
                "{x: int, y: int} | schema struct --wrap-list",
            description: "create a struct schema that treats lists as ordered fields",
            result: Some(
                Schema::Struct {
                    fields: vec![
                        (
                            "x".to_string(),
                            Schema::Type(Type::Int.into_spanned(Span::test_data())),
                        ),
                        (
                            "y".to_string(),
                            Schema::Type(Type::Int.into_spanned(Span::test_data())),
                        ),
                    ]
                    .into_boxed_slice()
                    .into_spanned(Span::test_data()),
                    wrap_missing: false.into_spanned(Span::test_data()),
                    wrap_list: true.into_spanned(Span::test_data()),
                    wrap_single: false.into_spanned(Span::test_data()),
                    wrap_null: false.into_spanned(Span::test_data()),
                    span: Span::test_data(),
                }
                .into_value(Span::test_data()),
            ),
        }]
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
            get_switch_spanned(call, "wrap-missing")?,
            get_switch_spanned(call, "wrap-list")?,
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
        .test_command_examples(&StructCmd)
}
