use nu_plugin::SimplePluginCommand;
use nu_protocol::{
    Example, IntoSpanned, IntoValue, LabeledError, Signature, Span, Spanned, Type, Value,
};

use crate::{Schema, SchemaPlugin, ValueCmd};

pub struct StructCmd;
impl StructCmd {
    #[inline]
    pub fn run_direct(
        input: &Value,
        wrap_missing: Option<Spanned<bool>>,
    ) -> Result<Schema, LabeledError> {
        let fields = input.as_record()?;
        let fields = fields
            .iter()
            .map(|(field, value)| Ok((field.clone(), ValueCmd::run_direct(value)?)))
            .collect::<Result<Box<[_]>, LabeledError>>()?
            .into_spanned(input.span());
        Ok(Schema::Struct {
            fields,
            wrap_missing: wrap_missing.unwrap_or_else(|| false.into_spanned(Span::unknown())),
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
    fn usage(&self) -> &str {
        "create a new schema for a struct (record)"
    }
    #[inline]
    fn signature(&self) -> Signature {
        Signature::build(self.name())
            .input_output_type(Type::Record(vec![].into_boxed_slice()), Schema::r#type())
            .switch("wrap-missing", "treat missing fields as null", Some('m'))
    }
    #[inline]
    fn examples(&self) -> Vec<nu_protocol::Example> {
        vec![Example {
            example:
                "{x: int, y: int, z: [[nothing, {fallback: 0}] int]} | schema tuple --wrap-missing",
            description: "create a tuple schema with optional field",
            result: None,
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
        Ok(Self::run_direct(input, call.get_flag("wrap-missing")?)?.into_value(input.span()))
    }
}

#[cfg(test)]
#[test]
fn test_tuple_examples() -> Result<(), nu_protocol::ShellError> {
    nu_plugin_test_support::PluginTest::new("schema", SchemaPlugin.into())?
        .test_command_examples(&StructCmd)
}
