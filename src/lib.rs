use nu_plugin::{EvaluatedCall, Plugin};
use nu_protocol::{
    FromValue, IntRange, IntoSpanned, LabeledError, Range, ShellError, Span, Spanned,
};

use crate::{normalize::NormalizeCmd, schema::*};

pub mod normalize;
pub mod schema;

pub struct SchemaPlugin;
impl Plugin for SchemaPlugin {
    fn version(&self) -> String {
        env!("CARGO_PKG_VERSION").into()
    }
    fn commands(&self) -> Vec<Box<dyn nu_plugin::PluginCommand<Plugin = Self>>> {
        vec![
            Box::new(ValueCmd),
            Box::new(TupleCmd),
            Box::new(ArrayCmd),
            Box::new(StructCmd),
            Box::new(MapCmd),
            Box::new(NormalizeCmd),
        ]
    }
}

/// Gets value from an optional named boolean (switch), preserving the span.
#[inline]
pub fn get_switch_spanned(call: &EvaluatedCall, flag: &str) -> Result<Spanned<bool>, ShellError> {
    for (name, value) in &call.named {
        if name.item != flag {
            continue;
        }
        return Ok(value
            .clone()
            .map(FromValue::from_value)
            .transpose()?
            .unwrap_or_else(|| true.into_spanned(name.span)));
    }
    Ok(false.into_spanned(Span::unknown()))
}

/// Gets value from an optional range flag, defaulting to `0..`
pub fn get_optional_range(
    call: &EvaluatedCall,
    name: &str,
) -> Result<Spanned<IntRange>, LabeledError> {
    if let Some(range) = call.get_flag::<Spanned<Range>>(name)? {
        let span = range.span;
        let Range::IntRange(range) = range.item else {
            return Err(LabeledError::new("only integer ranges are allowed")
                .with_label("float range", span));
        };
        Ok(range.into_spanned(span))
    } else {
        Ok(unbounded())
    }
}
