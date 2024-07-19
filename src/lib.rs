//! This plugin provides a mechanism to validate and coerce a [`Value`] of arbitrary complexity.
//!
//! # Usage
//! Use the `normalize` command to test values.
//! It can either be used inline
//! ```nu
//! 42 | normalize int # => 42
//! '42' | normalize int # causes error
//! '42' | normalize int --result # => {err: "..."}
//! ```
//! Or by defining a schema using the `schema` command and its subcommands
//! ```nu
//! let schema = [[nothing {fallback: 0}] int] | schema
//! null | normalize $schema # => 0
//! let point = [
//!     int
//!     $schema
//!     $schema
//! ] | schema tuple --wrap-single
//! [1 2 3] | normalize $tuple # => [1 2 3]
//! 42 | normalize $tuple # => [42 0 0]
//! ```
//! Nested schema can be defined using closures
//! ```nu
//! let leaf = [int nothing] | schema
//! let branch = {
//!     left: { normalize $node }
//!     right: { normalize $node }
//! } | schema tuple --wrap-missing
//! let node = [
//!     $leaf
//!     $branch
//! ] | schema
//! let input = {
//!     left: {
//!         right: 1
//!     }
//!     right: 2
//! }
//! let tree = normalize $node
//! $tree.right # => 2
//! $tree.left.left # => null
//! ```
use nu_plugin::{EvaluatedCall, Plugin};
use nu_protocol::{
    FromValue, IntRange, IntoSpanned, LabeledError, Range, ShellError, Span, Spanned,
};

use crate::{normalize::NormalizeCmd, schema::*};

pub mod normalize;
pub mod schema;

/// Core plugin type, manages all commands.
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
