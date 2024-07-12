use nu_plugin::Plugin;

use crate::schema::*;

pub mod normalize;
pub mod schema;

pub struct SchemaPlugin;
impl Plugin for SchemaPlugin {
    fn version(&self) -> String {
        env!("CARGO_PKG_VERSION").into()
    }
    fn commands(&self) -> Vec<Box<dyn nu_plugin::PluginCommand<Plugin = Self>>> {
        vec![Box::new(ValueCmd), Box::new(TupleCmd)]
    }
}
