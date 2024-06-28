use nu_plugin::{serve_plugin, MsgPackSerializer};
use nu_plugin_schema::SchemaPlugin;

fn main() {
    serve_plugin(&SchemaPlugin, MsgPackSerializer)
}
