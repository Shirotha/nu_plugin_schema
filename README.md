# Schema
This plugin provides a mechanism to validate and coerce a `Value` of arbitrary complexity.

## Installation
Install using cargo
```nu
cargo install --git https://github.com/Shirotha/nu_plugin_schema.git
plugin add nu_plugin_schema
```

## Usage
Use the `normalize` command to test values.
It can either be used inline
```nu
42 | normalize int # => 42
'42' | normalize int # causes error
'42' | normalize int --result # => {err: "..."}
```
Or by defining a schema using the `schema` command and its subcommands
```nu
let schema = [[nothing {fallback: 0}] int] | schema
null | normalize $schema # => 0
let point = [
    int
    $schema
    $schema
] | schema tuple --wrap-single
[1 2 3] | normalize $tuple # => [1 2 3]
42 | normalize $tuple # => [42 0 0]
```
Nested schema can be defined using closures
```nu
def tree [] {
    let leaf = [int nothing] | schema
    let branch = {
        left: { normalize -r (tree) }
        right: { normalize -r (tree) }
    } | schema struct --wrap-missing
    [
        $leaf
        $branch
    ] | schema
}
let input = {
    left: {
        right: 1
    }
    right: 2
}
let tree = $input | normalize $node
$tree.right # => 2
$tree.left.left # => null
```
