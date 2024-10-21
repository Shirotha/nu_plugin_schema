plugin use schema

def ident [] {
    if $in =~ '^(\[.*?\]|[A-Za-z][A-Za-z0-9_]*)$' {
        {ok: $in}
    } else {
        {err: "invalid sql identifier"}
    }
}
def expression [] {
    [
        string
        ({ normalize -r (expression) } | schema array)
        ({value: [[]]} | schema struct --wrap-single)
    ] | schema
}
let select = {
    from: [
        ({ ident } | schema array --wrap-single --wrap-null)
        ([{ ident } [nothing { ident }]] | schema map)
        { ident }
    ]
    where: ({ normalize -r (expression) } | schema array --wrap-null),
    join: ([{ ident } ({
            type: [[([INNER LEFT RIGHT OUTER] | wrap value)] [nothing {fallback: LEFT}]]
            table: [nothing { ident }]
            condition: ({ normalize -r (expression)} | schema array)
        } | schema struct --wrap-missing)
    ] | schema map --wrap-null)
    output: [[nothing {fallback: ["*"]}] ('string' | schema array --wrap-single)]
} | schema struct --wrap-missing

{
    from: {
        t1: first_table
    }
    where: [id EQ 42]
    join: {
        t2: {
            table: second_table
            type: LEFT
            condition: [t1.id EQ t2.id]
        }
    }
} | normalize $select
