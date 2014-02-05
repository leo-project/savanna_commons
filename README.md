# **savannadb_commons**

An original statistics library, which is able to easily realize to calculate statistics such as counter and histogram.

## Usage

```erlang

-include("savannadb_commons.hrl").

sample() ->
    %% Start "folsom"
    folsom:start(),

    %% Start "mnesia"
    mnesia:start(),

    %% Start savannadb_commons's supervisor
    {ok,_Pid} = svdbc_sup:start_link(),

    %% Create tables in order to manage the statistics-schemas
    {atomic,ok} = svdbc_tbl_schema:create_table(ram_copies, [node()]),
    {atomic,ok} = svdbc_tbl_column:create_table(ram_copies, [node()]),

    %% Create a schema
    SchemaName = 'test_1',
    ok = savannadb_commons:create_schema(
            SchemaName, [#svdb_column{name = 'col_1',
                                      type = ?COL_TYPE_COUNTER,
                                      constraint = [{min, 0}, {max, 16384}]},
                         #svdb_column{name = 'col_2',
                                      type = ?COL_TYPE_H_SLIDE,
                                      constraint = []},
                         #svdb_column{name = 'col_3',
                                      type = ?COL_TYPE_H_SLIDE,
                                      constraint = []}
                        ]),

    %% Create a metric by the schema
    Window = 10,
    Callback = fun({_SchemaName, _Key, _Value}) ->
                        io:format("schema:~w, key:~w, value:~p",
                                  [_SchemaName, _Key, _Value]),
                        ok
                end,
    ok = savannadb_commons:create_metrics_by_schema(SchemaName, Window, Callback),

    %% Notify events for a column (Counter)
    Key_1 = 'col_1',
    savannadb_commons:notify(Schema, {Key_1, 128}),
    savannadb_commons:notify(Schema, {Key_1, 256}),
    savannadb_commons:notify(Schema, {Key_1, 384}),
    savannadb_commons:notify(Schema, {Key_1, 512}),

    %% Notify events for a column (Histogram)
    Key_2 = 'col_2',
    savannadb_commons:notify(Schema, {Key_2,  16}),
    savannadb_commons:notify(Schema, {Key_2,  32}),
    savannadb_commons:notify(Schema, {Key_2,  64}),
    savannadb_commons:notify(Schema, {Key_2, 128}),
    savannadb_commons:notify(Schema, {Key_2, 128}),
    savannadb_commons:notify(Schema, {Key_2, 256}),
    savannadb_commons:notify(Schema, {Key_2, 512}),

    %% Retrieve stats
    {ok, _Ret_1} = savannadb_commons:get_metric_value(Schema, Key_1),
    {ok, _Ret_2} = savannadb_commons:get_histogram_statistics(Schema, Key_2),
    ok.

```

## License

savannadb_commons's license is [Apache License Version 2.0](http://www.apache.org/licenses/LICENSE-2.0.html)
