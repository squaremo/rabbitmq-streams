-module(streams).

-export([all_pipelines/1]).

all_pipelines(DbName) ->
    Rows = couchapi:get_view_rows(DbName, "feeds", "join?group=true"),
    Rows.
