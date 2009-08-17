-module(streams).

-export([all_pipelines/1]).

all_pipelines(DbName) ->
    couchapi:get_view_rows(DbName, "feeds", "all").
