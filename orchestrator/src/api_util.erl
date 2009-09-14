-module(api_util).

-export([url/3, status_doc_type/1]).

% TODO Check the facet and resource type
url(Facet, ResourceType, Id) ->
    "/" ++ mochiweb_util:join([atom_to_list(Facet), atom_to_list(ResourceType), Id], "/").

status_doc_type(pipeline) -> <<"feed-status">>;
status_doc_type(gateway) -> <<"server-status">>.
