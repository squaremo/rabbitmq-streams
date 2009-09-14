-include("orchestrator.hrl").

-define(APPLICATION_NAME, <<"RabbitMQ Streams">>).
-define(APPLICATION_VERSION, ?FEEDSHUB_VERSION).

-define(DEFAULT_PIPELINE_FIELDS, ["name", "author"]).

-record(modelctx, {kind, doc = undefined}).
