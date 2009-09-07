-module(orchestrator_util).

-export([amqp_plugin_config/1]).

-include("orchestrator.hrl").

amqp_plugin_config(#amqp_config { host=Host,
                                  port=Port,
                                  virtual_host=VHost,
                                  user=User,
                                  password=Password }) ->
    {"messageserver",
     {obj, [{"host", list_to_binary(Host)},
            {"port", Port},
            {"virtual_host", VHost},
            {"username", list_to_binary(User)},
            {"password", list_to_binary(Password)}
           ]}}.
