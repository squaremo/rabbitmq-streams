{application, orchestrator,  %% -*- erlang -*-
 [{id,"RabbitMQ Streams"},
  {description,"RabbitMQ Streams server"},
  {vsn, "0.0.0"},
  {modules,[
            orchestrator,
            orchestrator_root,
            orchestrator_root_sup,
            couchapi
	   ]},
  {applications,[kernel,stdlib,crypto]},
  {mod, {orchestrator, []}},
  %% pass these in with ``erl [...] -orchestrator couch_base_url http://...``
  {env, [{couch_base_url, "YOU WILL HAVE TO SUPPLY SOMETHING LIKE THIS ON THE \
 COMMANDLINE:  http://localhost:5984/"},
         {root_config_url, "YOU WILL HAVE TO SUPPLY SOMETHING LIKE THIS ON THE \
 COMMANDLINE: http://localhost:5984/feedshub_status/root_config"},
         {config_db, "Supply on command line e.g., feedshub_status"},
         {debug, false},
         {trace, false}]}
 ]}.
