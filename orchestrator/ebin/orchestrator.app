{application, orchestrator,  %% -*- erlang -*-
 [{description,"FeedsHub Orchestrator"},
  {vsn, "0.0"},
  {modules,[
            orchestrator,
            orchestrator_root,
            orchestrator_root_sup,
            couchapi
	   ]},
  {applications,[kernel,stdlib,crypto]},
  {mod, {orchestrator, []}},
  %% pass these in with ``erl [...] -orcestrator couch_base_url http://...``
  {env, [{couch_base_url, "YOU WILL HAVE TO SUPPLY SOMETHING LIKE THIS ON THE \
 COMMANDLINE:  http://localhost:5984/"},
         {root_config_url, "YOU WILL HAVE TO SUPPLY SOMETHING LIKE THIS ON THE \
 COMMANDLINE: http://localhost:5984/feedshub_status/root_config"}]}
 ]}.
