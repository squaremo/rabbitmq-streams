{application, orchestrator,  %% -*- erlang -*-
 [{description,"FeedsHub Orchestrator"},
  {vsn, "0.0"},
  {modules,[
            orchestrator,
            orchestrator_root,
            orchestrator_root_sup,
            couchapi
	   ]},
  {applications,[kernel,stdlib]},
  {mod, {orchestrator, []}},
  {env, [{couch_base_url, "http://localhost:5984/"}]}
 ]}.
