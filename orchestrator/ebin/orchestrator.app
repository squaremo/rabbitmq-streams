{application, orchestrator,  %% -*- erlang -*-
 [{description,"FeedsHub Orchestrator"},
  {vsn, "0.0"},
  {modules,[
            orchestrator,
            orchestrator_root,
            orchestrator_root_sup
	   ]},
  {applications,[kernel,stdlib]},
  {mod, {orchestrator, []}},
  {env, [
         {couch_host, "localhost"},
         {couch_port, 5984},
         {rabbitmq_host, "localhost"}
        ]}
 ]}.
