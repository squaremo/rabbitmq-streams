{application, orchestrator,  %% -*- erlang -*-
 [{description,"FeedsHub Orchestrator"},
  {vsn, "0.0"},
  {modules,[
            orchestrator
	   ]},
  {applications,[kernel,stdlib]},
  {mod, {orchestrator, []}},
  {env, []}
 ]}.
