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
  {env, [
         {couch_base_url, "http://localhost:5984/"},
         {rabbitmq_host, "localhost"},
         {rabbitmq_feedshub_admin_user, "feedshub_admin"},
         {rabbitmq_feedshub_admin_password, "feedshub_admin"}
        ]}
 ]}.
