{application, api,
 [{description, "Streams API"},
  {vsn, "0.01"},
  {modules, [
    api,
    api_app,
    api_sup,
    api_web,
    api_deps,
    streams
  ]},
  {registered, []},
  {mod, {api_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
