plugin.js # applies to both pipeline components and servers

{ name: String # textual description,
  author: { name : String } # who wrote the plugin
  type: "plugin-specification" String
  harness: "java" | "python" | "ruby" String # type of harness to run
  subtype: "pipeline_component" | "server"
  global_configuration_specification: [{ name: "name", label: "label", type: "Type" }]
                           # speculative for configuration that is more
                           # user editable than just editing plugin.js
                           # but is applicable to every instance of this
                           # plugin.js file
  configuration_specification: [ { name: "port", label: "Port", type: "Nat" } ]
       # the configuration that must be provided per plugin instance
  destination_specification: [ { name: "title", label: "Title for RSS", type: "String" } ]
       # configuration per terminal (terminal ONLY)
  source_specification: [ { name: "url", label: "URL of RSS", type: "URL" } ]
       # configuration per terminal (terminal ONLY)
  inputs_specification: [ { name: "input" } ]
       # (feed_component ONLY)
  outputs_specification: [ { name: "output" } ]
       # (feed_component ONLY)
  database_specification: null | {} # initial values for the per instance db
}

-----------------------

feedshub_status/abc123 (as a feed)

{ type: "feed" String
  wiring: {
        nodes : {
            "node_name" : { type : "regexp_matcher"
                            configuration : { name : value }
                                # where "name" comes from the
                                # configuration_specification corresponding to the
                                # above value for the "type" field
                          },
            ...,
            "node_name_for_terminal" :
                          { terminal : "abc123" String # the terminal name }
            },
        edges : [
              {from: {node: "node_name", channel: "output"}, to: {node: "node_name2", channel: "input"}},
              {from: {node: "node_name", channel: "output"}, to: {node: "node_name_for_terminal"}}, # use terminal as output
              {from: {node: "node_name_for_terminal"}, to: {node: "node_name2", channel: "input"}}, # use terminal as input
              ...
            ]
}

-----------------------

feedshub_status/abc123 (as a server)

{ type: "server" String
  server_type: "archiver" String # the name of the server
  configuration: { name : value }
      # where "name" comes from the configuration_specification
      # in the corresponding plugin.js
}

-----------------------

feedshub_config/abc123 (as a terminal)

{ type: "terminal" String
  server: "abc123server" # the name of the server instance
  destination: { name : value }
  source: { name : value }
  # must be provided if the _specification in the plugin.js is not empty,
  # and correspond to the relevant sections in the plugin.js for the server
}

-----------------------

What gets sent to the plugin on STDIN at startup:

(for a plugin)

{ harness_type: "java" # String from harness in plugin.js
  plugin_name: "xslt"  # String from type in nodes in wiring in feed config
  plugin_dir: "/home/matthew/feedshub/orchestrator/priv/../../plugins/xslt/"
  feed_id: "abc123"
  node_id: "the_transformer"
  plugin_type: { name: "XSLT Transformation" # this obj taken from plugin.js
                 author: {name: "LShift Ltd."}
                 type: "plugin"
                 harness: "java"
                 subtype: "pipeline_component"
                 global_configuration_specification: {}
                 configuration_specification:
                     {name: "stylesheet_url"
                      label: "Stylesheet URL"
                      type: "url"
                     }
                 inputs_specification: [ {name: "input"} ]
                 outputs_specification: [ {name: "output"} ]
                 database_specification: null # (initial database content)
               }
  global_configuration: {} # place holder - currently we don't know where the values come from
  configuration: # this comes from the feeds config, the node in the wiring
              {stylesheet_url: "http://dev.lshift.net/matthew/sample.xslt"}
  messageserver: # provided by the orchestrator
              {host: "localhost"
               port: 5672
               virtual_host: "/"
               username: "feedshub_admin"
               password: "feedshub_admin"
              }
  inputs:  {"input": "abc123_the_transformer_input"}, # Q name provided by orchestrator
  outputs: {"output": "abc123_the_transformer_output"}, # Exchange name provided by orchestrator
  state: "http://localhost:5984/feed_abc123/state_the_transformer"
          # the document within the feed state database that can be used to store state
  database: null | url # the url of the private db if database_specification non-null
}

(for a server)

{ harness_type: "java" # String from harness in plugin.js
  plugin_name: "archiver"  # String from server_type in server config
  plugin_dir: "/home/matthew/feedshub/orchestrator/priv/../../plugins/xslt/"
  server_id: "abc123"
  plugin_type: { name: "Archiver" # this obj taken from plugin.js
                 author: {name: "LShift Ltd."}
                 type: "plugin"
                 harness: "java"
                 subtype: "server"
                 global_configuration_specification: {}
                 configuration_specification:
                     {name: "port"
                      label: "Port to listen on"
                      type: "Nat"
                     }
                 source_specification: [ {name: "title", type: "String", label: "Title for RSS feed"} ]
                 destination_specification: [ {name: "url", type: "URL", label: "URL of RSS"} ]
                 database_specification: null # (initial database content)
               }
  global_configuration: {} # place holder - currently we don't know where the values come from
  configuration: # this comes from the server config
              { port: 1234 }
  messageserver: # provided by the orchestrator
              {host: "localhost"
               port: 5672
               virtual_host: "/"
               username: "feedshub_admin"
               password: "feedshub_admin"
              }
  database: "http://localhost:5984/server_abc123/"
          # the database for the server, populated as per database_specification
          # (though exists even if database_specification is null)
  inputs: {"command": "abc123_command", "input": "abc123_input"}
  outputs: {"output": "abc123_output"}
          # outputs only exists for servers that are sources
          # inputs always exists, but only contains command for servers that sources
  terminals_database: "http://localhost:5984/feedshub_status/"
          # where to find the configuration of the terminals
}
