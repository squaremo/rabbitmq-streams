Identifier = /^[a-zA-Z_]+$/;

ConfigurationVariableDefinition = {
    "name": Identifier,
    "label": nonempty_string(),
    "type": nonempty_string() // can most certainly do better here TODO
};

ConnectorDefinition = {
    "name": Identifier,
    "label": nonempty_string()
};

DatabaseSpecification = dictionary(nonempty_string(),
				   {"language": "javascript",
				    "views": dictionary(nonempty_string(),
							{"map": /^function/})});

PluginBase = {
    "type": "plugin-specification",

    "name": nonempty_string(),
    "author": {"name": nonempty_string(),
	       "email": optional(email())},
    "harness": nonempty_string(),

    "global_configuration_specification": array_of(ConfigurationVariableDefinition),
    "configuration_specification": array_of(ConfigurationVariableDefinition),
    "database_specification": optional(DatabaseSpecification)
};

ServerPluginBase =
    merge(PluginBase, {
	      "subtype": "server"
	  });

SourceServerPlugin =
    merge(ServerPluginBase, {
	      "source_specification": array_of(ConfigurationVariableDefinition)
	  });

DestinationServerPlugin =
    merge(ServerPluginBase, {
	      "destination_specification": array_of(ConfigurationVariableDefinition)
	  });

SourceDestinationServerPlugin = merge(SourceServerPlugin, DestinationServerPlugin);

ComponentPlugin =
    merge(PluginBase, {
	      "subtype": "pipeline_component",
	      "inputs_specification": array_of(ConnectorDefinition),
	      "outputs_specification": array_of(ConnectorDefinition)
	  });

Plugin = or_dict({"SourceServerPlugin": SourceServerPlugin,
		  "DestinationServerPlugin": DestinationServerPlugin,
		  "SourceDestinationServerPlugin": SourceDestinationServerPlugin,
		  "ComponentPlugin": ComponentPlugin});
