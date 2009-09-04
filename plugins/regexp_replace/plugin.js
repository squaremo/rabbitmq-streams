{
    "name": "Regular Expression Replacer",
    "author": {"name": "Alexander Schmolck", "email": "alexander@lshift.net"},
    "help": "Replace the input, using the regular expressions and replacements given, in sequence.  These follow Perl regular expression syntax.",

    "type": "plugin-specification",
    "harness": "java",
    "subtype": "pipeline_component",

    "global_configuration_specification": [],
    "configuration_specification": [
      {"label": "Replacements",
       "name": "expressions",
       "type": ["list",
                [{"label": "Regexp",
                  "type": "string",
                  "name": "regexp"
                 },
                 {"label": "Replacement",
                  "type": "string",
                  "name": "replacement"
                 },
                 {"label": "Multiline",
                  "type": "boolean",
                  "name": "multiline",
                  "default": false
                 },
                 {"label": "Case insensitive",
                  "type": "boolean",
                  "name": "caseinsensitive",
                  "default": false
                 },
                 {"label": "Dot all",
                  "type": "boolean",
                  "name": "dotall",
                  "default": false
                 }]]},
      {"label": "Multiline",
       "type": "boolean",
       "name": "global_multiline",
       "default": false
      },
      {"label": "Case insensitive",
       "type": "boolean",
       "name": "global_caseinsensitive",
       "default": false
      },
      {"label": "Dot all",
       "type": "boolean",
       "name": "global_dotall",
       "default": false
      }
    ],

    "inputs_specification": [{"name": "input", "label": "Input"}],
    "outputs_specification": [{"name": "output", "label": "Output"}]
}
