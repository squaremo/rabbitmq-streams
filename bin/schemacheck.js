#!/usr/bin/env js
// -*- javascript -*-
//
// Javascript JSON schema checker. General idea (non-backtracking
// higher-order pattern matchers), but not code, taken from
// http://www.cerny-online.com/cerny.js/documentation/1.0/schema/
//
// Type tests adapted from http://www.JSON.org/json2.js
//
///////////////////////////////////////////////////////////////////////////

// Returns a false value if validation succeeded; otherwise, returns a
// non-false description of the cause of the validation failure.
function validate(obj, schema) {
    if (schema instanceof RegExp) {
	if ((obj + "").match(schema)) {
	    return false;
	} else {
	    return "RegExp failed: " + schema;
	}
    }

    if (typeof schema === 'function') {
	try {
	    return schema(obj);
	} catch (e) {
	    return "Predicate failed: " + e;
	}
    }

    if (obj === schema) {
	return false;
    }

    if (typeof obj !== typeof schema) {
	return "Type mismatch: expected typeof to be " + (typeof schema);
    }

    if (typeof schema !== 'object') {
	return "Value mismatch: expected " + schema;
    }

    // Recursive structural equivalence.
    var key;

    if (!(schema.hasOwnProperty("_extensible") && schema._extensible)) {
	// Ensure there's nothing in the object that the schema doesn't have.
	var oKeys = {};
	for (key in obj) {
	    if (obj.hasOwnProperty(key)) {
		oKeys[key] = 1;
	    }
	}
	for (key in schema) {
	    if (!special_property(key) && schema.hasOwnProperty(key)) {
		delete oKeys[key];
	    }
	}

	var aKeys = [];
	for (key in oKeys) {
	    aKeys.push(key);
	}
	if (aKeys.length) {
	    return "Unexpected properties: " + aKeys.join(", ");
	}
    }

    var haveResult = false;
    var result = {};
    var intermediate;

    for (key in schema) {
	if (!special_property(key) && schema.hasOwnProperty(key)) {
	    intermediate = validate(obj[key], schema[key]);
	    if (intermediate) {
		result[key] = intermediate;
		haveResult = true;
	    }
	}
    }

    return haveResult ? result : false;
}

function special_property(propname) {
    return propname === "_extensible";
}

function optional(schema) {
    return function(obj) {
	return (obj === undefined) ? false : validate(obj, schema);
    };
}

function merge() {
    var pieces = arguments;
    var result = {};
    for (var i = 0; i < pieces.length; i++) {
	var piece = pieces[i];
	for (var key in piece) {
	    if (piece.hasOwnProperty(key)) {
		result[key] = piece[key];
	    }
	}
    }
    return result;
}

function dictionary(keyschema, valueschema) {
    return function(obj) {
	if (typeof obj !== 'object' || obj === null) {
	    return "Expected dictionary";
	}

	var haveResult = false;
	var result = {};

	for (var key in obj) {
	    if (obj.hasOwnProperty(key)) {
		var intermediate = validate(key, keyschema);
		if (intermediate) {
		    result["key " + key] = intermediate;
		    haveResult = true;
		} else {
		    intermediate = validate(obj[key], valueschema);
		    if (intermediate) {
			result["valueAt " + key] = intermediate;
			haveResult = true;
		    }
		}
	    }
	}

	return haveResult ? result : false;
    }
}

function array_of(schema) {
    return function(obj) {
	if ((typeof obj === 'object') &&
	    (typeof obj.length === 'number') &&
	    (!obj.propertyIsEnumerable('length')))
	{
	    var haveResult = false;
	    var result = {};
	    for (var i = 0; i < obj.length; i++) {
		var intermediate = validate(obj[i], schema);
		if (intermediate) {
		    result[i] = intermediate;
		    haveResult = true;
		}
	    }
	    return haveResult ? result : false;
	} else {
	    return "Expected array";
	}
    };
}

function not(schema) {
    return function (obj) { return validate(obj, schema) ? false : "Negation failed"; };
}

function or_validator(schemas) {
    return function (obj) {
	var result = {};
	for (var key in schemas) {
	    if (schemas.hasOwnProperty(key)) {
		var intermediate = validate(obj, schemas[key]);
		if (!intermediate) return false;
		result[key] = intermediate;
	    }
	}
	return result;
    };
}

function or() {
    return or_validator(arguments);
}

function or_dict(d) {
    return or_validator(d);
}

function and() {
    var schemas = arguments;
    return function (obj) {
	for (var i = 0; i < schemas.length; i++) {
	    var intermediate = validate(obj, schemas[i]);
	    if (intermediate) return intermediate;
	}
	return false;
    };
}

function number() {
    return function (obj) {
	return (typeof obj === 'number') ? false : "Expected number";
    };
}

function string() {
    return function (obj) {
	return (typeof obj === 'string') ? false : "Expected string";
    };
}

function nonempty_string() {
    return function (obj) {
	return ((typeof obj === 'string') && (obj.length > 0))
	    ? false : "Expected non-empty string";
    };
}

function anything() {
    return function (obj) { return false; };
}

function email() {
    return nonempty_string(); // ha ha. Find me a sane regex and I'll put it in ;)
}

///////////////////////////////////////////////////////////////////////////

function main(argv) {
    if (argv.length == 0) {
	print("Usage: schemacheck.js [<schemafile> ...] <type>");
	print("Checks the JSON document presented to standard input.");
	quit(1);
    }

    var schemaFilenames = [];
    while (argv.length > 1) {
	schemaFilenames.push(argv.shift());
    }

    var topTypeExpr = argv.shift();

    for (var i = 0; i < schemaFilenames.length; i++) {
	load(schemaFilenames[i]);
    }

    // Spidermonkey doesn't have a compiled-in readfile operation in
    // most builds, but it *does* have readline(). Sadly, readline()
    // returns the empty string both when an empty line is read and on
    // the end of file, so this revolting hack is required to
    // heuristically detect the end of the input!
    function readStdin() {
	var input = "";
	var blankcount = 0; // ewwwwwwww yuuuuuuuck
	while (blankcount < 50) // utterly arbitrary, gross
	{
	    var line = readline();
	    if (!line) {
		blankcount++;
	    } else {
		blankcount = 0;
	    }
	    input = input + line + "\n";
	}
	return input.substring(0, input.length - blankcount);
    }

    var result = validate(eval("(" + readStdin() + ")"), eval(topTypeExpr));
    if (result) {
	print(uneval(result));
	quit(1);
    } else {
	quit(0);
    }
}

main(arguments);
