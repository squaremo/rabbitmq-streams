function(keys, values, rereduce) {
    if (rereduce) {
	return values;
    } else {
	var output = {};
	for (idx in values) {
	    output[values[idx].type] = values[idx];
	}
	if (!(output["server-status"] && output["server-status"]["active"])) {
	    output = null;
	}
	return output;
    }
}