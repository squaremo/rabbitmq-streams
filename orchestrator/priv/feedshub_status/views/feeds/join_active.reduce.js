function(keys, values, rereduce) {
    if (rereduce) {
	return values;
    } else {
	var output = {};
	for (idx in values) {
	    output[values[idx].type] = values[idx];
	}
	if (output["feed-status"]["active"]) {
	    return output;
	} else {
	    return {};
	}
    }
}