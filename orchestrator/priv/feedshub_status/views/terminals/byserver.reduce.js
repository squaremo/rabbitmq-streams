function(keys, values, rereduce) {
  if (rereduce) {
	  var output = {"terminals": []};
	  for(v in values)	{
	  	for(t in values[v].terminals)	{
	  		output["terminals"].push(values[v].terminals[t]);
	  	}
	  	output["server"] = values[v].server;
	  }
  	return output;
  } 
  else {  
    var output = {"terminals": []}
  	for(t in values)	{
  	  var v = values[t];
  	  if(v.type == "terminal") {
  	    output["terminals"].push(v);
  	  }
  	  else if (v.type == "server") {
          output["server"] = v;
      }
  	}
    return output;
  }
}
