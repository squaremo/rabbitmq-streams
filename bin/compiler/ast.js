/**
*
* start = ModelItem*
* 
* ModelItem = Feed
*           | Terminal
*           | Server
*
* Feed = Node* Edge*
*
* Node = PluginInstance
*      | TerminalMention
*
* PluginInstance = name configuration
* 
* TerminalMention = name
*
* Edge = Endpoint Endpoint
*
* Endpoint = name name?
*
* Terminal = ServerUse*
*
* ServerUse = name configuration? configuration?
* 
* Server = name configuration
*/

var AST = {};

function setActive(item) {
  item.active = true;
}

function feed(name, nodes, edges) {
  var feed = {"name": name, "type": "feed", "active": false};
  AST[name] = feed;
  var defn = {}; defn.wiring = {};
  defn.wiring['nodes'] = process_nodes(nodes);
  defn.wiring['edges'] = edges;
  feed.definition = defn;
  return feed;
}

// We construct the nodes as a pait of name and content; later, the output format is
// a dictionary, though.
function process_nodes(nodes) {
  var res = {};
  for (ind in nodes) {
    if (nodes.hasOwnProperty(ind)) {
      var n = nodes[ind];
      res[n[0]] = n[1];
    }
  }
  return res;
}

function terminal(name, serverUses) {
  var t = {
    "name": name,
    "type": "terminal",
    "active": false,
    "definition": {
      "servers": serverUses
    }
  };
  AST[name] = t;
  return t;
}

function serverUse(server, source, destination) {
  return {
    "server": (typeof server == "string") ? server : server.name,
    "source": source,
    "destination": destination
  };
}

function server(name, plugin, config) {
  var s = {
    "name": name,
    "type": "server",
    "active": false,
    "definition": {
      "server_type": plugin,
      "configuration": config
    }
  };
  AST[name] = s;
  return s;
}

// not ideal, we don't really want to have to name these
function pluginNode(name, pluginname, conf) {
  return [
    name, 
    {
      "type": pluginname,
      "configuration": conf
    }];
}

function terminalNode(name, terminal) {
  if (typeof terminal == "string") {
    return [name, {"terminal": terminal}];
  }
  else {
    return [name, {"terminal": terminal.name}]
  }
}

function noderef(node) {
  if (typeof node == "string") {
    return node;
  }
  else {
    return node[0]; // it's been constructed with pluginNode or terminalNode
  }
}

function endpoint() {
  var args = endpoint.arguments;
  return (args.length < 2) ?
    {"node": noderef(args[0])} :
    {"node": noderef(args[0]), "channel": args[1]};
}

function edge(from, to) {
  return {"from": from, "to": to}
}
