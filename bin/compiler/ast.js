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

function declare_terminal(name) {
  var t = {
    "name": name,
    "type": "terminal",
    "active": false,
    "definition": {
      "servers": []
    }
  };
  AST[name] = t;
  return t;
}

function addServerUse(terminal, server, source, destination) {
  function serverUse(server, source, destination) {
    return {
      "server": (typeof server == "string") ? server : server.name,
      "source": source,
      "destination": destination
    };
  }
  var servers = terminal["definition"]["servers"];
  servers.push(serverUse(server, source, destination));
}

// Consume from a server's ingress point -- e.g., a HTTP POST path, to a terminal.
function consume(server, ingresspoint, terminal) {
  addServerUse(terminal, server, ingresspoint, {});
}

// Subscribe a remote resource to a terminal via a server;
// e.g., subscribe node "pubsub.feedshub.livetext.radio1" on
// the PushFeeds server to the radio1_livetext terminal.
function subscribe(server, egresspoint, terminal) {
  addServerUse(terminal, server, {}, egresspoint);
}

// Bind a terminal to itself as a relay via server.  Usually we would
// bind two things together, possibly with some arguments, but in our
// model currently a terminal serves as both sides of the bind; i.e.,
// it looks like both a source and a destination.
function bind(server, terminal) {
  addServerUse(terminal, server, {}, {});
}

function create_server(name, plugin, config) {
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

// For creating feeds.  This is somewhat different to the messaging
// style stuff above; a feed has its own namespace, and things must be
// "registered" within the namespace, including what are otherwise
// (unlike terminals) anonymous pipeline components.  For this reason,
// we gensym names and return them, to be used in feed.connect()
// afterwards.
function feed(name) {
  var feed = {"name": name, "type": "feed", "active": false};
  AST[name] = feed;
  var defn = {}; defn.wiring = {};
  defn.wiring['nodes'] = {};
  defn.wiring['edges'] = [];
  feed.definition = defn;
  var counter = 0;

  var gensym = function(kind) {
    counter++;
    return name+'-'+kind+counter
  };

  var connection = function(from, to) {
    return {"from": from, "to": to}
  }

  feed.plugin = function(plugin) {
    var name = gensym('component');
    defn.wiring.nodes[name] = plugin;
    return name;
  };
  feed.terminal = function(terminal) {
    var name = gensym('terminal');
    defn.wiring.nodes[name] = {'terminal': terminal.name};
    return name;
  }
  feed.connect = function(from, to) {
    defn.wiring.edges.push(connection(from, to));
  }
  return feed;
}

function resource(node, point) {
  return {"node": node, "channel": point};
}
