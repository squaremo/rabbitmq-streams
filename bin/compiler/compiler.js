/*
* Compile the files given in the arguments to Streams definition files.
*/

importPackage(java.io);
//load(['json2.js']);
//load(['ast.js']);

function writeToFiles(dir, ast) {
  for (key in ast) {
    if (ast.hasOwnProperty(key)) writeConfig(dir, key, ast[key]);
  }
}

function writeConfig(dir, name, ast) {
  var configFile = name + '.js';
  var active = ast.active;
  var kind = ast.type;
  var defn = ast.definition;
  defn['type'] = kind;
  writeJsonToFile(defn, dir, configFile);

  var status_doc = {'type': kind + '-status', 'active': active};
  var statusFile = name + '_status.js';
  writeJsonToFile(status_doc, dir, statusFile);
}

function writeJsonToFile(doc, dir, filename) {
  var outDir = new File(dir);
  if (!(outDir.exists() && outDir.isDirectory())) throw dir + " is not a directory.";
  outFile = new File(outDir, filename);
  outFile.createNewFile(); // ignore result
  if (!outFile.canWrite()) throw "Cannot write to file " + outFile.getAbsolutePath();
  out = new FileWriter(outFile);
  var json = JSON.stringify(doc);
  out.write(json);
  out.close();
}

var requireBase = "./";
function require(file) {
  load([requireBase+'/'+file]);
}

var outputDir = arguments.shift();
if (arguments.length < 1) throw "No files supplied";
print("Compiling " + arguments + " to directory " + outputDir);
for (var i in arguments) {
  var arg = arguments[i];
  var file = new java.io.File(arg);
  requireBase = file.getAbsoluteFile().getParent();
  load(arguments[i]);
}
writeToFiles(outputDir, AST);
