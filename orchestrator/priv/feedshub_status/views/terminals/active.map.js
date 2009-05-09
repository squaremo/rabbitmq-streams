function(doc) {
    if (doc.type == "terminal-status" && doc.active) emit(null, doc._id);
}
