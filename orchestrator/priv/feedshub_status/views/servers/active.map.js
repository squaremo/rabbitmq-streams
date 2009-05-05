function(doc) {
    if (doc.type == "server-status" && doc.active) emit(null, doc._id);
}
