function(doc) {
    if (doc.type == "feed" && doc.active) emit(null, doc._id);
}
