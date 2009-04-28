function(doc) {
    if (doc.type == "feed-status" && doc.active) emit(null, doc._id);
}
