function(doc) {
    if (doc.type == "feed" && doc.active) emit(doc._id, null);
}
