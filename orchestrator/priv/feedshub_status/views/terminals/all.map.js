function(doc) {
    if (doc.type == "terminal") emit(null, doc._id);
}
