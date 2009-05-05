function(doc) {
    if (doc.type == "feed") {
	emit(doc._id, doc);
    } else if (doc.type == "feed-status" && doc.active) {
	emit(doc._id.replace(/_status/, ""), doc);
    }
}
