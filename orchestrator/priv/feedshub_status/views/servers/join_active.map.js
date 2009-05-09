function(doc) {
    if (doc.type == "server") {
	emit(doc._id, doc);
    } else if (doc.type == "server-status" && doc.active) {
	emit(doc._id.replace(/_status/, ""), doc);
    }
}
