function(doc) {
    if (doc.type == "terminal") {
	emit(doc._id, doc);
    } else if (doc.type == "terminal-status" && doc.active) {
	emit(doc._id.replace(/_status/, ""), doc);
    }
}
