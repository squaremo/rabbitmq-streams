function(doc) {
    date = new Date(doc.updated)
    year = date.getFullYear()
    month = date.getMonth() + 1
    day = date.getDate()
    hour = date.getHours()
    minute = date.getMinutes()
    seconds = date.getSeconds()
    emit([year, month, day, hour, minute, seconds], {Body: doc.body})
}


/*
 * Apparently this should subset the view: startkey=[1,2009,5]&endkey=[1,2009,5,{}]#
 * For example http://localhost:5984/archive_outtestarchive/_design/by_date/_view/By%20Date?startkey=[2007]&endkey=[2009,{}]
 */