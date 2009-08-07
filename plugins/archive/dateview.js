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


