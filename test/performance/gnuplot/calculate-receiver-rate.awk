# Calculate the TPS, average over each 1 second block from the start
BEGIN { 
        FS="," 
        timeS = 0
        startT = 0
        txCount = 0
        print "Time (s), TPS"
} 

# Only match lines starting with numeric data, i.e., ignore header
/[0-9]/ { 
    txCount++

    # First row
    if(startT == 0) {
        startT = $4
    }

    # Print number of tx in previous 1s
    if($4 >= (startT + 1000)) {
       print timeS, txCount
       timeS++
       startT = $4
       txCount = 0
    }
} 

END {
    # Ignore any remaining tx - there will be less than 1s of data which is
    # not enough to give any meaningful information
}
