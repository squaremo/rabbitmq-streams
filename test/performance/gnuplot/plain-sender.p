# Plot the last sender data file found in the log directory

load "test/performance/gnuplot/common.p"

# Setup our graph
set title "Send Time"
set xlabel "Run number"
set ylabel "Time (ms)"

# Setup the output
set terminal png
set output "tmp/gnuplot/sender.png"

plot "tmp/logs/data_sender1-0.log" using 2:5
