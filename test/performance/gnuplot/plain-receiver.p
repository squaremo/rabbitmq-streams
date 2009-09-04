# Plot the last receiver data file found in the log directory

load "test/performance/gnuplot/common.p"

# Setup our graph
set title "Delivery Time"
set xlabel "Run number"
set ylabel "Time (ms)"

# Setup the output
set terminal png
set output "tmp/gnuplot/receiver.png"

plot "tmp/logs/data_receiver-0.log" using 2:8

