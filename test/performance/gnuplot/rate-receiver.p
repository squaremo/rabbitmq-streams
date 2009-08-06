# Plot the average TPS for receiver data. Requires data to have been generated
# using the awk script as per Makefile.test

load "test/performance/gnuplot/common.p"

# Setup our graph
set title "Receiver TPS"
set xlabel "Seconds"
set ylabel "No. of tx"

# Setup the output
set terminal png
set output "tmp/gnuplot/rate-receiver.png"

plot "tmp/gnuplot/receiver-rate.log" using 1:2

