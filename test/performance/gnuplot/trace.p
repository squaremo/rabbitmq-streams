# Plot the timings data file found in the log directory

load "test/performance/gnuplot/common.p"

set xlabel "Run number"
set ylabel "Time (ms)"

# Setup the output
set terminal png
set term png size 1200, 800
set output "tmp/gnuplot/trace.png"

set multiplot layout 2,3

set title "Acquire mutex"
plot "tmp/logs/timings" using 0:1
set title "Handle message"
plot "tmp/logs/timings" using 0:2
set title "Ack message"
plot "tmp/logs/timings" using 0:3
set title "Commit message"
plot "tmp/logs/timings" using 0:4
set title "Release mutex"
plot "tmp/logs/timings" using 0:5

unset multiplot
