#!/bin/bash
runs=2
for switch in '' '-j'; do
	for run in `seq $runs`; do
		((mvn clean >&/dev/null); /usr/bin/time --format "%e " bash -c "make $switch mvn >&/dev/null") 2>> timings$switch
	done
done

