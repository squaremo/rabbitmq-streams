import matplotlib
import numpy as np
import optparse
import sys

def calculateRate(data):
	count = 0
	time = data[0]
	rate = []

	for value in data:
		count = count + 1
		if value > time + 1000:
			rate.append(count)
			time = value
			count = 0

	return rate

def main():
	parser = optparse.OptionParser(usage="%prog [options] sender_data_file receiver_data_file")
	parser.add_option('--show', action='store_true', default=False, help='show graphs in viewer')
	parser.add_option('-o', action='store', default='plot.png', dest='output_file', help='store plot in file')
	(options, args) = parser.parse_args()

	if len(args) < 2:
		parser.print_help()
		sys.exit(1)

	if options.show == False:
		matplotlib.use('Agg')
	import matplotlib.pyplot as plt		
	
	a, b, rate = np.loadtxt(args[0], skiprows=1, delimiter=',', unpack = True, usecols=(1, 6, 3))
	c, d = np.loadtxt(args[1], skiprows=1, delimiter=',', unpack = True, usecols=(1, 11))

	plt.figure(1)
	plt.subplot(311)
	plt.plot(a, b, 'r.')
	plt.ylabel('Time (ms)')

	plt.subplot(312)
	plt.ylabel('Time (ms)')
	plt.plot(c, d, 'b.')

	plt.subplot(313)
	plt.ylabel("Tx/s")
	plt.plot(calculateRate(rate), 'g.')

	if options.show:
		plt.show()
	else:
		plt.savefig(options.output_file)
	
if __name__ == "__main__":
    main()



