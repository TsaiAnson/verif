import random, sys

def randomCAMIOIn(count):
	final_string = ""
	for _ in range(int(count)):
		# 1 bit en,we 8 bits for kR,kW,dW and 4 bits for NullTr
		rand_int = random.getrandbits(30)
		if (not rand_int%16):
			final_string += "new CAMIOInTrNull(),\n"
		else:
			en = "true" if rand_int >> 29 else "false"
			we = "true" if (rand_int >> 28) & 1 else "false"
			kR = (rand_int >> 20) & 255
			kW = (rand_int >> 12) & 255
			dW = (rand_int >> 4) & 255

			final_string += "CAMIOInTr({},{},{},{},{}),\n".format(en, we, kR, kW, dW)

	print(final_string)

if __name__ == '__main__':
   randomCAMIOIn(sys.argv[1])