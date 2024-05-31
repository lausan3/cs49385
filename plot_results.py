import matplotlib.pyplot as plt
import numpy as np
import math
import sys

g = [ln.rstrip('\n').split(',') for ln in open(sys.argv[1])]
x = [float(e[0]) for e in g]
y = [float(e[1]) for e in g]

fig = plt.figure()
ax = fig.add_subplot(111)
ax.plot(x,y)
plt.xlabel('Number of coin flips per trial')
plt.ylabel('Expected maximum length of a streak of heads')
plt.title('Number of trials per amount of coin flips = 10000')
plt.show()
