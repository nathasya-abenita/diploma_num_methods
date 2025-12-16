import matplotlib.pyplot as plt
import pandas as pd
import numpy as np

def fx (x):
    if -1 <= x <= 1:
        return 2 * (1 - x**2)**0.5 / np.pi
    else:
        return 0

# Read data
path = './rejection.txt'
data = np.loadtxt(path)

# Create histogram
plt.hist(data, bins=50, color='skyblue', edgecolor='black', density=True)

# Plot PDF
x = np.linspace(-1, 1, 50)
y = x.copy()
for i in range (len(x)):
    y[i] = fx(x[i])
plt.plot(x, y, label=r'$f(x)$', color='darkorange')

# Add labels and title
plt.xlim(-2, 2) # CONTROL XLIM!!
plt.legend()
plt.xlabel("Value")
plt.ylabel("Density")
plt.savefig('hist.png')