import matplotlib.pyplot as plt
import pandas as pd
import numpy as np

# Read data
path = './output/rejection.txt'
data = np.loadtxt(path)

# Create histogram
plt.hist(data, bins=60, color='skyblue', edgecolor='black', density=True)

# Plot PDF
x = np.linspace(0, 1, 10)
plt.plot(x, 3 * x ** 2, label=r'$f(x)=3x^2$', color='darkorange')

# Add labels and title
plt.xlim(0, 1) # Remember to control x-limit!
plt.legend()
plt.xlabel("Value")
plt.ylabel("Density")
plt.show()