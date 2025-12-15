import matplotlib.pyplot as plt
import pandas as pd
import numpy as np

# Read data
path = './exam_2024/rejection.txt'
data = np.loadtxt(path)

# Create histogram
plt.hist(data, bins=150, color='skyblue', edgecolor='black', density=True)

# Plot PDF
x = np.linspace(0, 10, 50)
plt.plot(x, np.exp(-1 * x), label=r'$f(x)=e^{-x}$', color='darkorange')

# Add labels and title
plt.xlim(0, 10) # CONTROL XLIM!!
plt.legend()
plt.xlabel("Value")
plt.ylabel("Density")
plt.show()