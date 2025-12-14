import matplotlib.pyplot as plt
import pandas as pd
import numpy as np


'''
first problem
'''

# Read data
path = r'./output/clt.txt'
data = np.loadtxt(path)

mean = np.mean(data)
std = np.std(data)

# Create histogram
plt.hist(data, bins=13, density=False)
plt.suptitle('histogram for sample means')
plt.title(f'mean: {mean:.3f}, std: {std:.3f}')

# Add labels and title
plt.xlim(0.4,0.6)
plt.xlabel("value")
plt.ylabel("frequency")
plt.show()

'''
second plot
'''

# Read data
path = r'./output/position.txt'
data = np.loadtxt(path)
print(data.shape)

time = np.arange(0, 1000, 1)


for i in range (10):
    plt.plot(time, data[:, i])

plt.xlabel('step')
plt.ylabel('position')
plt.title('time series of position for ten different random walks')
plt.show()

'''
third problem
'''

# Read data
path = r'./output/last_position.txt'
data = np.loadtxt(path)
print(data.shape)


# Create histogram
plt.hist(data, bins=200, density=False)
plt.title('histogram for last position of random walks')

# Add labels and title
plt.xlabel("value")
plt.ylabel("frequency")
plt.show()

'''
bonus
'''

# Read data
path = r'./output/mean_positions.txt'
data = np.loadtxt(path)
plt.plot(time, data[:, 1])
plt.xlabel('steps')
plt.ylabel('mean squared displacement (msd)')
plt.title('msd from 1e5 random walks with 1e3 steps')
plt.show()