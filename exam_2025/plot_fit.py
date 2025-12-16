import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

# Define data paths
data_path = r'./C14.txt'

# Read data

df_data = pd.read_csv(data_path, sep='\s+', names=['t', 'n'], header=None, skiprows=1)

print(df_data)

# Function
def fit (x, b1, b2):
    return np.exp(b1 + b2 * x)

def fit_basic(t):
    a = 0.48159
    tau = 8284.95605
    return a * np.exp(-t/tau)

b1 = -0.73067
b2 = -0.00012
fit_list = fit(df_data.t.values, b1, b2)

# Data
fig, ax = plt.subplots()
ax.scatter(df_data.t, df_data.n, c='gray', alpha=0.2, label='Observation')
ax.plot(df_data.t, fit_list, label='Linear fit', color='tab:red', linewidth=3)
ax.plot(df_data.t, fit_basic(df_data.t.values), '--', label='Linear fit', color='k', linewidth=3)
ax.set_ylabel(r'$N(t)$')
ax.set_xlabel(r'$t$')
ax.legend()

# Save picture
plt.savefig('fit.png')