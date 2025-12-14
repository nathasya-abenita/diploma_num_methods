import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

# Define data paths
path = r'./output/fit_sun.txt'
data_path = r'./data_sample/sun_data.txt'

# Read data
df = pd.read_csv(path, sep='\s+', names=['b1', 'b2', 'se1', 'se2'], header=None, skiprows=1)
df_data = pd.read_csv(data_path, sep='\s+', names=['lamb', 'B'], header=None, skiprows=2)

print(df_data)
b1, b2 = df.b1.values[-1], df.b2.values[-1]
print(b1, b2)

# Create a color array based on row index or another column
colors = np.arange(len(df))  # or df['some_column'] for custom coloring

# Fit parameters update
fig, axs = plt.subplots(1, 2, width_ratios=[1, 2], figsize=(15, 5))
sc = axs[0].scatter(df['b1'], df['b2'], c=colors, cmap='viridis')  # Try 'plasma', 'coolwarm', etc.
cb = fig.colorbar(sc, ax=axs[0], label='Iteration number', shrink=0.6)
cb.set_ticks([colors.min(), colors.max()])  # only min and max ticks
axs[0].set_xlabel(r'$\beta_1$')
axs[0].set_ylabel(r'$\beta_2$')
axs[0].set_title('Fit parameters values \nover the fitting process')

# Function
def fit (x, b1, b2):
    return b1 / (x**5 * (np.exp(b2/x) - 1))

fit_list = fit(df_data.lamb.values, b1, b2)
print(fit_list)
# Data
axs[1].scatter(df_data.lamb, df_data.B, c='gray', alpha=0.2, label='Observation')
axs[1].plot(df_data.lamb, fit_list, label='Best fit', color='tab:red', linewidth=3)
axs[1].set_ylabel(r'$B_\lambda$ ($W \cdot m^{-2} \cdot nm^{-1}$)')
axs[1].set_xlabel(r'$\lambda$ (microns)')
axs[1].legend()
axs[1].set_title('Comparison between observation \nand fit result')

# Save picture
plt.show()