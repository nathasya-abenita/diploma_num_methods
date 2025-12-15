import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

# Define data paths
inp_path = r'./data_sample/input_data.dat'
out_path = r'./output/output_dat.dat'

# Read data
df_inp = pd.read_csv(inp_path, sep='\s+', names=['t', 'm'], header=None)
df_out = pd.read_csv(out_path, delim_whitespace=True, names=['t', 'm'], header=None)

# Define plot
fig, axs = plt.subplots(2, 1, sharex=True, figsize=(9,6))

# Plot first data
axs[0].plot(df_inp.t, df_inp.m, 'o', label='Measurement')
axs[0].plot(df_out.t, df_out.m, 'o', label='Interpolated points')
axs[0].set_ylabel('m')
axs[0].set_title('Input: input_data.dat')


# Define data paths
inp_path = r'./data_sample/input_data1.dat'
out_path = r'./output/output_dat1.dat'

# Read data
df_inp = pd.read_csv(inp_path, delim_whitespace=True, names=['t', 'm'], header=None)
df_out = pd.read_csv(out_path, delim_whitespace=True, names=['t', 'm'], header=None)

# Plot second data
axs[1].plot(df_inp.t, df_inp.m, 'o', label='Measurement')
axs[1].plot(df_out.t, df_out.m, 'o', label='Interpolated points')
axs[1].set_xlabel('t'); axs[0].set_ylabel('m')
axs[1].set_ylabel('m')
axs[1].set_title('Input: input_data1.dat')
axs[1].legend()

# Save picture
plt.show()