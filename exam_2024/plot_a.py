import matplotlib.pyplot as plt
import pandas as pd

# Read
df_euler = pd.read_csv('./exam_2024/euler.txt', sep='\s+', names=['t', 'x', 'v'], header=None, skiprows=1)

# Plot
plt.plot(df_euler.t, df_euler.x, label='Euler method')
plt.xlabel(r'$t$')
plt.ylabel(r'$x(t)$')
plt.title('Friedmann equation')
plt.legend()
plt.show()
