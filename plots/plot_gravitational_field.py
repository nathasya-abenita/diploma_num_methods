import matplotlib.pyplot as plt
import pandas as pd

# Read
df_euler = pd.read_csv('./output/euler.txt', sep='\s+', names=['t', 'x1', 'x2', 'y1', 'y2'], header=None, skiprows=1)
df_verlet = pd.read_csv('./output/verlet.txt', sep='\s+', names=['t', 'x1', 'x2', 'y1', 'y2'], header=None, skiprows=1)

# Plot
plt.plot(df_euler.x1, df_euler.x2, label='Euler method')
plt.plot(df_verlet.x1, df_verlet.x2, label='Verlet method')
plt.xlabel(r'$x_1(t)$')
plt.ylabel(r'$x_2(t)$')
plt.title('A point mass in gravitational field')
plt.legend()
plt.show()
