import matplotlib.pyplot as plt
import pandas as pd

# Read
df_euler = pd.read_csv('./output/euler1.txt', sep='\s+', names=['t', 'x', 'v'], header=None, skiprows=1)
df_verlet = pd.read_csv('./output/verlet1.txt', sep='\s+', names=['t', 'x', 'v'], header=None, skiprows=1)

# Plot
plt.plot(df_euler.t, df_euler.x, label='Euler method')
plt.plot(df_verlet.t, df_verlet.x, label='Verlet method')
plt.xlabel(r'$t$')
plt.ylabel(r'$x(t)$')
plt.title('Solving a frictionless physical pendulum')
plt.legend()
plt.show()
