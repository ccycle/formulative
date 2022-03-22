import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import matplotlib

# matplotlib.use('TkAgg')
matplotlib.use('Agg')

# TODO: ラベルからパスを取得してプロットできるようにする
# TODO: Haskell側でplotのみ行うようにする

outputDir = 'output/'

position = pd.read_csv('output/position.csv',names = ["position"])
momentum = pd.read_csv('output/momentum.csv',names = ["momentum"])

# position = np.loadtxt('output/position.csv')
# momentum = np.loadtxt('output/momentum.csv')

df = pd.concat([position,momentum],axis=1)

plt.figure()
df.plot(x="position",y="momentum")
# fig, ax = plt.subplots( nrows=1, ncols=1 )
# plt.ion()
# ax.plot(position,momentum)
# plt.ioff()
plt.savefig('output/phase_space.png')
plt.close('all')
