import matplotlib.pyplot as plt
import pandas as pd
import matplotlib

matplotlib.use('Agg')

# outputDir = 'output/'
params_ = (pd.read_csv('output/time.csv'))
params = params_[['time']]
data = pd.read_csv('output/globalQuantity.csv')

df = pd.concat([params,data],axis=1)

# plt.figure()
df.plot(x='time')
plt.savefig('output/global_quantity.png')
plt.close('all')

# csvを読み込み
# 横軸: 時間
# 縦軸: 大域的な物理量
