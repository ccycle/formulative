import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

x_data = []
y_data = []

fig, ax = plt.subplots()
(line,) = ax.plot([], [])
title = ax.set_title(None, fontsize=15)  # タイトルを追加


def gen_function():
    """
    ジェネレーター関数
    """
    y = [0] * 30
    # yリストの10, 11, 12, 13, 14, 15, 16番目のデータを置き換える
    y[10], y[11], y[12], y[13], y[14], y[15], y[16] = (
        0.05,
        -0.05,
        0.05,
        -0.05,
        0.05,
        -0.05,
        0.4,
    )
    x = range(len(y))

    for _x, _y in zip(x, y):
        yield [_x, _y]


def init():
    """
    初期化関数
    """
    ax.set_xlim(0, 30)
    ax.set_ylim(-1, 1)
    del (x_data[:], y_data[:])
    title.set_text(None)  # タイトルを初期化

    return (line,)


def plot_func(frame):
    """
    frameにはジェネレーター関数の要素が代入される
    frame: [_x, _y]
    """
    print(frame)
    print("")
    x_data.append(frame[0])
    y_data.append(frame[1])
    line.set_data(x_data, y_data)
    title.set_text("FuncAnimation: {}".format(frame[0]))  # 追加

    return (line,)


anim = FuncAnimation(
    fig=fig,
    func=plot_func,
    frames=gen_function,
    init_func=init,
    interval=100,
    repeat=False,
)

plt.show()
