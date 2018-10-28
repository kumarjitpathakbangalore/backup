import matplotlib.pyplot as plt
import numpy as np

from utils.Utils import stream_counter, stream_init

fig = plt.figure()
ax1 = fig.add_subplot(1,1,1)
x = [0]
y = [0]

def animate(xar,yar):
    ax1.clear()
    ax1.plot(xar, yar)


if __name__ == '__main__':
     page_size = int(input("Enter Page Size: "))

     arr = stream_init(page_size)
     print('Array is',arr)


     fig = plt.figure(1)
     plt.ion()
     ax = fig.add_subplot(111)
     ax.set_title("linear realtime")
     ax.set_xlim(0, 5)
     ax.set_ylim(0, 20)
     line, = ax.plot(x, y, 'ko-')

     while True:
        new_value = int(input("Enter new Value: "))
        arr, page, flag = stream_counter(arr, new_value, page_size)
        print("Array is:",arr)
        print("Page is:",page)
        print("Flag is:",flag)

        old_x=x[len(x)-1]
        x = np.concatenate((line.get_xdata(), [old_x+1]))
        y = np.concatenate((line.get_ydata(), [new_value]))
        print(" X is:",x)
        print(" Y is:", y)

        line.set_data(x, y)
        plt.show()
        plt.pause(1)
        cont = input("Do you want to continue (y/n)")
        if cont.__contains__('n'):
            break
        # ani = animation.FuncAnimation(fig, animate, interval=1000)
        # plt.show()

     print('Goof bye!')


