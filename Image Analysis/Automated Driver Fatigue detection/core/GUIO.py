from __future__ import print_function,division

import cv2
import numpy as np
import random
import matplotlib.pyplot as plt

if __name__ == '__main__':
    # r = int(input('Enter Color Redness of Graph background:'))
    # g = int(input('Enter Color Greenness of Graph background:'))
    # b = int(input('Enter Color Blueness of Graph background:'))
    # b = int(input('Enter breath of Graph background:'))
    # l = int(input('Enter length of Graph background:'))

    # r = g = b = 255
    # l = b = 500
    #
    # img = np.zeros([l, b, 3])
    # print('Image shape',img.shape)
    # x = np.arange(l)
    #
    # for i in range(1,1000):
    #     img[:, :, 0] = np.ones([l, b]) * r / 255.0
    #     img[:, :, 1] = np.ones([l, b]) * g / 255.0
    #     img[:, :, 2] = np.ones([l, b]) * b / 255.0
    #
    #     newX = random.randrange(1,b)*np.ones([l])
    #     # y2 = random.randrange(1,l)
    #     pts = np.vstack((x, newX)).astype(np.int32).T
    #     cv2.polylines(img, [pts], isClosed=False, color=(255, 0, 0))
    #
    #     # cv2.line(img, (x1[i-1], y1[i-1]), (x2, y2), (255, 0, 0), 5)
    #     # x1.append(x2)
    #     # y1.append(y2)
    #     # cv2.imwrite('color_img.jpg', img)       
    #     cv2.imshow("image", img);
    #     cv2.waitKey(100);
    plt.axis([0, 10, 0, 1])
    plt.ion()

    for i in range(10):
        y = np.random.random()
        plt.scatter(i, y)
        plt.pause(0.5)

    while True:
        plt.pause(0.05)