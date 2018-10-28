'''
These are Dictionary of Utility Functions
'''
from __future__ import print_function,division

import cv2
import numpy as np

# from Extraction import gray_thresh_limit,eye_center_range

# Functions
def resizing(image, pixel_length, maintain_aspect = True, trace = False):
    '''
    This 
    :param image: Image to be Resized
    :param pixel_length: Pixel to be Resized
    :return: Resized image
    '''
    if maintain_aspect:
        r = pixel_length / image.shape[1]
        dim = (pixel_length, int(image.shape[0] * r))
        if trace:
            print("Original image size: ", image.shape)
            print("Maintaining Aspect Ratio Multiplier: ", r)
            print("Expected Resize : ", dim)
    else:
        dim = (pixel_length, pixel_length)
        if trace:
            print("Original image size: ", image.shape)
            print("Expected Resize : ", dim)

    # perform the actual resizing of the image and show it
    resized = cv2.resize(image, dim, interpolation=cv2.INTER_AREA)

    return resized


def probable_mouth(x, y, w, h):
    probableWidth = int(w / 2)
    probableHeigth = int(h / 3)
    shiftX = int((w - probableWidth) / 2)
    shiftY = int(probableHeigth * 2)

    return ((shiftX, shiftY, probableWidth, probableHeigth))


def mouth_area_calculator(img, img_gray, x, y, w, h):
    gray_thresh_limit = 90
    (mx, my, mw, mh) = probable_mouth(x, y, w, h)
    cv2.rectangle(img, (mx, my), (mx + mw, my + mh), (0, 255, 0), 2)
    mouth_img = img_gray[my:my + mh, mx:mx + mw]
    # mouth_img = img[my:my + mh, mx:mx + mw]
    cv2.imshow('Mouth image', mouth_img)

    ret, thresh = cv2.threshold(mouth_img, gray_thresh_limit, 255, cv2.THRESH_BINARY)
    thresh_img = thresh
    image, contours, hierarchy = cv2.findContours(thresh_img, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)
    cv2.drawContours(thresh_img, contours, -1, (255, 0, 0), 3)
    cv2.imshow('thresh image', thresh_img)

    return True

#
# def eye_circle_detection(eye_image, area_ration=0.3, trace=False):
#     '''
#
#     :param eye_image:
#     :param method:
#     :return:
#     '''
#     min_radius = int(eye_image.shape[1] * area_ration)
#     circle_image = eye_image
#     cv2.imshow('Eye Image', eye_image)
#     try:
#         circles = cv2.HoughCircles(circle_image, cv2.HOUGH_GRADIENT, 1, 20,
#                                    param1=2, param2=20, minRadius=min_radius, maxRadius=0)
#         # print("Circle we got: ", circles)
#         circles = np.uint16(np.around(circles))
#         area = 0
#         if len(circles[0, :]) == 1:
#             if trace:
#                 print('Only one circle region found! : ', circles[0, :][0])
#                 (cx, cy, cradius) = circles[0, :][0]
#                 cv2.circle(circle_image, (cx, cy), cradius, (255, 0, 0), 2)
#                 cv2.circle(circle_image, (cx, cy), 2, (255, 0, 0), 3)
#                 area = cv2.contourArea(circles)
#         else:
#             for (cx, cy, cradius) in circles[0,]:
#                 if cx in range(int(eye_frame_center[0] - eye_center_range),
#                                int(eye_frame_center[0] + eye_center_range + 1)):
#                     if trace:
#                         print('Circle found as', (cx, cy, cradius))
#                         print(' This circle is in range!')
#                         cv2.circle(circle_image, (cx, cy), cradius, (255, 0, 0), 2)
#                         cv2.circle(circle_image, (cx, cy), 2, (255, 0, 0), 3)
#                         area += cv2.contourArea((cx, cy, cradius))
#         return area
#     except Exception:
#         print("Circle not found")
#

# Least Recently Used Page Replacement Algorithm
def stream_init(page_size, init_value = 0):
    page = []
    for i in range(page_size):
        page.append(init_value)
    return page

def stream_counter(array, new_value, page_size, init_value = 0):
    '''
    
    :param array: A array of values for blink, perclose and yawn 
    :param new_value: New Value added to be added array
    :param page_size: The interval for which counter should work
    :param init_value: for initilising the array
    :return: 
    '''

    n = len(array)
    if n == 0:
        array = lru_init(page_size, init_value)
        init_flag = True
    elif n <= 2*page_size-1:
        init_flag = True
    elif n >= 2*page_size:
        init_flag = False

    present_array = array
    array.append(new_value)
    page = array[(n-page_size+1):(n+1)]

    # if len(array) == (len(present_array)+1):
    return (array, page, init_flag)