from __future__ import print_function, with_statement, division

import os

import cv2
import numpy as np

from models.Process import modelLoader, testEyeOnModel, testYawnOnModel # calling .Process in model also calling model loader , testmodel etc we are calling
from parameters.Parameters import TestEnvParameters
from utils.Utils import resizing, stream_counter, \
    stream_init  # probable_mouth # ,eye_circle_detection,mouth_area_calculator


param = TestEnvParameters(propFolder=os.getcwd())

print('Properties have values:',param.__repr__())

# Setting runtime variables
wait_time = param.WAIT_TIME
inter_wait_time = param.INTER_WAIT_TIME
epochs_eye = param.EPOCHS_EYE
epochs_yawn = param.EPOCHS_YAWN

blink_stream = stream_init(param.BLINK_COUNTER_WINDOW,param.COUNTER_INITIAL_VALUE)
# blink_stream = stream_init(30,0)
yawn_stream = stream_init(param.YAWN_COUNTER_WINDOW,param.COUNTER_INITIAL_VALUE)
# yawn_stream = stream_init(90,0)
blink_counter_stream = []
yawn_counter_stream = []

trace = False
show_blink = True

eye_center_range = 5
resize_image_px = 500
font = cv2.FONT_HERSHEY_COMPLEX
gray_thresh_limit = 90

# Making and loading CNN Models
modelLoader(epochs_eye,epochs_yawn)

# Setting HARR Cascades from Opencv Location
haar_path = param.HAAR_PATH

# Loading Basic Haar Cascades
face_cascade = cv2.CascadeClassifier(haar_path.__add__('haarcascade_frontalface_default.xml'))
eye_cascade = cv2.CascadeClassifier(haar_path.__add__('haarcascade_eye.xml'))


# defining video captures
# cap = cv2.VideoCapture('D:\innovation\eye-blink\VID_20170320_165704367.webm')
# cap = cv2.VideoCapture('D:\innovation\eye-blink\VID_SHIVU_2.webm')
# cap = cv2.VideoCapture('/home/jitin/Documents/innovation/innovate/VID_SHIVU_2.webm')
# cap = cv2.VideoCapture('D:\innovation\eye-blink\VID_JITIN.webm')

cap =cv2.VideoCapture(param.VIDEO_PATH)

totalFrames = cap.get(cv2.CAP_PROP_FRAME_COUNT)
fps = cap.get(cv2.CAP_PROP_FPS)
print("Total Frames:", totalFrames, " And FPS:", fps)

# Defining the drowsiness criteria
thresholdBlinkFrames = int(param.SECONDS_TO_BLINK*fps)
thresholdYawnFrames = int(param.SECONDS_TO_YAWN*fps)
print('Threshold for Eye Blink:',thresholdBlinkFrames)
print('Threshold for No of Yawns:',thresholdYawnFrames)
# thresholdBlinkFrames = 2
# thresholdYawnFrames = 5

# def eyeGradientCalculator(image):
#     pass


def detectEyeBlink(image):
    val = testEyeOnModel(np.array(image, dtype='float32'))
    # print("Prediction for eye: ", val)
    print("Prediction for Eye", val[0], 'Is Blink?:',val[0] >= param.PROB_EYE)
    return val[0] >= param.PROB_EYE

# def detectEyePerclose(image):
#     pass


def detectYawn(image):
    val = testYawnOnModel(np.array(image, dtype='float32'))
    print("Prediction for Yawn", val[0], 'Is Yawn?:', val[0] >= param.PROB_YAWN)
    return val[0] >= param.PROB_YAWN

nFrames = 0
blinkCounter = 0
yawnCounter = 0
perclosCounter = 0

while cap.isOpened():
    ret, img = cap.read()
    img = resizing(img, resize_image_px)
    # if not nFrames>1:
    #     print('Frame Size:',(img.shape[0],img.shape[1]))
    gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
    faces = face_cascade.detectMultiScale(gray, 1.3, 5)
    # print('Face for Frame no: ',nFrames,' is: ',faces)
    nFrames += 1
    print('Face Array:', faces)
    for (x, y, w, h) in faces:
        cv2.rectangle(img, (x, y), (x + w, y + h), (255, 0, 0), 2)
        drowsieFalg = False
        # Subsetting face Face from the images
        roi_gray = gray[y:y + h, x:x + w]
        roi_color = img[y:y + h, x:x + w]
        # cv2.imshow('Color Face',roi_color)
        # cv2.imshow('Gray Face', roi_gray)

        # Detecting Eye and blinks
        eyes = eye_cascade.detectMultiScale(roi_gray)
        print('Blink Counter: ', blinkCounter, '\tYawn Counter: ', yawnCounter)

        # if len(eyes) <= 1:
        #     # blinkCounter += 1
        # elif len(eyes) >= 2:
        #     print("Eye Open!")
        #     blinkCounter = 0

        minarea = 1000000000000000
        for (ex, ey, ew, eh) in eyes:
            # print('Center of eye frame is: ',ew/2,',',eh/2)
            if minarea >= ew * eh:
                minarea = ew * eh
        for (ex, ey, ew, eh) in eyes:
            if ew * eh < 2 * minarea:
                # eye_frame_center = (ew / 2, eh / 2)
                # print('Center of eye frame is: ', eye_frame_center)
                cv2.rectangle(roi_color, (ex, ey), (ex + ew, ey + eh), (0, 255, 0), 2)
                # eye_image = roi_gray[ey: ey + eh, ex: ex + ew]
                # eye_circle_detection(eye_image)
                if detectEyeBlink(roi_color[ey: ey + eh, ex: ex + ew]):
                   print('You Blinked!')
                   blinkCounter += 1
                else:
                    # print("You are Awake!")
                    blinkCounter = 0

        if blinkCounter >= thresholdBlinkFrames:
            print("You Blinked Too Much!")
            drowsieFalg = True
            blink_stream, blinkPage, _ = stream_counter(blink_stream, 1, param.BLINK_COUNTER_WINDOW)
            print('Eye Page Value:', sum(blinkPage))
            blink_counter_stream.append(sum(blinkPage))
        else:
            blink_stream, blinkPage, _ = stream_counter(blink_stream, 0, param.BLINK_COUNTER_WINDOW)
            print('Eye Page Value:', sum(blinkPage))
            blink_counter_stream.append(sum(blinkPage))

        if int(nFrames %100) ==0:
            print('Blink Stream:',blink_stream)
            print('Yawn Stream:',yawn_stream)

        if detectYawn(roi_color):
            print('You Are Yawning!')
            yawnCounter += 1

        if yawnCounter >= thresholdYawnFrames:
            yawn_stream, yawnPage , _ = stream_counter(yawn_stream,1,param.YAWN_COUNTER_WINDOW)
            print('Yawn Page Value:', sum(yawnPage))
            yawn_counter_stream.append(sum(yawnPage))
            drowsieFalg = True
        else:
            yawn_stream, yawnPage, _ = stream_counter(yawn_stream, 0, param.YAWN_COUNTER_WINDOW)
            print('Yawn Page Value:', sum(yawnPage))
            # yawn_stream, yawnPage, _ = stream_counter(yawn_stream, 1, 90)
            yawn_counter_stream.append(sum(yawnPage))
            yawnCounter = 0
            drowsieFalg = False

        if drowsieFalg:
            cv2.imshow('You are drowsie!', img)

        cv2.imshow('Video Stream', img)
        cv2.waitKey(inter_wait_time)

k = cv2.waitKey(inter_wait_time) & 0xff

if k == 27:
    cap.release()
    cv2.destroyAllWindows()