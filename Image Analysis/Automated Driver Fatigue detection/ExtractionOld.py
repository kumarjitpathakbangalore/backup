from __future__ import print_function, with_statement, division
import numpy as np
import cv2

# Setting runtime variables
wait_time = 100
inter_wait_time = 200
eye_center_range = 5
resize_image_px = 300
font = cv2.FONT_HERSHEY_COMPLEX
gray_thresh_limit = 90

trace = False
show_blink = True
secondsToCheck = 0.1

# setiing harr cascades
face_cascade = cv2.CascadeClassifier('C:\opencv\sources\data\haarcascades\haarcascade_frontalface_default.xml')
eye_cascade = cv2.CascadeClassifier('C:\opencv\sources\data\haarcascades\haarcascade_eye.xml')
# smile_cascade = cv2.CascadeClassifier('C:\opencv\sources\data\haarcascades\haarcascade_smile.xml')
smile_cascade = cv2.CascadeClassifier('D:\innovation\test_opencv\test_opencv\haar\mouth.xml')

# defining video captures
# cap = cv2.VideoCapture('D:\innovation\eye-blink\VID_20170320_165704367.webm')
# cap = cv2.VideoCapture('D:\innovation\eye-blink\VID_SHIVU_2.webm')
cap = cv2.VideoCapture('D:\innovation\eye-blink\VID_SHIVU_2.webm')
# cap = cv2.VideoCapture('D:\innovation\eye-blink\VID_JITIN.webm')

totalFrames = cap.get(cv2.CAP_PROP_FRAME_COUNT)
fps = cap.get(cv2.CAP_PROP_FPS)
print("Total Frames:", totalFrames, " And FPS:", fps)

# Defining the drowsiness criteria
# thresholdBlinkFrames = int(secondsToCheck*fps)
thresholdBlinkFrames = 2


# Functions
def resizing(image, pixel_length, trace=False):
    '''
    This
    :param image: Image to be Resized
    :param pixel_length: Pixel to be Resized
    :return: Resized image
    '''
    r = pixel_length / image.shape[1]
    dim = (pixel_length, int(image.shape[0] * r))

    # perform the actual resizing of the image and show it
    resized = cv2.resize(image, dim, interpolation=cv2.INTER_AREA)
    if trace:
        print("Original image size: ", image.shape)
        print("Maintaining Aspect Ratio Multiplier: ", r)
        print("Expected Resize : ", dim)

    return resized


def probable_mouth(x, y, w, h):
    probableWidth = int(w / 2)
    probableHeigth = int(h / 3)
    shiftX = int((w - probableWidth) / 2)
    shiftY = int(probableHeigth * 2)

    return ((shiftX, shiftY, probableWidth, probableHeigth))


def mouth_area_calculator(img, img_gray, x, y, w, h):
    (mx, my, mw, mh) = probable_mouth(x, y, w, h)
    cv2.rectangle(img, (mx, my), (mx + mw, my + mh), (0, 255, 0), 2)
    mouth_img = img_gray[my:my + mh, mx:mx + mw]
    # mouth_img = img[my:my + mh, mx:mx + mw]
    cv2.imshow('Mouth image', mouth_img)

    ret, thresh = cv2.threshold(mouth_img, gray_thresh_limit, 255, cv2.THRESH_BINARY)
    # threshAda = cv2.adaptiveThreshold(eye_image, 255, cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY, 7, 2)
    # threshAda3 = cv2.adaptiveThreshold(eye_image, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY, 5, 2)
    thresh_img = thresh

    # (cx, cy), radius = cv2.minEnclosingCircle(thresh_img)
    # (cx, cy), radius = cv2.HoughCircles(mouth_img, cv2.HOUGH_GRADIENT, 1, 20,
    #                            param1=2, param2=20, minRadius=0, maxRadius=0)
    # cv2.circle(img, (cx, cy), radius, (255, 0, 0), 3)
    # ellipse = cv2.fitEllipse(thresh_img)
    # cv2.ellipse(img, ellipse, (0, 255, 0), 2)
    # print('Mouth Ellipse',ellipse)
    # if (mw*mh)

    image, contours, hierarchy = cv2.findContours(thresh_img, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)
    cv2.drawContours(thresh_img, contours, -1, (255, 0, 0), 3)
    cv2.imshow('thresh image', thresh_img)

    return True


def eye_circle_detection(eye_image, area_ration=0.3, trace=False):
    '''

    :param eye_image:
    :param method:
    :return:
    '''
    min_radius = int(eye_image.shape[1] * area_ration)
    circle_image = eye_image
    cv2.imshow('Eye Image', eye_image)
    try:
        circles = cv2.HoughCircles(circle_image, cv2.HOUGH_GRADIENT, 1, 20,
                                   param1=2, param2=20, minRadius=min_radius, maxRadius=0)
        # print("Circle we got: ", circles)
        circles = np.uint16(np.around(circles))
        area = 0
        if len(circles[0, :]) == 1:
            if trace:
                print('Only one circle region found! : ', circles[0, :][0])
                (cx, cy, cradius) = circles[0, :][0]
                cv2.circle(circle_image, (cx, cy), cradius, (255, 0, 0), 2)
                cv2.circle(circle_image, (cx, cy), 2, (255, 0, 0), 3)
                area = cv2.contourArea(circles)
        else:
            for (cx, cy, cradius) in circles[0,]:
                if cx in range(int(eye_frame_center[0] - eye_center_range),
                               int(eye_frame_center[0] + eye_center_range + 1)):
                    if trace:
                        print('Circle found as', (cx, cy, cradius))
                        print(' This circle is in range!')
                        cv2.circle(circle_image, (cx, cy), cradius, (255, 0, 0), 2)
                        cv2.circle(circle_image, (cx, cy), 2, (255, 0, 0), 3)
                        area += cv2.contourArea((cx, cy, cradius))
        return area
    except Exception:
        print("Circle not found")


def detectEyeBlink(image):
    pass


def detectEyePerclose(image):
    pass


def detectYawn(image):
    pass


vec = []
tvec = []
rawVec = []
ret = False
nFrames = 0
blinkCounter = 0

while (cap.isOpened()):
    ret, img = cap.read()
    img = resizing(img, resize_image_px)
    if not nFrames > 1:
        print('Frame Size:', (img.shape[0], img.shape[1]))
    gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
    faces = face_cascade.detectMultiScale(gray, 1.3, 5)
    # print('Face for Frame no: ',nFrames,' is: ',faces)
    nFrames += 1
    print('Face Array:', faces)
    for (x, y, w, h) in faces:
        cv2.rectangle(img, (x, y), (x + w, y + h), (255, 0, 0), 2)
        # cv2.rectangle(gray, (x, y), (x + w, y + h), (255, 0, 0), 2)

        roi_gray = gray[y:y + h, x:x + w]
        roi_color = img[y:y + h, x:x + w]

        cv2.imshow('Color Face', roi_color)
        cv2.imshow('Gray Face', roi_gray)

        # Detecting Eye and blinks
        eyes = eye_cascade.detectMultiScale(roi_gray)
        # print('Eye contour got : ', len(eyes))
        print('Blink Counter: ', blinkCounter)
        if len(eyes) <= 1:
            blinkCounter += 1
        elif len(eyes) >= 2:
            print("Eye Open!")
            blinkCounter = 0
        if blinkCounter >= thresholdBlinkFrames:
            print("Eye Blinked!")
            cv2.imshow('You are drowsie!', img)
        minarea = 1000000000000000
        for (ex, ey, ew, eh) in eyes:
            # print('Center of eye frame is: ',ew/2,',',eh/2)
            if minarea >= ew * eh:
                minarea = ew * eh
        for (ex, ey, ew, eh) in eyes:
            if ew * eh < 2 * minarea:
                eye_frame_center = (ew / 2, eh / 2)
                # print('Center of eye frame is: ', eye_frame_center)
                cv2.rectangle(roi_color, (ex, ey), (ex + ew, ey + eh), (0, 255, 0), 2)
                eye_image = roi_gray[ey: ey + eh, ex: ex + ew]
                eye_circle_detection(eye_image)

        # Detecting the mouth and ellipse inside
        try:
            # mouth = smile_cascade.detectMultiScale(roi_gray)
            # (mx, my, mw, mh) = probable_mouth(x,y,w,h,)
            # cv2.rectangle(roi_color, (mx, my), (mx + mw, my + mh), (0, 255, 0), 2)
            # print('Mouth',(mx, my, mw, mh))
            mouth_area_calculator(roi_color, roi_gray, x, y, w, h)
            print('Mouth Found')
        except Exception:
            print("Mouth not found")
        # for (mx, my, mw, mh) in mouth:
        #     cv2.rectangle(roi_color, (mx, my), (mx + mw, my + eh), (0, 255, 0), 2)


        cv2.imshow('Normal Video', img)
        # cv2.imshow('Grayscaled Video',gray)
        T = "is this working"
        cv2.putText(img, "Hello World!", (x, y), font, 80, (255, 255, 255), 80, cv2.LINE_AA)
        # if not type(T) is str:
        #     try:
        #         cv2.putText(img, str(T), (10, 20 ), font, 1, (0, 255, 0), 10, cv2.LINE_AA)
        #     except Exception, e:
        #         print(e)

        cv2.waitKey(inter_wait_time)

k = cv2.waitKey(inter_wait_time) & 0xff

if k == 27:
    cap.release()
    cv2.destroyAllWindows()