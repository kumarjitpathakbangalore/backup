from __future__ import print_function, with_statement
import numpy as np
import cv2

wait_time = 5000
eye_center_range = 10

face_cascade = cv2.CascadeClassifier('C:\opencv\sources\data\haarcascades\haarcascade_frontalface_default.xml')
eye_cascade = cv2.CascadeClassifier('C:\opencv\sources\data\haarcascades\haarcascade_eye.xml')
# smile_cascade = cv2.CascadeClassifier('C:\opencv\sources\data\haarcascades\haarcascade_smile.xml')

cap = cv2.VideoCapture('D:\innovation\eye-blink\VID_SHIVU_1.webm')
totalFrames = cap.get(cv2.CAP_PROP_FRAME_COUNT)
fps = cap.get(cv2.CAP_PROP_FPS)
print("Total Frames:", totalFrames)
print("And FPS:",fps)

ret, img = cap.read()
gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
faces = face_cascade.detectMultiScale(gray, 1.3, 5)

for (x, y, w, h) in faces:
    cv2.rectangle(img, (x, y), (x + w, y + h), (255, 0, 0), 2)
    roi_gray = gray[y:y + h, x:x + w]
    roi_color = img[y:y + h, x:x + w]
    eyes = eye_cascade.detectMultiScale(roi_gray)
    print('Eye contour got : ',len(eyes))
    minarea = 1000000000000000
    for (ex, ey, ew, eh) in eyes:
        # print('Center of eye frame is: ',ew/2,',',eh/2)
        if minarea >= ew*eh :
            minarea = ew*eh
    for (ex, ey, ew, eh) in eyes:
        if ew*eh < 2*minarea:
            eye_frame_center = (ew / 2, eh / 2)
            print('Center of eye frame is: ', eye_frame_center)

            cv2.rectangle(roi_color, (ex, ey), (ex + ew, ey + eh), (0, 255, 0), 2)
            eye_image = roi_gray[ey: ey+eh,ex: ex+ew]
            ret, thresh = cv2.threshold(eye_image, 0,255,cv2.THRESH_BINARY+cv2.THRESH_OTSU)
            threshAda = cv2.adaptiveThreshold(eye_image, 255, cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY, 7, 2)
            # threshAda2 = cv2.adaptiveThreshold(eye_image, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY, 11, 2)
            threshAda3 = cv2.adaptiveThreshold(eye_image, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY , 5, 2)
            blur = cv2.GaussianBlur(eye_image, (5, 5), 0)
            # ret3, threshOtsu = cv2.threshold(blur, 0, 255, cv2.THRESH_BINARY + cv2.THRESH_OTSU)
            # threshEdges = cv2.Canny(eye_image,10,300)
            circle_image = eye_image
            circles = cv2.HoughCircles(circle_image, cv2.HOUGH_GRADIENT, 1, 20,
                                       param1=10, param2=60, minRadius=10, maxRadius=0)
            circles = np.uint16(np.around(circles))
            if len(circles[0, :]) == 1:
                print('Only one circle region found! : ',circles[0, :][0])
                (cx, cy, cradius) = circles[0, :][0]
                cv2.circle(circle_image, (cx, cy), cradius, (0, 255, 0), 2)
                cv2.circle(circle_image, (cx, cy), 2, (0, 0, 255), 3)
            else:
                for (cx,cy,cradius) in circles[0, ]:
                    print('Circle found as', (cx,cy,cradius))
                    if cx in range((eye_frame_center[0]-eye_center_range),(eye_frame_center[0]+eye_center_range+1)):
                        print(' This circle is in range!')
                        # draw the outer circle
                        cv2.circle(circle_image, (cx, cy), cradius, (0, 255, 0), 2)
                        # draw the center of the circle
                        cv2.circle(circle_image, (cx, cy), 2, (0, 0, 255), 3)

            # im2, contours, hierarchy = cv2.findContours(eye_image, 1, 2)
            # print('Threshold: ',thresh)
            # print('Contour: ',len(contours))
            # for cnt in contours:
            # #     # print('cnt',cnt)
            #     try:
            #         print('Doing Circle Fit')
            #         # ellipse = cv2.fitEllipse(cnt)
            #         # im = cv2.ellipse(threshAda2, ellipse, (0, 255, 0), 2)
            #         # (x, y), radius = cv2.minEnclosingCircle(cnt)
            #         # center = (int(x), int(y))
            #         # radius = int(radius)
            #         # cv2.circle(eye_image, center, radius, (0, 255, 0), 2)
            #         circles = cv2.HoughCircles(eye_image, cv2.HOUGH_GRADIENT, 1, 20,
            #                                    param1=10, param2=10, minRadius=0, maxRadius=0)
            #         circles = np.uint16(np.around(circles))
            #         for i in circles[0, :]:
            #             print('Circle found as',i)
            #             # draw the outer circle
            #             cv2.circle(eye_image, (i[0], i[1]), i[2], (0, 255, 0), 2)
            #             # draw the center of the circle
            #             cv2.circle(eye_image, (i[0], i[1]), 2, (0, 0, 255), 3)
            #
            #     except:
            #     try:
            #         # print('Doing Convex Hull Fit')
            #         # hull = cv2.convexHull(cnt, returnPoints=False)
            #         # defects = cv2.convexityDefects(cnt, hull)
            #         #
            #         # for i in range(defects.shape[0]):
            #         #     s, e, f, d = defects[i, 0]
            #         #     start = tuple(cnt[s][0])
            #         #     end = tuple(cnt[e][0])
            #         #     far = tuple(cnt[f][0])
            #         #     cv2.line(eye_image, start, end, [0, 255, 0], 2)
            #         #     cv2.circle(eye_image, far, 5, [0, 0, 255], -1)
            #
            #         epsilon = 0.1 * cv2.arcLength(cnt, True)
            #         approx = cv2.approxPolyDP(cnt, epsilon, True)
            #         print(approx)
            #     except:
            #         print('Nothing is possible for this eye!')
            cv2.imshow('Eye Area',eye_image)
            cv2.waitKey(wait_time)
            cv2.imshow('Circle Eye Area', circle_image)
            cv2.waitKey(wait_time)
            cv2.imshow('Smoothened Eye Area', blur )
            cv2.waitKey(wait_time)
            cv2.imshow('Threshold Eye Area', threshAda)
            cv2.waitKey(wait_time)
            # cv2.imshow('Threshold Eye Area', threshAda2)
            # cv2.waitKey(wait_time)
            # cv2.imshow('Threshold Eye Area', threshAda3)
            # cv2.waitKey(wait_time)
            # cv2.imshow('Threshold Eye Area', threshEdges)
            # cv2.waitKey(wait_time)
    # smile = smile_cascade.detectMultiScale(roi_gray)
    # print('Smile contour got : ', len(smile))
    # for (sx,sy,sw,sh) in smile:
    #     cv2.rectangle(roi_color, (sx, sy), (sx + sw, sy + sh), (0, 0, 255), 2)


cv2.imshow('img', img)
k = cv2.waitKey(wait_time) & 0xff

if k == 27:
    cap.release()
    cv2.destroyAllWindows()