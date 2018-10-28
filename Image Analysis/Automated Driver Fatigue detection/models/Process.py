'''
This file is for Loading/Training the CNN models and Testing/Predicting CNN on the Input Images. 
'''

import numpy as np
from models.YawnCNN import yawnCNNModel
from models.EyesCNN import eyeCNNModel

from utils.Utils import resizing

eyeModel = None
yawnModel = None

def modelLoader(epochs_eye = 12, epochs_yawn = 20):

    global eyeModel
    eyeModel = eyeCNNModel(epochs=epochs_eye)
    print("Eye Model Loaded as",eyeModel.summary())

    global yawnModel
    yawnModel = yawnCNNModel(epochs=epochs_yawn)
    print("Yawn Model Loaded as", yawnModel.summary())

def testEyeOnModel(input_image):
    X_pred = np.ndarray([1, 24, 24, 1], dtype='float32')
    im= np.dot(np.array(input_image, dtype='float32'), [[0.2989], [0.5870], [0.1140]]) / 255
    X_pred[0, :, :, 0] = resizing(im, 24, maintain_aspect = False)
    prob_eye = eyeModel.predict(X_pred)
    return(prob_eye)

def testYawnOnModel(input_image):
    X_pred = np.ndarray([1, 60, 60, 1], dtype='float32')
    im= np.dot(np.array(input_image, dtype='float32'), [[0.2989], [0.5870], [0.1140]]) / 255
    X_pred[0, :, :, 0] = resizing(im, 60, maintain_aspect = False)
    prob_yawn = yawnModel.predict(X_pred)
    return(prob_yawn)

if __name__ == '__main__':

    # eyeModel = eyeCNNModel()
    # yawnModel = yawnCNNModel()

    modelLoader()
