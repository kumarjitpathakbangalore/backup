from __future__ import absolute_import
from __future__ import print_function

import numpy as np
from keras.models import Sequential
from keras.layers.core import Dense, Dropout, Activation, Flatten
from keras.layers.convolutional import Conv2D, MaxPooling2D
from keras.optimizers import SGD, Adadelta, Adagrad, RMSprop
from six.moves import cPickle as pickle

# from sklearn import svm
# from keras.utils import np_utils
# import matplotlib.pyplot as plt

np.random.seed(1337)  # for reproducibility

yawnModel = None

def yawnCNNModel(batch_size = 1,epochs = 20, optimizer = 'rmsprop', lr = 'default'):
    pickle_files = ['data/yawnMouths.pickle']
    i = 0
    for pickle_file in pickle_files:
        with open(pickle_file, 'rb') as f:
            save = pickle.load(f)
            if i == 0:
                train_dataset = save['train_dataset']
                train_labels = save['train_labels']
                test_dataset = save['test_dataset']
                test_labels = save['test_labels']
            else:
                print("here")
                train_dataset = np.concatenate((train_dataset, save['train_dataset']))
                train_labels = np.concatenate((train_labels, save['train_labels']))
                test_dataset = np.concatenate((test_dataset, save['test_dataset']))
                test_labels = np.concatenate((test_labels, save['test_labels']))
            del save  # hint to help gc free up memory
        i += 1

    print('Training set', train_dataset.shape, train_labels.shape)
    print('Test set', test_dataset.shape, test_labels.shape)

    # batch_size = 1
    nb_classes = 1
    # epochs = 20

    X_train = train_dataset
    # X_train = X_train.reshape((X_train.shape[0], X_train.shape[3]) + X_train.shape[1:3])
    # X_train = X_train.reshape(X_train.shape[0:3])
    # X_train = X_train.reshape(len(X_train), -1)
    Y_train = train_labels

    X_test = test_dataset
    # X_test = X_test.reshape((X_test.shape[0], X_test.shape[3]) + X_test.shape[1:3])
    # X_test = X_test.reshape(X_test.shape[0:3])
    # X_test = X_test.reshape(len(X_test), -1)
    Y_test = test_labels

    # input image dimensions
    img_samples, img_rows, img_cols, img_channels = X_train.shape

    # convert class vectors to binary class matrices
    # Y_train = np_utils.to_categorical(y_train, nb_classes)
    # Y_test = np_utils.to_categorical(y_test, nb_classes)
    global yawnModel
    yawnModel = Sequential()

    yawnModel.add(Conv2D(32, (3, 3),
                            # padding ='same',
                            data_format="channels_last",
                            input_shape=( img_rows, img_cols,img_channels)))
    yawnModel.add(Activation('relu'))

    yawnModel.add(Conv2D(32, (3, 3)))
    yawnModel.add(Activation('relu'))
    yawnModel.add(MaxPooling2D(pool_size=(2, 2)))
    yawnModel.add(Dropout(0.25))

    yawnModel.add(Conv2D(64, (3, 3)))
    yawnModel.add(Activation('relu'))
    yawnModel.add(MaxPooling2D(pool_size=(2, 2)))
    yawnModel.add(Dropout(0.25))

    yawnModel.add(Flatten())
    yawnModel.add(Dense(512))
    yawnModel.add(Activation('relu'))
    yawnModel.add(Dropout(0.5))
    yawnModel.add(Dense(nb_classes))
    yawnModel.add(Activation('sigmoid'))

    # let's train the yawnModel using SGD + momentum (how original).
    if optimizer == 'sgd':
        print('Optimiser Used is SGD!')
        if lr == 'default':
            lr = 0.01
        elif isinstance(lr, float):
            pass
        optimizer = SGD(lr=lr, decay=1e-6, momentum=0.9, nesterov=True)
    elif optimizer == 'delta':
        print('Optimiser Used is AdaDelta!')
        if lr == 'default':
            lr = 1
        elif isinstance(lr, float):
            pass
        optimizer = Adadelta(lr=lr)
    elif optimizer == 'delta':
        print('Optimiser Used is Adagrad!')
        if lr == 'default':
            lr = 0.01
        elif isinstance(lr, float):
            pass
        optimizer = Adagrad(lr=lr)
    elif optimizer == 'rmsprop':
        print('Optimiser Used is RMSprop!')
        if lr == 'default':
            lr = 0.001
        elif isinstance(lr, float):
            pass
        optimizer = RMSprop(lr=lr)
    else:
        print('Not a right sepcifier! Optimiser Used is SGD!')
        optimizer = RMSprop(lr=0.0005)

    yawnModel.compile(loss='binary_crossentropy', optimizer=optimizer, metrics=['accuracy'])

    yawnModel.fit(X_train, Y_train, batch_size=batch_size, epochs=epochs, verbose=2,
              validation_data=[X_test, Y_test])
    # yawnModel.save('yawnModel.h5')

    score = yawnModel.evaluate(X_test, Y_test, batch_size = 8, verbose=1)

    print('Test score:', score[0])
    print('Test accuracy:', score[1])

    '''cls = svm.LinearSVC()
    cls.fit(X_train, Y_train)
    score = cls.score(X_test, Y_test)'''

    return(yawnModel)


