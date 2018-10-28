from __future__ import absolute_import
from __future__ import print_function
import numpy as np
np.random.seed(1337)  # for reproducibility

from keras.models import Sequential
from keras.layers.core import Dense, Dropout, Activation, Flatten
from keras.layers.convolutional import Conv2D, MaxPooling2D
# from keras.utils import np_utils
from keras.optimizers import SGD, Adadelta, Adagrad, RMSprop

from six.moves import cPickle as pickle

# eyeModel = None

def eyeCNNModel(batch_size = 30,epochs = 12,optimizer = 'sgd',lr = 'default'):
    pickle_files = ['data/open_eyes.pickle', 'data/closed_eyes.pickle']
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
                train_dataset = np.concatenate((train_dataset, save['train_dataset']))
                train_labels = np.concatenate((train_labels, save['train_labels']))
                test_dataset = np.concatenate((test_dataset, save['test_dataset']))
                test_labels = np.concatenate((test_labels, save['test_labels']))
            del save  # hint to help gc free up memory
        i += 1

    print('Training set', train_dataset.shape, train_labels.shape)
    print('Test set', test_dataset.shape, test_labels.shape)

    # batch_size = 30
    nb_classes = 1
    # epochs = 12

    X_train = train_dataset
    print("The original shape of input is :", X_train.shape)
    # X_train= X_train.reshape((X_train.shape[0], X_train.shape[3]) + X_train.shape[1:3])
    # print("The changed shape of input is :", X_train.shape)
    # input image dimensions
    # img_samples, img_channels, img_rows, img_cols = X_train.shape
    img_samples, img_rows, img_cols, img_channels = X_train.shape

    Y_train = train_labels

    X_test = test_dataset
    # X_test= X_test.reshape((X_test.shape[0], X_test.shape[3]) + X_test.shape[1:3])
    Y_test = test_labels

    # print shape of data while eyeModel is building
    print("{1} train samples, {4} channel{0}, {2}x{3}".format("" if X_train.shape[1] == 1 else "s", *X_train.shape))
    print("{1}  test samples, {4} channel{0}, {2}x{3}".format("" if X_test.shape[1] == 1 else "s", *X_test.shape))

    # print(" We have training as: ",X_train.shape)

    # convert class vectors to binary class matrices
    # Y_train = np_utils.to_categorical(y_train, nb_classes)
    # Y_test = np_utils.to_categorical(y_test, nb_classes)

    # global eyeModel
    eyeModel = Sequential()
    # print("\nAdding Level 1")
    # eyeModel.add(Conv2D(32, (3, 3), padding='same', input_shape=(img_samples, img_channels, img_rows, img_cols)))
    eyeModel.add(Conv2D(32, (3, 3),
                     # padding='same',
                     data_format="channels_last",
                     input_shape=(img_rows, img_cols, img_channels)))

    eyeModel.add(Activation('relu'))

    # print("\nAdding Level 2")
    eyeModel.add(Conv2D(24, (3, 3)))
    eyeModel.add(Activation('relu'))
    eyeModel.add(MaxPooling2D(pool_size=(2, 2)))
    eyeModel.add(Dropout(0.25))

    # print("\nAdding Level 3")
    eyeModel.add(Conv2D(64, (3, 3), padding='same'))
    eyeModel.add(Activation('relu'))

    # print("\nAdding Level 4")
    eyeModel.add(Conv2D(64, (3, 3)))
    eyeModel.add(Activation('relu'))
    eyeModel.add(MaxPooling2D(pool_size=(2, 2)))
    eyeModel.add(Dropout(0.25))

    # print("\nAdding Level 5")
    eyeModel.add(Flatten())
    eyeModel.add(Dense(512))
    eyeModel.add(Activation('relu'))
    eyeModel.add(Dropout(0.5))
    eyeModel.add(Dense(nb_classes))
    eyeModel.add(Activation('sigmoid'))

    # let's train the eyeModel using SGD + momentum (how original).
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
        optimizer = SGD(lr=0.01, decay=1e-6, momentum=0.9, nesterov=True)

    eyeModel.compile(loss='binary_crossentropy', optimizer=optimizer, metrics=['accuracy'])

    eyeModel.fit(X_train, Y_train, batch_size=batch_size,
              epochs=epochs, verbose=2, validation_data=(X_test, Y_test))

    score = eyeModel.evaluate(X_test, Y_test, verbose=1)

    print('Test score:', score[0])
    print('Test accuracy:', score[1])
    # print('')
    return(eyeModel)
