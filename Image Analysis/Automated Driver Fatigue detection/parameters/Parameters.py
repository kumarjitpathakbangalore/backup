'''

'''
from __future__ import print_function
import os
from utils.properties import Properties
from utils.propertiesUtils import getPathAtFolder, converter, getPropertiesAtFolder


class TestEnvParameters:

    def __init__(self,propFolder=""):
        try:
            path = getPropertiesAtFolder(propFolder,"test.properties")
        except:
            print("Path not found in local folder. Going to resources!")
            path = getPathAtFolder("resources","test.properties")

        prop = Properties()
        prop.load(open(path))

        # path variables
        # try:
        self.HAAR_PATH = self.tryCatch(prop,'haar_dir', 's', '')
        self.VIDEO_PATH = self.tryCatch(prop,'vid_path', 's', 'device(0)')
    # except:
        #     self.HAAR_PATH = ''
        #     self.VIDEO_PATH = 'device(0)'

        # CNN Model training
        # self.EPOCHS_EYE = converter(prop.getProperty('epochs_eye'),'i')
        # self.EPOCHS_YAWN = converter(prop.getProperty('epochs_yawn'),'i')

        self.EPOCHS_EYE = self.tryCatch(prop, 'epochs_eye','i',5)
        self.EPOCHS_YAWN = self.tryCatch(prop, 'epochs_yawn','i',5)

        # Probability Cutoffs for Blink and Yawn
        # try:
        self.PROB_EYE = self.tryCatch(prop,'prob_eye', 'f', 0.1)
        self.PROB_YAWN = self.tryCatch(prop,'prob_yawn', 'f', 0.1)
        # except:
        #     self.PROB_EYE = 0.1
        #     self.PROB_YAWN = 0.1

        # Wait time for Evaluation
        self.WAIT_TIME = self.tryCatch(prop,'wait_time', 'i', 200)
        self.INTER_WAIT_TIME = self.tryCatch(prop,'inter_wait_time', 'i', 1000)

        # Drowsiness criterion
        # try:
        self.SECONDS_TO_BLINK = self.tryCatch(prop,'seconds_to_blink', 'f', 0.1)
        self.SECONDS_TO_YAWN = self.tryCatch(prop,'seconds_to_yawn', 'f', 1.2)
        # except:
        #     self.SECONDS_TO_BLINK = 0.1
        #     self.SECONDS_TO_YAWN = 1.2

        # try:
        self.BLINK_COUNTER_WINDOW = self.tryCatch(prop,'blink_counter_window', 'i',30)
        self.PERCLOS_COUNTER_WINDOW = self.tryCatch(prop,'perclos_counter_window', 'i', 180)
        self.YAWN_COUNTER_WINDOW = self.tryCatch(prop,'yawn_counter_window', 'i', 120)
        self.COUNTER_INITIAL_VALUE = self.tryCatch(prop,'counter_initial_value', 'i', 0)
        # except:
        #     self.BLINK_COUNTER_WINDOW = 30
        #     self.PERCLOS_COUNTER_WINDOW = 180
        #     self.YAWN_COUNTER_WINDOW = 120
        #     self.COUNTER_INITIAL_VALUE = 0


        # # cross validation and splitting options
        # try:
        #     self.CROSS_VAL = converter(prop.getProperty('cross_validation'),"b")
        #     self.VALIDATE = self.tryCatch(prop,'validation'),"b")
        # except:
        #     self.CROSS_VAL =False
        #     self.VALIDATE=False
        #
        # if self.CROSS_VAL:
        #     try:
        #         self.CROSS_VAL_FOLDS=converter(prop.getProperty('cross_val_folds'),"i")
        #         self.CROSS_VAL_TYPE=converter(prop.getProperty('shuffle_cross_val'),"b")
        #     except:
        #         self.CROSS_VAL_FOLDS=10
        #         self.CROSS_VAL_TYPE=False
        #
        # elif self.VALIDATE:
        #     try:
        #         self.VALIDATE_FRAC=converter(prop.getProperty('validation_fraction'),"f")
        #     except:
        #         self.VALIDATE_FRAC=0.2
        #
        # # preprocessor and ensemble options
        # try:
        #     self.ALLOW_PROCESSING=converter(prop.getProperty('allow_processing'),"b")
        #     self.DO_ENSEMBLE=converter(prop.getProperty('do_ensemble'),"b")
        # except:
        #     self.ALLOW_PROCESSING=self.DO_ENSEMBLE=False
        #
        # if self.DO_ENSEMBLE:
        #     try:
        #         self.ENSEMBLE_TYPE=converter(prop.getProperty('ensemble_type'))
        #     except:
        #         self.ENSEMBLE_TYPE="nn"
        #
        # # Defining Evaluation metrics
        # try:
        #     self.EVAL_METRIC=converter(prop.getProperty('evaluation_metric'))
        # except:
        #     self.EVAL_METRIC="none"
        #
        # # Defining whether hyper parameter tuning is active or not
        # try:
        #     self.ALLOW_HYPERTUNING=converter(prop.getProperty('allow_tuning'),"b")
        # except:
        #     self.ALLOW_HYPERTUNING=False
        #
        # # script generation option
        # try:
        #     self.ALLOW_SCRIPT_GEN=converter(prop.getProperty('allow_script_gen'),"b")
        # except:
        #     self.ALLOW_SCRIPT_GEN=False

        # Logging options
        try:
            self.ALLOW_LOGGING=converter(prop.getProperty('allow_logging'),"b")
        except:
            self.ALLOW_LOGGING=False

        if self.ALLOW_LOGGING:
            try:
                self.LOGGER_LEVEL=converter(prop.getProperty('logging_level'))
            except:
                self.LOGGER_LEVEL="INFO"

    def __repr__(self):
        return [self.VIDEO_PATH, self.HAAR_PATH, self.EPOCHS_YAWN, self.EPOCHS_EYE, self.INTER_WAIT_TIME,
                self.WAIT_TIME, self.PROB_EYE, self.PROB_YAWN, self.SECONDS_TO_BLINK, self.ALLOW_LOGGING]

    def tryCatch(self,prop, value, selector, default):
        try:
            val = converter(prop.getProperty(value),selector)
            if val ==  None:
                raise Exception('Value not found returning default value')
            print('Got Value for', value, 'as', val)
            return val
        except:
            print('Returning default for', value, 'as', default)
            return default


if __name__ == '__main__':
    dirPath=os.getcwd()
    dirPath
    param = TestEnvParameters(propFolder=os.getcwd())
    print('Properties have values:', param.__repr__())
