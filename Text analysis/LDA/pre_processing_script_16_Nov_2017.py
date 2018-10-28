# -*- coding: utf-8 -*-
"""
Created on Tue Nov 07 10:57:23 2017

@author: inmyelipet
"""

# -*- coding: utf-8 -*-
"""
Created on Thu Oct 26 12:03:52 2017

@author: inmyelipet
"""

# -*- coding: utf-8 -*-
"""
Created on Mon Oct 23 15:44:39 2017

@author: inmyelipet
"""

import os
import pandas as pd
import seaborn as sns
import numpy as np

#returns current working directory
os.getcwd()
#changes working directory
os.chdir("E:/dat01")


from collections import defaultdict
import csv
import cPickle
import numpy as np 
import scipy as sp 
import matplotlib as mpl 
import matplotlib.cm as cm 
import matplotlib.pyplot as plt 
import pandas as pd 
import nltk
import re
import csv
from nltk.corpus import stopwords
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.naive_bayes import MultinomialNB
from sklearn.naive_bayes import GaussianNB
from sklearn.metrics import accuracy_score
from sklearn.cross_validation import train_test_split
from sklearn.linear_model import SGDClassifier
from scipy.sparse import hstack
from sklearn.metrics import confusion_matrix
from time import time



pd.set_option('display.width', 500)
pd.set_option('display.max_columns', 100)
pd.set_option('display.notebook_repr_html', True)


os.chdir("E:/dat01")
stop = set(stopwords.words('english'))
def clean(doc):
    doc = " ".join([i.replace('*', '') for i in doc.lower().split()])
    doc = " ".join([i.replace(':', ' ') for i in doc.split()])
    doc = " ".join([i.replace('.', ' ') for i in doc.split()])
    doc = " ".join([i.replace('=', '') for i in doc.split()])
    doc = " ".join([i.replace('/', ' ') for i in doc.split()])
    doc = " ".join([i.replace(')', ' ') for i in doc.split()])
    doc = " ".join([i.replace('(', ' ') for i in doc.split()])
    doc = " ".join([i.replace('"', ' ') for i in doc.split()])
    doc = " ".join([i.replace('-', ' ') for i in doc.split()])
    doc = " ".join([i.replace('_', ' ') for i in doc.split()])
    doc = " ".join([i for i in doc.split() if not i.isdigit()])
    doc = " ".join([i for i in doc.split() if i.isalpha()])
    doc = " ".join([i for i in doc.split() if i not in stop])
    return doc

def vectorize_data(clean_title, vectorizer, vectorize = False, table = None):
    time_0 = time()
    print("Vectorizing cleaned data...\n")
    if (vectorize == True):
        X_features = vectorizer.fit_transform(clean_title)
        X_features = X_features.toarray()
    else:
        X_features = vectorizer.transform(clean_title)
        X_features = X_features.toarray()        
    if (table is not None and 'Topic' in table.columns):
        y_variable = np.asarray(table['Topic'])
        print("Returned X and y")
        time_taken = time() - time_0
        print("Time taken: %f sec" % (time_taken))
        return (X_features, y_variable, vectorizer)
    else:
        time_taken = time() - time_0
        print("Returned X")
        print("Time taken: %f sec" % (time_taken))
        return(X_features, vectorizer)

vectorizer = CountVectorizer(analyzer = "word", max_features = 2000)

with open('storage.bin', 'rb') as f:
    data_struct = cPickle.load(f)
    vectorizer, model = data_struct['vectorizer'], data_struct['model']

output_dir = "E:\dat01\Preprocessed_files\preprocessed_files_15_nov_2017"
#for Redw log files prepare the documents as per delimiters

print("Enter the location of the files:"); 
#give the path copied from the windows system as it is 


#directory = input()
directory = "E:\Company_work\log_files_15_nov\edw\load_area\oracle_traces\dat01\edw_alert_log-15_nov"
path = r"%s" % directory

for file in os.listdir(path):
    current_file = os.path.join(path, file)
    list_path = current_file.split('\\')
    output_file_name = file.split('.')[0]
    file_date = list_path[len(list_path)-2].split('-')[1]
    #open the log file for preprocessing
    f_input = open(current_file, "r")
    d = defaultdict(list)
    key = ''

    for line in f_input:
        if len(line.strip()) == 0 :
            continue
        try:
            line_list = line.strip().split()
            '''
            if len(line_list) < 3:
                continue
                '''
            if len(line_list) > 2 and '/' in line_list[0] and ':' in line_list[1]:
                key = line_list[0] + " " + line_list[1]
                log = ' '.join(line_list[2:len(line_list)])
            else:
                log = line.strip()
            d[key].append(log.strip())
        except Exception as e:
            print(e)
            print(line)
    
		
    f_input.close()
    #clean the documents by eliminating trailing spaces and empty documents
    
    s =   {k: [elem.strip() for elem in v ] for k,v in d.items()}
    s =   {k: list(filter(None,v)) for k,v in s.items()}
    f = {k: ".".join(v) for k,v in s.items()}
    fi = dict((k, v) for k, v in f.items() if v)
    #save the preprocessed documents into a csv file		
    output_file = os.path.join(output_dir, output_file_name)
    output_file = output_file + file_date + ".csv"
    with open(output_file,'w+') as output1:
        writer = csv.writer(output1)
        for key,value in fi.items():
            writer.writerow([key,value])
    
    
    data = pd.read_csv(output_file, header = None)
    data.columns = ('Date', 'Notes')
    doc_complete = data['Notes'].tolist()
    doc_clean = [clean(doc).split() for doc in doc_complete]
    doc_clean1 = [' '.join(i) for i in doc_clean]
    data['Notes_clean'] = doc_clean1
    clean_test = data['Notes_clean'].tolist()
    X_test, vectorizer = vectorize_data(clean_title = clean_test, vectorizer = vectorizer)
    y_test = model.predict(X_test)
    data['Prediction'] = y_test
    prediction_file = output_file.split('.')
    prediction_output = prediction_file[0] + "_prediction" + ".csv"
    data.to_csv(prediction_output, index = False)