# -*- coding: utf-8 -*-
"""
Created on Thu Aug 31 18:15:17 2017

@author: ESelvaraj
"""

import gensim
import numpy as np
import pandas as pd 

from time import time
from gensim import corpora
from datetime import datetime
from nltk.corpus import stopwords

def clean(doc, stops):
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
    doc = " ".join([i for i in doc.split() if i not in stops])
    return doc
    
def get_topic_list(ldamodel, doc_term_matrix):
    topic_list = []
    for j in doc_term_matrix:
        temp = ldamodel[j]
        list1 = []
        list2 = []
        for k in temp:
            list1.append(k[0])
            list2.append(k[1])
        topic_list.append(list1[np.argmax(list2)])
    return topic_list
    
def get_topics(ldamodel, doc_term_matrix, n_topic, n_words):
    topic_names = ldamodel.print_topics(num_topics = n_topic, num_words = n_words)
    index = list(xrange(n_topic))
    col1 = []
    col2 = []
    for i in xrange(n_words):
        col1.append('word' + str(i))
        col2.append('score' + str(i))
    cols = col1 + col2
    topics = pd.DataFrame(index = index, columns = cols)
    for j, top in enumerate(topic_names):
        top_list = (top[1]).split('+')
        max_up = len(col1) - 1
        for k in xrange(len(col1)):
            if k == 0:
                topics[col1[k]][j] = top_list[k][7:-2]
                topics[col2[k]][j] = float(top_list[k][0:5])
            elif k == max_up:
                topics[col1[k]][j] = top_list[k][8:-1]
                topics[col2[k]][j] = top_list[k][1:6]
            else:
                topics[col1[k]][j] = top_list[k][8:-2]
                topics[col2[k]][j] = top_list[k][1:6]
    return topics

def run_lda(dataset, col, id_c, clean, n_topic = 10, n_iter = 10000, n_words = 5, stop = None, model = None, 
            dicti = None, topic = None):
    t0 = time()
    data = dataset[[id_c, col]]
    data = data.applymap(str)
    data[id_c] = data[id_c].astype(int)
    data = data[data[col] != 'nan']
    name = str(datetime.today().day) + str(datetime.today().month)
    doc_complete = data[col].tolist()
    stops = set(stopwords.words('english') + stop)
    doc_clean = [clean(doc, stops).split() for doc in doc_complete]
    print('Data Cleansed')
    dictionary = corpora.Dictionary(doc_clean)
    doc_term_matrix = [dictionary.doc2bow(doc) for doc in doc_clean]
    print('Document Term Matrix Processed')
    Lda = gensim.models.ldamodel.LdaModel
    ldamodel = Lda(doc_term_matrix, num_topics = n_topic, id2word = dictionary, iterations = n_iter)
    print('LDA Model Trained')
    topic_list = get_topic_list(ldamodel, doc_term_matrix)
    data['topic_list'] = topic_list
    print('Topic List Processed')
    topics = get_topics(ldamodel, doc_term_matrix, n_topic, n_words)
    print('Top Words in Topics Processed')
    dataset = dataset.merge(data, how = 'left', on = id_c)
    if model != None:
        model = model + '.model'
    else:
        model = name + '.model'
    ldamodel.save(model)
    if dicti != None:
        dicti = dicti + '.dict'
    else:
        dicti = name + '.dict'
    dictionary.save(dicti)
    if topic != None:
        topic = topic + '.csv'
    else:
        topic = name + '.csv'
    topics.to_csv(topic, index = False)
    print('Saved Files')
    print('Total time taken:', time() - t0, 'sec')
    return dataset