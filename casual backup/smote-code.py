# -*- coding: utf-8 -*-
"""
Created on Wed Aug 31 01:06:48 2016

@author: jitink
"""

#from __future__ import division

import sys
import numpy as np
import pandas as pd
from random import choice, sample
from sklearn.neighbors import NearestNeighbors


def SMOTE(T, N, k, h = 1.0):
    """
    Returns (N/100) * n_minority_samples synthetic minority samples.

    Parameters
    ----------
    T : array-like, shape = [n_minority_samples, n_features]
        Holds the minority samples
    N : percetange of new synthetic samples:
        n_synthetic_samples = N/100 * n_minority_samples. Can be < 100.
    k : int. Number of nearest neighbours.

    Returns
    -------
    S : Synthetic samples. array,
        shape = [(N/100) * n_minority_samples, n_features].
    """
    n_minority_samples, n_features = T.shape

    if N < 100:
        #create synthetic samples only for a subset of T.
        #TODO: select random minortiy samples
        N = 100
        pass

    if (N % 100) != 0:
        raise ValueError("N must be < 100 or multiple of 100")

    N = N/100
    n_synthetic_samples = N * n_minority_samples
    S = np.zeros(shape=(n_synthetic_samples, n_features))

    #Learn nearest neighbours
    neigh = NearestNeighbors(n_neighbors = k)
    neigh.fit(T)

    #Calculate synthetic samples
    for i in xrange(n_minority_samples):
        nn = neigh.kneighbors(T[i], return_distance=False)
        for n in xrange(N):
            nn_index = choice(nn[0])
            #NOTE: nn includes T[i], we don't want to select it
            while nn_index == i:
                nn_index = choice(nn[0])

            dif = T[nn_index] - T[i]
            gap = np.random.uniform(low = 0.0, high = h)
            S[n + i * N, :] = T[i,:] + gap * dif[:]

    return S

def SMOTING(X, y, minority_target, N, k,n=(0.9,0.1),h =0.5):
    """

    :param X: Array of Independent Variables
    :param y: Array of Dependent Variables
    :param minority_target: Value which is minority class
    :param N: Ratio (multiple of 100) in which data is to be synthesized
    :param k: No of K neighbours for kNN.
    :param n: Ratio of majority and minority must be less than .95
    :return: SMOTEed Data
    """

    n_sample,_=X.shape

    majority_indexes=[]
    minority_indexes=[]
    for i in xrange(n_sample):
        if y[i] == minority_target:
            minority_indexes.append(i)
        else:
            majority_indexes.append(i)

    synthetic_samples = SMOTE(X[minority_indexes], N=N, k=k, h=h)
    syn_rows,_=synthetic_samples.shape

    resampled_ratio = int(((len(minority_indexes)+syn_rows)*n[0])/n[1])
    undersampled_majority= sample(majority_indexes,resampled_ratio)

    resampled_min = np.vstack((X[minority_indexes],synthetic_samples))
    y_min = np.repeat(1,resampled_min.shape[0])
    resampled_maj = X[undersampled_majority]
    y = np.append(y_min,np.repeat(0, resampled_maj.shape[0]))

    return (np.vstack((resampled_min,resampled_maj)),y)

def main():
    """
    
    """
	
    data = pd.read_csv("/media/jitink/dsa/ofc/fault-pred/Phase II/smote_data.csv")
    data.replace([np.inf, -np.inf], np.nan)
    data.fillna(0, inplace=True)
    X = data.loc[:, data.columns.drop("tnd_cc_flag")].values
    y = data["tnd_cc_flag"].values
    print("Read data :{}".format(data.shape))
    print("X shape :{}".format(X.shape))
    print("y shape : {}".format(y.shape))

    print("Present Distribution :{}".format(data['tnd_cc_flag'].value_counts()))
    smoting = SMOTING(X, y, 1, 100, 7, (.7,.3))
    print("train for pure smote shape{}".format(X.shape))
    print("target for pure smote shape{}".format(y.shape))
    print("target distribution :{}".format(np.bincount(y)))
    print("final Smoting {}".format(smoting[0].shape))
    print("Smote ditribution:{}".format(np.bincount(smoting[1])))
    print("Smote target shape:{}".format(smoting[1].shape))

    #print("smote columns {}".format(smoting.columns))
    print("saving")
    print("smote columns {}".format(data.columns))
    print("smote columns {}".format(data.columns[1:]))
    #smoting=pd.DataFrame(smoting,columns=data.columns)
    print(type(smoting))
    smote = pd.DataFrame(smoting[0],columns=data.columns[1:])
    print(smote.shape)
    print(smote.head)
    smote['tnd_cc_flag']=smoting[1]
    print(smote.shape)
    print(smote.head)

    smote.to_csv("/media/jitink/dsa/ofc/fault-pred/Phase II/post_smote_data.csv",index=False)

    sys.exit(0)

if __name__ == "__main__": main()
