# -*- coding: utf-8 -*-
"""
Created on Fri Aug 26 18:55:47 2016

@author: jitink

This is Implementation of Varimax,promax and oblimin rotations of PCA and FA
Kindly refer function Definitions for Varibles to be passed.

Also, Phi is the loadings matrix after PCA.

"""
from __future__ import division
import numpy as np
from numpy.linalg import svd, lstsq

def varimax(Phi, gamma = 1.0, q = 20, tol = 1e-6, normalize=False):
    p,k = Phi.shape
    R = np.eye(k)
    d=0
    if normalize:
        norm_val = np.sqrt(np.dot(Phi,Phi.T).diagonal())
        norm_val = np.repeat(norm_val,k).reshape(p,k)
        Phi = Phi/norm_val
        
    for i in xrange(q):
        d_old = d
        Lambda = np.dot(Phi, R)
        u,s,vh = np.svd(np.dot(Phi.T,np.asarray(Lambda)**3 - (gamma/p) * dot(Lambda, 
                         np.diag(np.diag(np.dot(Lambda.T,Lambda))))))
        R = np.dot(u,vh)
        d = np.sum(s)
        if d_old!=0 and d/d_old < 1 + tol: break
    
    if normalize:
        return np.dot(Phi, R)*norm_val,R
    else:    
        return np.dot(Phi, R),R

def promax(Phi, gamma = 4, q = 20, tol = 1e-6):
    n,m = Phi.shape
    R=np.eye(m)
    var,rotation=varimax(Phi=Phi,q=q,tol=tol)
    Q = var*(abs(var)**(gamma-1))
    U = lstsq(var,Q)[0]
    V = lstsq(np.dot(U.T,U),R)[0].diagonal()
    U = np.dot(U,np.diag(np.sqrt(V)))
    return np.dot(var,U),np.dot(rotation,U)
    
def oblimin_obj(Phi,gamma=0,return_gradient=True):
    p,k = Phi.shape
    L2=Phi**2
    N=np.ones((k,k))-np.eye(k)
    if np.isclose(gamma,0):
        X=L2.dot(N)
    else:
        C=np.ones((p,p))/p
        X=(np.eye(p)-gamma*C).dot(L2).dot(N)
    phi=np.sum(L2*X)/4
    if return_gradient:
        Gphi=Phi*X
        return phi, Gphi
    else:
        return phi
        
def oblimin(Phi,Rot=None,max_iters=20,tol=1e-6):
    A=Phi
    T=Rot
    if T is None:
        T=np.eye(A.shape[1])
    al=1
    table=[]
    Ti=np.linalg.inv(T)
    L= A.dot(Ti.T)
    f,Gq = oblimin_obj(Phi=Phi,return_gradient=True)
    G=-((L.T).dot(Gq).dot(Ti)).T
    #iteration
    for i_try in range(0,max_iters):
        Gp=G-T.dot(np.diag(np.sum(T*G,axis=0)))
        s=np.linalg.norm(Gp,'fro');
        table.append([i_try, f, np.log10(s), al])
        #if we are close stop
        if s < tol: break
        #update T
        al=2*al
        for i in range(11):
            #determine Tt
            X=T-al*Gp
            v=1/np.sqrt(np.sum(X**2,axis=0))
            Tt=X.dot(np.diag(v))
            Ti=np.linalg.inv(Tt)
            L= A.dot(Ti.T)
            ft,Gq = oblimin_obj(Phi=Phi,return_gradient=True)
            if ft<f-.5*s**2*al: break
            al=al/2
        T=Tt
        f=ft
        G=-((L.T).dot(Gq).dot(Ti)).T
    Th=T
    Lh = A.dot(np.linalg.inv(T.T))
    Phi = (T.T).dot(T)
    return Lh, Phi, Th, table
        
        
    
    
if __name__ == "__main__":
    
    A = np.array([[-0.73465832, -0.24819766, -0.32045055],
              [-0.3728976,   0.58628043, -0.63433607],
              [-0.72617152,  0.53812819, -0.22846634],
              [ 0.34042864, -0.08063226, -0.80064174],
              [ 0.8804307,   0.17166265,  0.04381426],
              [-0.66313032,  0.54576874,  0.37964986],
              [ 0.286712,    0.68305196,  0.21769803],
              [ 0.94651412,  0.14986739, -0.06825887],
              [ 0.40699665,  0.73202276, -0.08462949]])
    #vari=varimax(A)
    print("Varimax without Nomalisation :\n{}".format(varimax(A)))
    print("Varimax with Nomalisation :\n{}".format(varimax(A,normalize=True)))
    print("Promax without Nomalisation :\n{}".format(promax(A)))
    print("Oblimin without Gradient :\n{}".format(oblimin(A)))