# -*- coding: utf-8 -*-
"""
This is Script Calculates Information Value of Attributes w.r.t Target 
Variable!

To Use the Code Just copy paste the Functions and Imports and pass data to the
function InformationValue.

Note: It Is Recommended to explicitely defin event and Non Event while 
calculating Information Value.

Kindly refer Example.

"""
from __future__ import division,print_function
import pandas as pd
import numpy as np


def binner(target,variable,lower=None, upper=None,bins=None, min_obs=None,
           digits=None):
    if(np.shape(target)[0]==np.shape(variable)[0]):
        df = pd.DataFrame({'var':variable,'tar':target})
        if(digits== None):
            digits = 2
        if(lower == None):
            lower=df['var'].min(0)
        if(upper == None):
            mx = df['var'].max(0)
            perc = df['var'].quantile(.75)
            upper = (np.ceil(perc*1.5) if mx > perc*1.5 else mx)
        if (lower==upper):
            print("There is no difference in Variable value!")
            return None
        if(min_obs == None):
            min_obs=np.ceil(np.shape(variable)[0]*0.05)
        if(bins == None):
            bins=1
            for i in range(1,np.int(upper-lower)):
                bin_val=np.append(np.arange(lower,upper,i),upper)
                bin_val=np.append(bin_val,np.inf)                
                df['cuts'] = pd.cut(df['var'],bin_val)
                crosstab=pd.crosstab(df['cuts'],df['tar'])
                cross_bool=crosstab>=min_obs
                val_bool = pd.value_counts(df['cuts'])>=min_obs
                if(cross_bool.values.any() != False and val_bool.values.all()):
                    break
                bins=i+1
        bins=np.append(np.arange(lower,upper,bins),upper)
        bins=np.append(bins,np.inf)
        print("Upper: {}, lower:{}, min_obs:{}, bins:{}".format(upper,lower,
              min_obs,bins))
        return(pd.cut(df['var'],bins=bins,include_lowest=True,precision=digits))
    else:
         print("Size of target and variable not same! Retruning None!")
         return None
 
def InformationValue(data,target_var,event=None,nonevent=None):
    df_iv = pd.DataFrame({'Variable':data.columns.drop(target_var)})
    df_iv['InformationValue']=np.repeat(0,data.columns.size-1)
    print("Initital Conditions :\n{}".format(df_iv))
    if(event==None):
        event=data[target_var].min(0)
    if(nonevent==None):
        nonevent=data[target_var].pop(event).max(0)
    for col_ in df_iv['Variable']:
        idx = df_iv[df_iv.Variable==col_].index
        df_part = data[[col_,target_var]]
        #print(df_part)
        print("\nCalculating IV for Attribute: {}".format(col_))
        print("\nBinnig Criteria:")
        df_part['binned']=binner(target=df_part[target_var],
                                 variable=df_part[col_])
        crosstab = pd.crosstab(df_part['binned'],df_part[target_var])
        print("\nBinned Variable Tabulation:\n{}".format(crosstab))
        woe,iv=0,0
        tot_event=np.nansum(crosstab[event])
        tot_bad=np.nansum(crosstab[nonevent])
        print("Event:{} , Nonevent:{}".format(tot_event,tot_bad))
        for i in range(crosstab.shape[0]):
            sub_event=crosstab.iloc[i][event]/tot_event
            sub_bad=crosstab.iloc[i][nonevent]/tot_bad
            woe=np.log(sub_event/sub_bad)
            print("SubEvent:{} , SubNonEvent:{} Woe:{}".format(sub_event,
                  sub_bad,woe))
            if(np.isfinite(woe)):
                iv += (sub_event-sub_bad)*woe
        print("\nFor {} the IV is :{}".format(col_,iv))
        df_iv.loc[idx,'InformationValue']=iv
    #print(df_iv)
    return(df_iv)
                
    
    

if __name__ == "__main__" :

    var = np.array([1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,6,6,7,8,
                    9,9,10,11,13,15,77,99])
    var1 = np.linspace(50,105,34)
    var2 = np.arange(34)
    tar = np.array([1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1,
                    0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1])
    d = {'VarA':var,'VarB':var1, 'VarC':var2,'target':tar}
    df=pd.DataFrame(d)
    
    IV=InformationValue(df,'target')
    print(IV)

    IV=InformationValue(df,'target',event=0,nonevent=1)
    print(IV)
    
    IV=InformationValue(df,'target',event=1,nonevent=0)
    print(IV)
   
    
    