"""
These are miscellaneous functions used in code
"""

import os

from utils.properties import Properties


def getPath(filename):
    mode = ["data","evaluation","model","parameters","utils"]
    path = ""
    for any in mode:
        if str(os.getcwd()).__contains__(any):
            path = str(os.getcwd()).replace(any,"resources")
            break
        else:
            path = "{}/{}".format(str(os.getcwd()),"resources")

    path = path+"/"+filename
    print("path: {}".format(path))
    return path

def getPropertiesAtFolder(folder,filename):
    path=""
    for f in os.listdir(folder):
        if str(f).__contains__(filename):
            path = "{}/{}".format(folder,filename)
            break

    if len(path) == 0:
        path=getPathAtFolder(folder,filename)

    return path

def getPathAtFolder(folder,filename):
    mode = ["data","evaluation","model","parameters","utils"]
    path = ""
    for any in mode:
        if str(os.getcwd()).__contains__(any):
            path = str(os.getcwd()).replace(any,folder)
            break
        else:
            path = "{}/{}".format(str(os.getcwd()),folder)

    path = path+"/"+filename
    print("path: {}".format(path))
    return path

def converter(string,to="s"):
    try:
        if to == "s":
            return str(string)
        elif to == "b":
            s = str(string)
            if s in ['true', '1', 't', 'y', 'yes', 'yeah', 'yup', 'certainly', 'uh-huh',"Y","T","TRUE"]:
                return True
            else:
                return False
        elif to == "i":
            return int(string)

        elif to == "f":
            return float(string)
    except Exception as e:
        print('There is Something Wrong with Value "',string , '"! Error: ',e)




