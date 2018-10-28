
import inspect
import json


def misc_string_convertor(string):

    param_dict={}
    par = []
    for str in string.split(","):
        sp = str.split("=")
        if len(sp) == 2:
            key = "{}".format(sp[0].replace(" ",""))
            param_dict[key] = eval(sp[1])
            s = "{}=values['{}']".format(key,key)
            par.append(s)

    js = json.dumps(param_dict,indent=6, separators=(',', ': '))
    to_method = ','.join(par)

    # print(retrieve_name(string))
    # print(json.dumps(dict2,indent=6, separators=(',', ': ')))
    # print(dict2)
    # print(''.join(par))

    return (param_dict,to_method)

def name_var(**variables):
    return [x for x in variables]

def retrieve_name(var):
    callers_local_vars = inspect.currentframe().f_back.f_locals.items()
    return [var_name for var_name, var_val in callers_local_vars if var_val is var]

def dict_to_par(par_dict):

    par=[]
    for k in par_dict.keys():
        s = "{}={}".format(k,par_dict[k])
        par.append(s)

    # print(par)

    return ','.join(par)