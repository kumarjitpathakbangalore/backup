
from utils.templates.func import dict_to_par
def template_class(name,message,default,className,estimator):

    string = """
class {0}(object):

    def __init__(self):
        print("This is {1} Model")
        self.default ={3}

    def getClassName(self):
    return "{2}"

    def getLibraryName(self):
        return ""

    def train_model(self,train,test,target,parameter,n_estimators = 500):
        values={4}
        try:
            for key in self.default.keys():
                values[key] = parameter[key] if parameter.has_key(key) else self.default[key]

        except:
            values = self.default

        try:
            model = {2}({5})

            model.fit(train,target)
        except:
            model = {2}()
            model.fit(train,target)

            self.results = model.predict(test)

    def prediction(self):
        return self.results
    """.format(name,message,className,default,"{}",estimator)
    print(string)

# template("RTECModel","Random Tree Embedded Classifier",
#          "{'warm_start': False, 'n_jobs': 1, 'verbose': 0, 'max_leaf_nodes': None, 'min_samples_leaf': 1, 'n_estimators': 10, 'min_weight_fraction_leaf': 0.0, 'random_state': None, 'sparse_output': True, 'min_samples_split': 2, 'max_depth': 5}",
#          "RandomTreesEmbedding",
#          "n_estimators=values['n_estimators'],max_depth=values['max_depth'],min_samples_split=values['min_samples_split'],min_samples_leaf=values['min_samples_leaf'],min_weight_fraction_leaf=values['min_weight_fraction_leaf'],max_leaf_nodes=values['max_leaf_nodes'],sparse_output=values['sparse_output'],n_jobs=values['n_jobs'],random_state=values['random_state'],verbose=values['verbose'],warm_start=values['warm_start']",
#         )

def template_code_snippet(script_gen_bool,library,class_name,trainPath,testPath,index,target,subPath,estimator,save_at,script_at):
    # library=""
    # class_name=""
    # inPath=""
    # outPath=""
    # subPath=""
    # index=""
    # target=""
    # estimator=""
    # save_at=""

    if script_gen_bool:
        string = """
    import numpy as np
    import pandas as pd
    from {0} import  {1}

    def basic_treatment(df): # this is most general basic treatment for most cases and is applied to treat by default

        for col in df:
            if df[col].dtype == np.dtype('O'):
                df[col] = df[col].apply(lambda x : hash(str(x)))

        df.fillna(-1, inplace = True)

    def output_function_cat(x):
        if x < 0.5:
            return 0
        elif x > 1.5:
            return 2
        else:
            return 1

    def trimmer(data,to_trim,target_var):
        trimmed_features = data.columns.tolist()
        for feature in features:
            if str(to_trim).__contains__(","):
                trim = str(to_trim).split(",")
                if feature in trim:
                    trimmed_features.remove(feature)
            else:
                if feature == to_trim:
                    trimmed_features.remove(feature)
        if features.__contains__(target_var):
            features.remove(target_var)

        return data[trimmed_features]

    train = pd.read_csv({2})
    test = pd.read_csv({3})

    basic_treatment(train)
    basic_treatment(test)

    target = train[{5}]

    model = {1}({7})

    model.fit(trimmer(train,'{5}','{4}'),target)

    results = model.predict(trimmer(test,'{5}','{4}')

    results = [output_function_cat(x) for x in results]

    sub = pd.read_csv({6})

    sub[{4}] = test[{4}]
    sub[{5}] = test[{5}]
    sub.to_csv({8},index=False)

        """.format(library,class_name,trainPath,testPath,index,target,subPath,estimator,save_at)
        print(string)
        with open(script_at, "w") as text_file:
            text_file.write("{}".format(string))
