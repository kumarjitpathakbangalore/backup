

from utils.templates.template_Gen import template_class

from utils.templates.func import retrieve_name, misc_string_convertor

# stringBC = "base_estimator=None, n_estimators=10, max_samples=1.0, max_features=1.0, bootstrap=True, bootstrap_features=False, oob_score=False, warm_start=False, n_jobs=1, random_state=None, verbose=0"
#
# stringAC = "base_estimator=None, n_estimators=50, learning_rate=1.0, algorithm='SAMME.R', random_state=None"
#
# stringGBMC = "loss='deviance', learning_rate=0.1, n_estimators=100, subsample=1.0, min_samples_split=2, min_samples_leaf=1, min_weight_fraction_leaf=0.0, max_depth=3, init=None, random_state=None, max_features=None, verbose=0, max_leaf_nodes=None, warm_start=False, presort='auto'"
#
# stringGBMR = "loss='ls', learning_rate=0.1, n_estimators=100, subsample=1.0, min_samples_split=2, min_samples_leaf=1, min_weight_fraction_leaf=0.0, max_depth=3, init=None, random_state=None, max_features=None, alpha=0.9, verbose=0, max_leaf_nodes=None, warm_start=False, presort='auto'"
#
# stringETC = "n_estimators=10, criterion='gini', max_depth=None, min_samples_split=2, min_samples_leaf=1, min_weight_fraction_leaf=0.0, max_features='auto', max_leaf_nodes=None, bootstrap=False, oob_score=False, n_jobs=1, random_state=None, verbose=0, warm_start=False, class_weight=None"
#
# stringETR = "n_estimators=10, criterion='mse', max_depth=None, min_samples_split=2, min_samples_leaf=1, min_weight_fraction_leaf=0.0, max_features='auto', max_leaf_nodes=None, bootstrap=False, oob_score=False, n_jobs=1, random_state=None, verbose=0, warm_start=False"

KernRidgeRModel ="alpha=1, kernel='linear', gamma=None, degree=3, coef0=1, kernel_params=None"

LinRegModel ="fit_intercept=True, normalize=False, copy_X=True, n_jobs=1"

LogRegModel ="penalty='l2', dual=False, tol=0.0001, C=1.0, fit_intercept=True, intercept_scaling=1, class_weight=None, random_state=None, solver='liblinear', max_iter=100, multi_class='ovr', verbose=0, warm_start=False, n_jobs=1"

RidgeRModel ="alpha=1.0, fit_intercept=True, normalize=False, copy_X=True, max_iter=None, tol=0.001, solver='auto', random_state=None"

LassoLarRModel ="alpha=1.0, fit_intercept=True, verbose=False, normalize=True, precompute='auto', max_iter=500, eps=2.2204460492503131e-16, copy_X=True, fit_path=True, positive=False"

ElasticNetRModel="alpha=1.0, l1_ratio=0.5, fit_intercept=True, normalize=False, precompute=False, max_iter=1000, copy_X=True, tol=0.0001, warm_start=False, positive=False, random_state=None, selection='cyclic'"

LarsRegModel = "fit_intercept=True, verbose=False, normalize=True, precompute='auto', n_nonzero_coefs=500, eps=2.2204460492503131e-16, copy_X=True, fit_path=True, positive=False"

LassoRegModel = "alpha=1.0, fit_intercept=True, normalize=False, precompute=False, copy_X=True, max_iter=1000, tol=0.0001, warm_start=False, positive=False, random_state=None, selection='cyclic'"

RidgeCModel="alpha=1.0, fit_intercept=True, normalize=False, copy_X=True, max_iter=None, tol=0.001, class_weight=None, solver='auto', random_state=None"

PrecptronModel="penalty=None, alpha=0.0001, fit_intercept=True, n_iter=5, shuffle=True, verbose=0, eta0=1.0, n_jobs=1, random_state=0, class_weight=None, warm_start=False"

LLICRModel ="criterion='aic', fit_intercept=True, verbose=False, normalize=True, precompute='auto', max_iter=500, eps=2.2204460492503131e-16, copy_X=True, positive=False"

SGDRModel = "loss='squared_loss', penalty='l2', alpha=0.0001, l1_ratio=0.15, fit_intercept=True, n_iter=5, shuffle=True, verbose=0, epsilon=0.1, random_state=None, learning_rate='invscaling', eta0=0.01, power_t=0.25, warm_start=False, average=False"

SGDCModel = "loss='hinge', penalty='l2', alpha=0.0001, l1_ratio=0.15, fit_intercept=True, n_iter=5, shuffle=True, verbose=0, epsilon=0.1, n_jobs=1, random_state=None, learning_rate='optimal', eta0=0.0, power_t=0.5, class_weight=None, warm_start=False, average=False"


name = retrieve_name(KernRidgeRModel)
default,estimator = misc_string_convertor(KernRidgeRModel)
template_class(name[0]," Kernel Ridge Regressor ",default,"KernelRidge",estimator)

name = retrieve_name(LarsRegModel)
default,estimator = misc_string_convertor(LarsRegModel)
template_class(name[0]," Lars Regressor ",default,"Lars",estimator)

name = retrieve_name(LassoLarRModel)
default,estimator = misc_string_convertor(LassoLarRModel)
template_class(name[0]," Lasso Lars Regressor ",default,"LassoLArs",estimator)

name = retrieve_name(LassoRegModel)
default,estimator = misc_string_convertor(LassoRegModel)
template_class(name[0]," Lasso Regressor ",default,"Lasso",estimator)

name = retrieve_name(PrecptronModel)
default,estimator = misc_string_convertor(PrecptronModel)
template_class(name[0]," Preceptron ",default,"Preceptron",estimator)

name = retrieve_name(RidgeCModel)
default,estimator = misc_string_convertor(RidgeCModel)
template_class(name[0]," Ridge Classifier ",default,"RidgeClassifier",estimator)

name = retrieve_name(RidgeRModel)
default,estimator = misc_string_convertor(RidgeRModel)
template_class(name[0]," Ridge Regressor ",default,"Ridge",estimator)

name = retrieve_name(SGDCModel)
default,estimator = misc_string_convertor(SGDCModel)
template_class(name[0]," SGD Classifier ",default,"SGDClassifier",estimator)

name = retrieve_name(LinRegModel)
default,estimator = misc_string_convertor(LinRegModel)
template_class(name[0]," Linear Regressor ",default,"LinearRegression",estimator)

name = retrieve_name(LogRegModel)
default,estimator = misc_string_convertor(LogRegModel)
template_class(name[0]," Logistic Regressor ",default,"LogisticRegression",estimator)

name = retrieve_name(ElasticNetRModel)
default,estimator = misc_string_convertor(ElasticNetRModel)
template_class(name[0]," Elastic Net Regressor ",default,"ElasticNet",estimator)

name = retrieve_name(LLICRModel)
default,estimator = misc_string_convertor(LLICRModel)
template_class(name[0]," Lasso Lars IC ",default,"LassoLarsIC",estimator)

name = retrieve_name(SGDRModel)
default,estimator = misc_string_convertor(SGDRModel)
template_class(name[0]," SGD Regressor ",default,"SGDRegressor",estimator)


