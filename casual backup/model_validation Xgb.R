library(xgboost)
library(data.table)
library(MatrixModels)
library(methods)
library(Matrix)
library(caret)

### loading attributes
attributes <- read.csv("",header = TRUE)
attributes <- attributes[,1]

### Loading Model
xgb_mod_nf_out <- xgb.load("xgb_model")

### Loading Data
yuk_oob <- read.csv("",header =TRUE)

# dt_oob <- data.table(yuk_oob,keep.rownames =  FALSE)
# sparse_matrix_oob = sparse.model.matrix(formula_nn,data=dt_oob)
# op_vec_oob <- dt_oob[,Y:=0][tnd_cc_flag == 1,Y:=1][,Y]
# d_oob <- xgb.DMatrix(sparse_matrix_oob,label = op_vec_oob)

yuk_oob[is.na(yuk_oob)] <- -1

d_oob <- xgb.DMatrix(data = data.matrix(yuk_oob[,attributes]),
                      label = data.matrix(yuk_oob$tnd_cc_flag),
                      missing = 0)



### Making Prediction
pred_oob <- predict(xgb_mod_nf_out,d_oob)
confusionMatrix(ifelse(pred_oob>0.3,1,0),yuk_oob[,"tnd_cc_flag"])

