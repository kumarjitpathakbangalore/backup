#import necessary packages
from pyspark.sql.types import *
from pyspark.sql.functions import col, isnan, isnull,count,when, mean


from pyspark.sql import SparkSession
from pyspark import SparkContext, SparkConf
from pyspark.sql import SQLContext
import pandas as pd

from pyspark.ml import Pipeline
from pyspark.ml.classification import RandomForestClassifier
from pyspark.ml.feature import IndexToString, StringIndexer, VectorIndexer
from pyspark.ml.evaluation import MulticlassClassificationEvaluator


#/user/jeevan/hackathon/ipfiles
#create the schema for cdr data

cdrschema = StructType([
   StructField("international_plan",StringType(),True),
   StructField("voice_mail_plan",StringType(),True),
   StructField("pct_call_drops",DoubleType(),True),
   StructField("pct_packet_drops",DoubleType(),True),
   StructField("number_vmail_messages",LongType(),True),
   StructField("total_day_minutes",DoubleType(),True),
   StructField("total_day_calls",LongType(),True),
   StructField("total_eve_minutes",DoubleType(),True),
   StructField("total_eve_calls",LongType(),True),
   StructField("total_night_minutes",DoubleType(),True),
   StructField("tota_night_calls",LongType(),True),
   StructField("total_intl_minutes",DoubleType(),True),
   StructField("total_intl_calls",LongType(),True),
   StructField("phone_number",StringType(),True)])

#create schema for complaint data

complaintschema = StructType([
   StructField("phone_number",StringType(),True),
   StructField("customer_service_calls",LongType(),True),
   StructField("Number_of_complaint_raised",LongType(),True),
   StructField("pct_acket_drops",LongType(),True),
   StructField("Time_for_resolution",LongType(),True),
   StructField("payment_made_but_order_not_placed_and_amount_is_deducted_from_my_bank_account",LongType(),True),
   StructField("Billing_issue",LongType(),True),
   StructField("Calls_are_not_going",LongType(),True),
   StructField("drop_calls",LongType(),True),
   StructField("No_internet_connection",LongType(),True),
   StructField("order",LongType(),True),
   StructField("payment_refund",LongType(),True),
   StructField("recharge_offer_service_fraud",LongType(),True),
   StructField("Slow_network_coverage",LongType(),True)])

#   StructField("uninformed_unsubscribed_weekly_amount_deduct",DoubleType(),True)])
   

#create schema for churn data

churnschema = StructType([
   StructField("churn",StringType(),True),
   StructField("state",StringType(),True),
   StructField("account_length",LongType(),True),
   StructField("area_code",LongType(),True),
   StructField("phone_number",StringType(),True),
   StructField("account_days",LongType(),True),
   StructField("age",LongType(),True),
   StructField("gender",StringType(),True),
   StructField("occupation",StringType(),True),
   StructField("join_date",StringType(),True),
   StructField("subscription_renewal",StringType(),True)])

#create schema for revenue data
revenueschema = StructType([
   StructField("total_day_charge",DoubleType(),True),
   StructField("total_eve_charge",DoubleType(),True),
   StructField("total_night_charge",DoubleType(),True),
   StructField("total_intl_charge",DoubleType(),True),
   StructField("arpu",DoubleType(),True),
   StructField("phone_number",StringType(),True)])

#create schema for signal data
signalschema = StructType([
   StructField("phone_number",StringType(),True),
   StructField("signalstrength_4g",LongType(),True),
   StructField("signalstrength_3g",LongType(),True),
   StructField("Duration_of_good_signal_strength_per",DoubleType(),True),
   StructField("Duration_of_bad_Signal_strength_percent",DoubleType(),True)])


#create dataframe using cdr data
cdr = spark.read.csv("/user/jeevan/hackathon/ipfiles/cdr.csv",header=True,schema=cdrschema)
complaint = spark.read.csv("/user/jeevan/hackathon/ipfiles/complaint.csv",header=True,schema=complaintschema)
churn = spark.read.csv("/user/jeevan/hackathon/ipfiles/customer_churn_demography.csv",header=True,schema=churnschema)
revenue = spark.read.csv("/user/jeevan/hackathon/ipfiles/revenue.csv",header=True,schema=revenueschema)
signal = spark.read.csv("/user/jeevan/hackathon/ipfiles/signalstrength.csv",header=True,schema=signalschema)

#joining all tables together

h_df = churn.join(cdr,"phone_number").join(complaint,"phone_number").join(revenue,"phone_number").join(signal,"phone_number")

#Preparing ABT

#Coding churn into 0 and 1

from pyspark.sql.functions import col, expr, when

new_churn = expr(
    """IF(churn == "False", 0, 1)"""
)
h_df1 = h_df.withColumn("churn",new_churn) 



#state

categories = h_df1.select("state").distinct().rdd.flatMap(lambda x: x).collect() 
exprs = [when(col("state") == category, 1).otherwise(0).alias(category)
         for category in categories] 


df_state = h_df1.select("phone_number",*exprs)
df_state.show(5)

#occupation

categories = h_df1.select("occupation").distinct().rdd.flatMap(lambda x: x).collect() 
exprs = [when(col("occupation") == category, 1).otherwise(0).alias(category)
         for category in categories] 
df_occu = h_df1.select("phone_number",*exprs)
df_occu.show(5)

#churn

#categories = h_df1.select("churn").distinct().rdd.flatMap(lambda x: x).collect() 
#exprs = [when(col("churn") == category, 1).otherwise(0).alias(category)
#         for category in categories] 
#df_churn = h_df1.select("phone_number","churn", *exprs)
#df_churn.show(5)

#gender

categories = h_df1.select("gender").distinct().rdd.flatMap(lambda x: x).collect() 
exprs = [when(col("gender") == category, 1).otherwise(0).alias(category)
         for category in categories] 
df_gender = h_df1.select("phone_number",*exprs)
df_gender.show(5)

#international_plan

int_plan = expr(
    """IF(international_plan == "no", 0, 1)"""
)
df_int_plan = h_df1.select("phone_number","international_plan").withColumn("international_plan_c",int_plan).drop("international_plan")
df_int_plan.show(5)


#voice_mail_plan

voice_plan = expr(
    """IF(voice_mail_plan == "no", 0, 1)"""
)

df_voic_plan = h_df1.select("phone_number","voice_mail_plan").withColumn("voice_mail_plan_c",voice_plan).drop("voice_mail_plan")
df_voic_plan.show(5)


#Combining all 

df_cat_comb = h_df1.select('phone_number').distinct().join(df_state,"phone_number").join(df_occu,"phone_number").join(df_gender,"phone_number").join(df_int_plan,"phone_number").join(df_voic_plan,"phone_number")


df_final = h_df1.join(df_cat_comb,"phone_number")


#
# Null value treatment
# Author : Rahul Agarwal
#

#step1

checkNull = df_final.select([count(when(isnull(c), c)).alias(c) for c in df_final.columns]).collect()


def missing_value_impute(df,col):
    mean_val = df.select(mean(df[col])).collect()
    return mean_val[0][0]

dict = checkNull[0].asDict()

#step 2

for key,value in dict.items():
    if(value>0):
        MeanValue = missing_value_impute(df_final,key)
        df_final=df_final.na.fill(MeanValue,[key])

#step 3

datatypes = df_final.dtypes

def OutlierDetection(col,df):
    q1,q3 = df_final.stat.approxQuantile(col,[0.25,0.75],0.0)
    iqr = q3 - q1
    low = q1 - 1.5*iqr
    up = q3 + 1.5*iqr
    return df.filter((df_final[col] < low) & (df_final[col] > up)).count()

#step 4

def performOutlier(datatypes,df):
    OutlierList = []
    for c in datatypes:
        if((c[1] == 'int') | (c[1] == 'double') | (c[1] == 'float') | (c[1] == 'long')):
            value = OutlierDetection(c[0],df)
            OutlierList.append((c[0],value))
    df_new = spark.createDataFrame(OutlierList)
    return df_new

outlierDf = performOutlier(datatypes,df_final)

outlierDf.show()

#-------------------------------------------------------------
#	Correlation
#	Author : Sivappa Gundulur
#--------------------------------------------------------------


df_final2 = df_final.drop("phone_number","state","gender","occupation","join_date","subscription_renewal","international_plan","voice_mail_plan")

# wrapper around
# https://forums.databricks.com/questions/3092/how-to-calculate-correlation-matrix-with-all-colum.html

from pyspark.mllib.stat import Statistics
import pandas as pd
def compute_correlation_matrix(df,method='spearman'):
    
    churn_data3_rdd = df.rdd.map(lambda row: row[0:])
    corr_mat = Statistics.corr(churn_data3_rdd, method=method)
    corr_mat_churn_data3 = pd.DataFrame(corr_mat,
                    columns=df.columns, 
                    index=df.columns)
    return corr_mat_churn_data3


compute_correlation_matrix(df=df_final2)

#
#Statistical Summary and EDA
#Author : Kumarjit Pathak
#

df_final.describe('churn').show()

h_df_stat= df_final.summary().repartition(1)

h_df_stat.write.save("/user/jeevan/hackathon/statistics",format="csv",header=True)


from pyspark.ml import Pipeline
from pyspark.ml.feature import StringIndexer, OneHotEncoder, VectorAssembler

# Get all string cols/categorical cols*
stringColList = [i[0] for i in df_final.dtypes if i[1] == 'string']

# generate OHEs for every col in stringColList*
OHEstages = [OneHotEncoder(inputCol = categoricalCol, outputCol = categoricalCol + "Vector") for categoricalCol in stringColList]

#Standardization
from pyspark.mllib.linalg import DenseVector
input_data = df_final.rdd.map(lambda x: (x[1], DenseVector(x[2:])))
type(input_data)

# Replace `df` with the new DataFrame
#input_data_df = spark.createDataFrame(input_data, ["label", "features"])


# Import `LinearRegression`
from pyspark.ml import Pipeline
from pyspark.ml.classification import LogisticRegression, LogisticRegressionModel
from pyspark.ml.feature import VectorAssembler

assembler = (VectorAssembler()
            .setInputCols(df_final.columns[2:])
            .setOutputCol("churn"))

# Initialize `lr`
#lr = LogisticRegression(labelCol="label", maxIter=10, regParam=0.3, elasticNetParam=0.8)

lr = LogisticRegression(maxIter=10, regParam=0.3)

pipeline = Pipeline(stages = [assembler, lr])
model = pipeline.fit(df_final)




#-------------------------------------------------------------
#	Data Modeling
#	Author : Nitish Rai
#--------------------------------------------------------------


churn_raw_v2 = h_df.na.drop()
churn_raw_v2.groupBy("churn").count().show()

#dropping duplicates from the dataframe
#churn_raw_v2.dropDuplicates().show()

# Remove NULL value
#churn_raw_v2.filter(churn_raw_v2.churn != '')

# Remove outliers from dataframe
# Index labels, adding metadata to the label column.
# Fit on whole dataset to include all labels in index
# var transformation :: Gender , international_plan, 

Gender_indexer = StringIndexer(inputCol="gender", outputCol="gender_cat")
churn_raw_v2 = Gender_indexer.fit(churn_raw_v2).transform(churn_raw_v2)

plan_indexer = StringIndexer(inputCol="international_plan", outputCol="plan_category")
churn_raw_v2 = plan_indexer.fit(churn_raw_v2).transform(churn_raw_v2)

# Data prepration part-1
# Replace blank value NONE

churn_raw_v2 = churn_raw_v2.replace('','0','Extension').alias('Extension') 
churn_raw_v2.createOrReplaceTempView("churn_table")

churn_raw_v3 = spark.sql("select churn,age,account_length,account_days,tota_night_calls,customer_service_calls from churn_table")

churn_raw_v3.printSchema() 
churn_raw_v3.show()

# Split the data into training and test sets (30% held out for testing)
(trainingData, testData) = churn_raw_v3.randomSplit([0.7, 0.3])

#### Data prepration part-2

from pyspark.mllib.linalg import Vectors
from pyspark.mllib.regression import LabeledPoint

data_train_rdd = trainingData.rdd
type(data_train_rdd)

data_test_rdd = testData.rdd
type(data_test_rdd)

transformed_train_df = data_train_rdd.map(lambda row: LabeledPoint(row[0], Vectors.dense(row[1:])))
transformed_test_df = data_test_rdd.map(lambda row: LabeledPoint(row[0], Vectors.dense(row[1:])))

from pyspark.mllib.tree import RandomForest
RANDOM_SEED = 10904
RF_NUM_TREES = 100

model = RandomForest.trainClassifier(transformed_train_df, numClasses=2,categoricalFeaturesInfo={},numTrees=RF_NUM_TREES, featureSubsetStrategy="auto",impurity="gini",maxDepth=4,maxBins=32,seed=RANDOM_SEED)

predictions = model.predict(transformed_train_df.map(lambda x: x.features))
labels_and_predictions = transformed_train_df.map(lambda x: x.label).zip(predictions)
acc = labels_and_predictions.filter(lambda x: x[0] == x[1]).count() / float(transformed_train_df.count())
print("Model accuracy: %.3f%%" % (acc * 100))

predictions = model.predict(transformed_test_df.map(lambda x: x.features))
labels_and_predictions = transformed_test_df.map(lambda x: x.label).zip(predictions)
acc = labels_and_predictions.filter(lambda x: x[0] == x[1]).count() / float(transformed_test_df.count())
print("Model accuracy: %.3f%%" % (acc * 100))

from pyspark.mllib.evaluation import BinaryClassificationMetrics

metrics = BinaryClassificationMetrics(labels_and_predictions)
print("Area under Precision/Recall (PR) curve: %.f" % (metrics.areaUnderPR * 100))
print("Area under Receiver Operating Characteristic (ROC) curve: %.3f" % (metrics.areaUnderROC * 100))


