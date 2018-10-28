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

#another way of joining(Duplicate phone_number column is here)
h_df2 = (churn.join(cdr,churn["phone_number"] == cdr["phone_number"],'left_outer') \
        .join(complaint,churn["phone_number"] == complaint["phone_number"],'left'))

#count the number of rows
h_df.count()


#check the number of partitions

h_df.rdd.getNumPartitions()

#describe statistics

h_df.summary().repartition(1).write.save("/user/jeevan/hackathon/ipfiles/statistics",format="csv",header=True)


#select few columns

h_df.select('churn','phone_number').show(5)

#find distinct columns

h_df.select('voice_mail_plan').distinct().count()

#Cross tabulate ans sorting
h_df.crosstab('Age', 'churn').sort('Age_churn', assending = True).show()


#fillna

h_df.na.fill({'age': 50, 'area code': 'unknown'}).show()

#withColumn 

from pyspark.sql.functions import col, expr, when

new_churn = expr(
    """IF(churn == "False", 0, 1)"""
)
h_df1 = h_df.withColumn("churn",new_churn) #It will override the existing churn column
h_df1 = h_df.withColumn("churn_2",new_churn) #It wil 

#Flatten(setting categorical variable with binary notation)
===========================================================
#state

categories = h_df1.select("state").distinct().rdd.flatMap(lambda x: x).collect() 
exprs = [when(col("state") == category, 1).otherwise(0).alias(category)
         for category in categories] 


df_state = h_df1.select("phone_number","state", *exprs)
df_state.show(5)

#occupation

categories = h_df1.select("occupation").distinct().rdd.flatMap(lambda x: x).collect() 
exprs = [when(col("occupation") == category, 1).otherwise(0).alias(category)
         for category in categories] 
df_occu = h_df1.select("phone_number","occupation", *exprs)
df_occu.show(5)

#churn

categories = h_df1.select("churn").distinct().rdd.flatMap(lambda x: x).collect() 
exprs = [when(col("churn") == category, 1).otherwise(0).alias(category)
         for category in categories] 
df_churn = h_df1.select("phone_number","churn", *exprs)
df_churn.show(5)

#gender

categories = h_df1.select("gender").distinct().rdd.flatMap(lambda x: x).collect() 
exprs = [when(col("gender") == category, 1).otherwise(0).alias(category)
         for category in categories] 
df_gender = h_df1.select("phone_number","gender", *exprs)
df_gender.show(5)

#international_plan

categories = h_df1.select("international_plan").distinct().rdd.flatMap(lambda x: x).collect() 
exprs = [when(col("international_plan") == category, 1).otherwise(0).alias(category)
         for category in categories] 
df_int_plan = h_df1.select("phone_number","international_plan", *exprs)
df_int_plan.show(5)

#voice_mail_plan

categories = h_df1.select("voice_mail_plan").distinct().rdd.flatMap(lambda x: x).collect() 
exprs = [when(col("voice_mail_plan") == category, 1).otherwise(0).alias(category)
         for category in categories] 
df_voic_plan = h_df1.select("phone_number","voice_mail_plan", *exprs)
df_voic_plan.show(5)


#Combining all 

df_cat_comb = h_df1.select('phone_number').distinct().join(df_state,"phone_number").join(df_occu,"phone_number").join(df_churn,"phone_number").join(df_gender,"phone_number").join(df_int_plan,"phone_number").join(df_voic_plan,"phone_number")


df_final_with_catvar = h_df1.join(df_cat_comb,"phone_number")

df_final_with_catvar.rdd.getNumpartitions()

df_cat_comb.show(10)

#-----------------------*
# Code from Rahul
#-----------------------*

#step1

checkNull = h_df.select([count(when(isnull(c), c)).alias(c) for c in h_df.columns]).collect()

#checkNull = df_cat_comb.select([count(when(isnull(c), c)).alias(c) for c in df_cat_comb.columns]).collect()

def missing_value_impute(df,col):
    mean_val = df.select(mean(df[col])).collect()
    return mean_val[0][0]

dict = checkNull[0].asDict()

#step 2

for key,value in dict.items():
    if(value>0):
        MeanValue = missing_value_impute(h_df,key)
        h_df=h_df.na.fill(MeanValue,[key])

#step 3

datatypes = h_df.dtypes

def OutlierDetection(col,df):
    q1,q3 = h_df.stat.approxQuantile(col,[0.25,0.75],0.0)
    iqr = q3 - q1
    low = q1 - 1.5*iqr
    up = q3 + 1.5*iqr
    return df.filter((h_df[col] < low) & (h_df[col] > up)).count()

#step 4

def performOutlier(datatypes,df):
    OutlierList = []
    for c in datatypes:
        if((c[1] == 'int') | (c[1] == 'double') | (c[1] == 'float')):
            value = OutlierDetection(c[0],df)
            OutlierList.append((c[0],value))
    df_new = spark.createDataFrame(OutlierList)
    return df_new

outlierDf = performOutlier(datatypes,h_df)

outlierDf.show()


#Need to trace the below code
#====================================================================================

# In[320]:


df_app = spark.read.csv("D:\\Telco\\appl_stock.csv",inferSchema=True,header=True)


# In[322]:


df_app.head(1)


# In[327]:


#df_app.select(['Date','Open']).show()


# In[328]:


from pyspark.sql.functions import dayofmonth, hour,dayofyear,month,year,weekofyear,format_number,date_format


# In[339]:


#df_app.select(dayofmonth(df_app['Date'])).show()


# In[351]:# In[320]:


df_app = spark.read.csv("D:\\Telco\\appl_stock.csv",inferSchema=True,header=True)


# In[322]:


df_app.head(1)


# In[327]:


#df_app.select(['Date','Open']).show()


# In[328]:


from pyspark.sql.functions import dayofmonth, hour,dayofyear,month,year,weekofyear,format_number,date_format


# In[339]:


#df_app.select(dayofmonth(df_app['Date'])).show()


# In[351]:


#df_app.select(year(df_app['Date'])).show()
newdf = df_app.withColumn('Year',year(df_app['Date']))


# In[356]:


result = newdf.groupBy('Year').mean().select(['Year','avg(Close)'])


# In[357]:


result.select(['Year',format_number('avg(Close)',2).alias("Avg Close")]).show()



#df_app.select(year(df_app['Date'])).show()
newdf = df_app.withColumn('Year',year(df_app['Date']))


# In[356]:


result = newdf.groupBy('Year').mean().select(['Year','avg(Close)'])


# In[357]:


result.select(['Year',format_number('avg(Close)',2).alias("Avg Close")]).show()

#==============================
#==============================

#EDA By Kumarjit
===========================


h_df1.describe('churn').show()

kk= h_df1.summary().repartition(1)

kk.write.save("C:\\Users\\inkpathak\\Desktop\\Hackathon\\Data\\descriptiveStatistics.csv", format="csv", header = True)

#Find the number of distinct levels in selected coluns
h_df1.select('voice_mail_plan').distinct().count()

# showing the number of distinct levels in selected columns
h_df1.select('voice_mail_plan').distinct().show()

# Cross tabulation
h_df1.crosstab('Age', 'churn').show()

# let's sort it
h_df1.crosstab('Age', 'churn').sort('Age_churn', assending = True).show() # 'Age_churn' is in the output table and hence the sorting sriteria should be on the same


# What if I want to drop the all rows with null value
h_df1.dropna().count()

# h_df1.fillna(-1).show(2)  # this will replace all the values with a constant
h_df1.na.fill({'age': 50, 'area code': 'unknown'}).show() # this operation can be done by selected columns

==========================================================================================================================
#--- From Nitish

===============================================================================================================
===============================================================================================================

### Import required library
from pyspark.sql import SparkSession
from pyspark import SparkContext, SparkConf
from pyspark.sql import SQLContext
import pandas as pd

from pyspark.ml import Pipeline
from pyspark.ml.classification import RandomForestClassifier
from pyspark.ml.feature import IndexToString, StringIndexer, VectorIndexer
from pyspark.ml.evaluation import MulticlassClassificationEvaluator

sc = SparkContext.getOrCreate()
sqlContext = SQLContext(sc)
================================================================================================================

### Import dataet & explore

from pyspark.sql import SQLContext
sqlContext = SQLContext(sc)

# Import csv file as dataframe 
churn_raw = sqlContext.read.csv("C:/D_DRIVE/descktop/Spark_project/churn_raw_data_v3.csv",inferSchema =True, header=True)

# Displays the content of the DataFrame to stdout
churn_raw.show()

# Print the schema in a tree format
churn_raw.printSchema()

# Select only the "name" column
churn_raw.select("churn").show()

# Select people older than 21
churn_raw.filter(churn_raw.Age > 40).show()

# Count people by age
churn_raw.groupBy("churn").count().show()
====================================================================================================================
### Data Cleaning
# drop na value
churn_raw_v2 = churn_raw.na.drop()
churn_raw_v2.groupBy("churn").count().show()

#dropping duplicates from the dataframe
churn_raw_v2.dropDuplicates().show()

# Remove NULL value
churn_raw_v2.filter(churn_raw_v2.churn != '')
=======================================================================================================

# Remove outliers from dataframe

# Index labels, adding metadata to the label column.
# Fit on whole dataset to include all labels in index
# var transformation :: Gender , international_plan, 
# 
Gender_indexer = StringIndexer(inputCol="Gender", outputCol="Gender_cat")
churn_raw_v2 = Gender_indexer.fit(churn_raw_v2).transform(churn_raw_v2)

#
plan_indexer = StringIndexer(inputCol="international_plan", outputCol="plan_category")
churn_raw_v2 = plan_indexer.fit(churn_raw_v2).transform(churn_raw_v2)

====================================================================================================

#### Data prepration part-1
# Replace blank value NONE
from pyspark.sql.functions import * 
churn_raw_v2 = churn_raw_v2.replace('','0','Extension').alias('Extension') 

====================================================================================

###
#### Data prepration part-1
churn_raw_v2.createOrReplaceTempView("churn_table")

churn_raw_v3 = sqlContext.sql("select churn, Age,account_length, account_days,\
                              total_night_calls, customer_service_calls \
                              from churn_table")

churn_raw_v3.printSchema() 
churn_raw_v3.show()

===============================================================================

# Split the data into training and test sets (30% held out for testing)
(trainingData, testData) = churn_raw_v3.randomSplit([0.7, 0.3])

=============================================================================

###
#### Data prepration part-2
from pyspark.mllib.linalg import Vectors
from pyspark.mllib.regression import LabeledPoint

data_train_rdd = trainingData.rdd
type(data_train_rdd)
data_test_rdd = testData.rdd
type(data_test_rdd)

===========================================================

###
transformed_train_df = data_train_rdd.map(lambda row: LabeledPoint(row[0], Vectors.dense(row[1:])))
transformed_test_df = data_test_rdd.map(lambda row: LabeledPoint(row[0], Vectors.dense(row[1:])))

============================================================

# Target is boolean type
from pyspark.mllib.tree import RandomForest
RANDOM_SEED = 10904
RF_NUM_TREES = 100


###
model = RandomForest.trainClassifier(transformed_train_df, numClasses=2, categoricalFeaturesInfo={}, 
                                     numTrees=RF_NUM_TREES, featureSubsetStrategy="auto",
                                     impurity="gini", maxDepth=4, maxBins=32, seed=RANDOM_SEED)
===================================================================

predictions = model.predict(transformed_train_df.map(lambda x: x.features))
labels_and_predictions = transformed_train_df.map(lambda x: x.label).zip(predictions)
acc = labels_and_predictions.filter(lambda x: x[0] == x[1]).count() / float(transformed_train_df.count())
print("Model accuracy: %.3f%%" % (acc * 100))

=====================================================================

predictions = model.predict(transformed_test_df.map(lambda x: x.features))
labels_and_predictions = transformed_test_df.map(lambda x: x.label).zip(predictions)
acc = labels_and_predictions.filter(lambda x: x[0] == x[1]).count() / float(transformed_test_df.count())
print("Model accuracy: %.3f%%" % (acc * 100))
=====================================================================

from pyspark.mllib.evaluation import BinaryClassificationMetrics

metrics = BinaryClassificationMetrics(labels_and_predictions)
print("Area under Precision/Recall (PR) curve: %.f" % (metrics.areaUnderPR * 100))
print("Area under Receiver Operating Characteristic (ROC) curve: %.3f" % (metrics.areaUnderROC * 100))

=================================================================

# Nitish code end here



#References

===========================================================================================================================
#https://stackoverflow.com/questions/40161879/pyspark-withcolumn-with-two-conditions-and-three-outcomes

from pyspark.sql.functions import col, expr, when

new_column_1 = expr(
    """IF(fruit1 IS NULL OR fruit2 IS NULL, 3, IF(fruit1 = fruit2, 1, 0))"""
)
(df
    .withColumn("new_column_1", new_column_1)
    .withColumn("new_column_2", new_column_2)
    .withColumn("new_column_3", new_column_3))


# Joining al five tables togeather
h_df = (h_cust_churn.join(h_cdr, h_cust_churn["phone number"] == h_cdr["phone number"])
        .join(h_complaint, h_cust_churn["phone number"] == h_complaint["phone number"])
        .join(h_revenue, h_cust_churn["phone number"] == h_revenue["phone number"])
        .join(h_signal, h_cust_churn["phone number"] == h_signal["phone number"]))

#http://kirillpavlov.com/blog/2016/04/23/beyond-traditional-join-with-apache-spark/
h_df = churn.join(cdr,"phone_number").join(complaint,"phone_number").join(revenue,"phone_number").join(signal,"phone_number")
  .show()


=============================================================================================================================


