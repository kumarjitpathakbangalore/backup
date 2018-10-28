GSS<-read.csv("E:\\Hayatt\\new data\\hyatt-survey.csv",header=F, sep="\t")
gss1<-read.csv("E:\\Hayatt\\new data\\hyatt_sample.csv",header=F, sep="\t")

hyattgss <- read.delim("E:\\Hayatt\\new data\\hyatt-dtl.csv", header=FALSE, quote="")
str(hyatt)
gss1<-read.csv("E:\\Hayatt\\teamwork\\social media kumarjit\\NYCGH.csv",header=T)

write.csv(hyatt,"E:\\Hayatt\\new data\\Hyatt_11000.csv")


gsssat<-hyattgss[,c(84,98,99,100,108,368,369,370,372,375,377,391,392,396,406,411,413,414,415,416,,417,418,421,422,423,520,530,536,537,538,539,540,541,542,543,544,545,546,547,548,549,550,551,552,553,554,555,557)]


Hyattgsssatisfaction <- subset(hyattgss,  
                               select=c(
                                 "GSS_SURVEY_ID",
                                 "PROPERTY_CODE",
                                 "Q_HY_GSS_ARRIVAL_SAT_CHECKIN",
                                 "Q_HY_GSS_CLUBLOUNGE_FB",
                                 "Q_HY_GSS_CLUBLOUNGE_SAT",
                                 "Q_HY_GSS_CLUBLOUNGE_SERVICE",
                                 "Q_HY_GSS_CONDITION_SAT",
                                 "Q_HY_GSS_GPMODULE_ACKNOWLEDGEMENT_OF_STATUS_SAT_EVAL11",
                                 "Q_HY_GSS_GPMODULE_COMP_BREAKFAST_SAT_EVAL11",
                                 "Q_HY_GSS_GPMODULE_DIAMOND_AMENITY_SAT_EVAL11",
                                 "Q_HY_GSS_GPMODULE_ENHANCEDSTAY_EVAL11",
                                 "Q_HY_GSS_GPMODULE_LTR_EVAL11",
                                 "Q_HY_GSS_GPMODULE_ROOM_UPGRADE_SAT_EVAL11",
                                 "Q_HY_GSS_MODULE_GOLF_CONDITION",
                                 "Q_HY_GSS_MODULE_GOLF_CUSTOMER_SERVICE",
                                 "Q_HY_GSS_MODULE_GOLF_VALUE",
                                 "Q_HY_GSS_MODULE_REC_GOLF",
                                 "Q_HY_GSS_MODULE_SPA_REC",
                                 "Q_HY_GSS_MODULE_SPA_REQUEST",
                                 "Q_HY_GSS_MODULE_SPA_SAT_APPT_AVAILABILITY",
                                 "Q_HY_GSS_MODULE_SPA_SAT_CLEANLINESS",
                                 "Q_HY_GSS_MODULE_SPA_SAT_CUSTOMER_SERVICE",
                                 "Q_HY_GSS_MODULE_SPA_SAT_ONLINEBOOKING",
                                 "Q_HY_GSS_MODULE_SPA_SAT_OVEXP",
                                 "Q_HY_GSS_OVERALL_RECOMMEND",
                                 "Q_HY_GSS_OVERALL_REPEAT_BIZ",
                                 "Q_HY_GSS_OVERALL_SAT",
                                 "Q_HY_GSS_ROOM_SUITE_OVSAT",
                                 "Q_HY_GSS_ROOM_TRANQ_SAT",
                                 "Q_HY_GSS_SLOT_SCORE_MENUVARIETY_1",
                                 "Q_HY_GSS_SLOT_SCORE_MENUVARIETY_2",
                                 "Q_HY_GSS_SLOT_SCORE_MENUVARIETY_3",
                                 "Q_HY_GSS_SLOT_SCORE_MENUVARIETY_4",
                                 "Q_HY_GSS_SLOT_SCORE_MENUVARIETY_5",
                                 "Q_HY_GSS_SLOT_SCORE_OV1",
                                 "Q_HY_GSS_SLOT_SCORE_OV2",
                                 "Q_HY_GSS_SLOT_SCORE_OV3",
                                 "Q_HY_GSS_SLOT_SCORE_OV4",
                                 "Q_HY_GSS_SLOT_SCORE_OV5",
                                 "Q_HY_GSS_SLOT_SCORE_QUALITY1",
                                 "Q_HY_GSS_SLOT_SCORE_QUALITY2",
                                 "Q_HY_GSS_SLOT_SCORE_QUALITY3",
                                 "Q_HY_GSS_SLOT_SCORE_QUALITY4",
                                 "Q_HY_GSS_SLOT_SCORE_QUALITY5",
                                 "Q_HY_GSS_SLOT_SCORE_SERVICE1",
                                 "Q_HY_GSS_SLOT_SCORE_SERVICE2",
                                 "Q_HY_GSS_SLOT_SCORE_SERVICE3",
                                 "Q_HY_GSS_SLOT_SCORE_SERVICE4",
                                 "Q_HY_GSS_SLOT_SCORE_SERVICE5",
                                 "Q_HY_GSS_STAFF_CARED"
                               ))
Hyattgsssatisfaction[Hyattgsssatisfaction == "NULL"]<-0


head(Hyattgsssatisfaction)


gssh<-unique(Hyattgsssatisfaction)

write.csv(gssh,"E:\\Hayatt\\teamwork\\social media kumarjit\\GSS\\Hyattsat.csv")







n <- nrow(hyattgss)
n1 <- floor(0.25*n)
dat1 <- sample(1:n,n1)

?write.csv

sample_data <-hyatt[dat1,] 

write.csv(sample_data,"E:\\Hayatt\\new data\\Hyatt_check1.csv", header=F,sep=',')

nrow(sample_data)

data <- read.table("data.csv", sep="\t")
colnames(GSS)

str(dt)

_______________________________________________________________________________________________

rm(variable) # to remove file
rm(hyatt)
rm(dt)
rm(gss1)


gssheader<-read.csv("E:\\Hayatt\\new data\\GSS HEADER.csv",header=F)


colnames(hyattgss) <- gssheader$V1 # this is how you can change the variable name 
str(hyattgss)



dump("hyattgss", "E:\\Hayatt\\new data\\GSS_total.Rdmpd") # to save data in r format

source("E:\\Hayatt\\new data\\GSS_total.Rdmpd") # to loa back the data in R

# Save a single object in binary RDS format
saveRDS(hyattgss, "E:\\Hayatt\\new data\\GSS_total.rds")
??saveRDS

# To load the data again:
gssdata <- readRDS("E:\\Hayatt\\new data\\GSS_total.rds")
str(gssdata)
colnames(gssdata)
rm(data)

gssdata$checkin<- 

table(gssdata$Q_HY_GSS_ARRIVAL_DISSAT_DELAY)
# subsetting the data only for 
Hyattgsssatisfaction <- subset(hyattgss, RESERVATION_ID >0, select=c("RESERVATION_ID","CG_GENDER","CG_HOME_ADDR_CITY","CG_HOME_ADDR_STATE","CG_HOME_ADDR_ZIP_CD","CG_HOME_ADDR_COUNTRY_CD","CG_BUS_ADDR_CITY","CG_BUS_ADDR_STATE","CG_BUS_ADDR_ZIP_CD","CG_BUS_ADDR_COUNTRY_CD","CG_GP_NUM","CHECK_IN_DATE","CHECK_OUT_DATE","RESERVATION_DATE","DIRECT_POV_CODE","INDIRECT_POV_CODE","CANCELLATION_DATE","PMS_OTHER_REV_USD","PMS_TOTAL_REV_USD","PR_ROOM_REV_USD","NUMBER_OF_ROOMS","ROOM_NUM","ADULT_NUM","CHILDREN_NUM","PMS_ROOM_REV_USD","PR_FOOD_BEVERAGE_REV_USD","PR_OTHER_REV_USD","PR_TOTAL_REV_USD","STAY_DATE",  "OFFER_CODE_FIRST_LETTER",  "PMS_RATE_CATEGORY_CODE",  "TRAVEL_AGENT_NAME",  "GOLD_PASSPORT_NUM",  "LENGTH_OF_STAY",  "VIP_STATUS",  "STATUS_CALCULATION",  "MEMBER_STATUS",  "CONFIRMATION_NUM"))  #select=c(var1, var2)



# Just to extract only satisfaction related questions:
k<-c(8,13,17,84,98,99,100,108,368,369,370,372,375,377,391,392,396,406,411,413,414,415,416,417,418,421,422,423,520,530,536,537,538,539,540,541,542,543,544,545,546,547,548,549,550,551,552,553,554,555,557)

Hyattgsssatisfaction<- hyattgss[,k]
str(Hyattgsssatisfaction)

pie(table(Hyattgsssatisfaction$Q_HY_GSS_OVERALL_SAT))

table(Hyattgsssatisfaction$PROPERTY_CODE)
table(Hyattgsssatisfaction$POV_CODE)

table(Hyattgsssatisfaction$Q_HY_GSS_OVERALL_SAT)
Hyattgsssatisfaction$Q_HY_GSS_OVERALL_SAT<-as.numeric(Hyattgsssatisfaction$Q_HY_GSS_OVERALL_SAT)

library(sqldf)
attach(Hyattgsssatisfaction)
test<-sqldf("select PROPERTY_CODE, avg(Q_HY_GSS_OVERALL_SAT) as Avg_satisfaction
            from Hyattgsssatisfaction
            group by PROPERTY_CODE")

colnames(Hyattgsssatisfaction)

test1<-aggregate(Hyattgsssatisfaction$Q_HY_GSS_OVERALL_SAT, by=list(Hyattgsssatisfaction$PROPERTY_CODE), FUN=mean,na.rm=TRUE)
aggdata <-aggregate(mtcars, by=list(cyl,vs), 
                    FUN=mean, na.rm=TRUE)





