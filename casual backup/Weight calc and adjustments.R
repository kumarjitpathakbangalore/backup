rm(list=ls(all=TRUE))

##########################################################################
##                        REQUIRED LIBRARIES                            ##
##########################################################################

library(RODBC)
library(foreign)
library(plyr)
library(gdata)
library(data.table)
library(sqldf)
library(stringr)
#library(moments)
library(anesrake)
#library(rococo)

#---------------------------------------Universal Estimates--------------------------------------#
gender <- c(0.4816, 0.5184)
agegroup <- c(0.065103, 0.062385, 0.1760, 0.1693, 0.110958, 0.074424, 0.3319) # 0.1620, 0.1799)
#education <- c(0.1289, 0.2879, 0.2975, 0.1835, 0.1022)

education <- c(0.052379817, 0.074683388, 0.2560356, 0.28337562, 0.215300914, 0.118224661)
income <- c(0.4166, 0.3185, 0.2648)
region <- c(0.1818, 0.2161, 0.3747, 0.2274)
race <- c(0.7417, 0.1188, 0.0294, 0.0121, 0.0985)
hispanic <- c(0.1490, 0.8510)
employment <- c(0.4831, 0.1181, 0.3989)
marital <- c(0.5335, 0.2759, 0.1906)

#-------------------------------------------------------------------------------------------------#
#                                       Reading the datafile                                      #
#-------------------------------------------------------------------------------------------------#

#####setwd("D:\\03 SAM 2\\10 Seema\\07. Bill\\Removing Refused and DK\\Weight Comparison Ascribed Vs. Refused\\weight comparison\\Ascibed Method\\Wk 201519")

setwd("C:\\Users\\inkpathak\\Desktop\\SAM\\KPShishir sample syndication")

#channel <- odbcConnect("verticadsn1", uid = "repuser", pwd = "Cupertino1")  
#datafile <- sqlQuery(channel, paste("SELECT USERS_META_ID, 
#                                      PANELISTID_QUESTION, AESDECRYPT(EMAIL ,'pass') EMAIL, REWARD_CHOICE, SURVEY_NAME, STATE, REGION, CENSUS_DIV, MM_STATUS, FB_TOKEN, TW_TOKEN, NUMBEROFDEVICES, NUMBEROFACTIVEDEVICES,
#                                      DEVICETYPE1, DEVICETYPE2, DEVICETYPE3, DEVICESTYLE1, DEVICESTYLE2, DEVICESTYLE3, CURRENT_AGE, Q4_GENDER, Q12_MARITALSTATUS, Q13_RACE, Q14_HISPANICID, Q15_EDUCATION, Q16_EMPLOYMENT, Q17_INCOME,
#                                      Q5_HOUSEHOLD_SIZE, Q7_PARENT, Q8_CHILDREN_YOUNGER11, Q9_CHILDREN_12TO17, Q20_1_WIRELESS, Q23_LANDLINE, Q24_PHONEUSAGE, AGEGROUP, SRC, IS_QUALIFIED, MMACTIVEWEEK,
#                                      MMACTIVEACRWEEK, MMACRACTIVEROLLING4WEEKS, MOBILEACTIVEWEEK, ONLINEACTIVEWEEK, EVERACTIVE, WEEK_ID, TV_INTAB_FLAG FROM SAM_PII_ENC.WEEKLY_WEIGHTS_INPUT_FINAL;"))


lines <- readLines("weights_input_2015-12-07 - sample syndication.csv")
#weights_input_2015-05-04

lines <- gsub('([^,])"([^,])', '\\1""\\2', lines)
datafile <- read.csv(textConnection(lines))

datafile <- data.frame(datafile)

#------------------------Subsetting dataset-----------------------------#

attach(datafile)
df <- datafile[MMACTIVEACRWEEK == 1, c("USERS_META_ID", "Q4_GENDER", "CURRENT_AGE", "REGION", "Q13_RACE", "Q16_EMPLOYMENT", "Q12_MARITALSTATUS",
                                       "Q14_HISPANICID", "Q17_INCOME", "Q15_EDUCATION", "Q5_HOUSEHOLD_SIZE", "Q8_CHILDREN_YOUNGER11", 
                                       "Q9_CHILDREN_12TO17", "Week_ID", "TV_INTAB_FLAG")] #-------change to Week_ID when reading from file------------#

df2 <- datafile[MMACTIVEACRWEEK == 1, c("USERS_META_ID", "Q4_GENDER", "CURRENT_AGE", "REGION", "Q13_RACE", "Q16_EMPLOYMENT", "Q12_MARITALSTATUS",
                                        "Q14_HISPANICID", "Q17_INCOME", "Q15_EDUCATION")]

intab_tab <- datafile[MMACTIVEACRWEEK == 1,c("USERS_META_ID", "TV_INTAB_FLAG")]
detach(datafile)

nrow(df)

name <- noquote(names(df2))

prop_tab <- NULL

prop_tab <- do.call("rbind", sapply(1:ncol(df2), FUN = function(i) c( name[i], nobs(df2[ ,i]), length(df2[ ,i]), nobs(df2[ ,i])/length(df2[ ,i])), simplify = FALSE))
prop_tab <- data.frame(prop_tab)

names(prop_tab)[1] <- "Demography_Name"
names(prop_tab)[2] <- "Non-Missing_values"
names(prop_tab)[3] <- "Total_counts"
names(prop_tab)[4] <- "Ratio"

#------Removing variable which has less than 50% counts ----------#

prop_tab$Ratio <- as.numeric(as.character(prop_tab$Ratio))

prop_tab$flag <- ifelse(prop_tab$Ratio < 0.5, 1, 0)           #<---------------------Change 0.8 to 0.5#

myvars_u <- prop_tab[which(prop_tab$flag == 0), 1]

#----------------------------------------------Using only those variables in the----------------------------------------------------#

equiv <- data.frame(datvar = c("USERS_META_ID", "Q4_GENDER", "CURRENT_AGE", "REGION", "Q13_RACE", "Q16_EMPLOYMENT", "Q12_MARITALSTATUS", "Q14_HISPANICID", "Q17_INCOME", "Q15_EDUCATION", "Q5_HOUSEHOLD_SIZE", "Q8_CHILDREN_YOUNGER11", "Q9_CHILDREN_12TO17"), 
                    usevar = c("USERS_META_ID", "gender", "agegroup", "region", "race", "employment", "marital", "hispanic", "income", "education", "hh", "child11", "child17"), stringsAsFactors=F )

#--------------------------------#--------------------------------#--------------------------------#--------------------------------#
#---------------------------------------------Subset the dataset based on above condition-------------------------------------------#
#                                             Include Code here for subsetting the dataset                                          #
#--------------------------------#--------------------------------#--------------------------------#--------------------------------#

write.csv(prop_tab, "Prob_tab.csv", row.names = F)

#Treating Missing values
df[is.na(df)] <- 99

attach(df)
GEN <- table(Q4_GENDER)
AGE <- table(CURRENT_AGE)
REG <- table(REGION)
RAC <- table(Q13_RACE)
EMP <- table(Q16_EMPLOYMENT)

MAR <- table(Q12_MARITALSTATUS)
HIS <- table(Q14_HISPANICID)
INC <- table(Q17_INCOME)
EDU <- table(Q15_EDUCATION)
HOU <- table(Q5_HOUSEHOLD_SIZE)
detach(df)

#--Class-wise observation
arr_class <- c("GENDER", GEN, "AGEGROUP", AGE, "REGION", REG, "RACE", RAC, "EMPLOYMENT", EMP, "MARITALSTATUS", MAR, "HISPANIC", HIS, "INCOME", INC, "EDUCATION", EDU)

#arr_class1 <- c(GEN, "GENDER", AGE, "AGEGROUP", REG, "REGION")

write.csv(arr_class, "arr_class.csv", row.names = F)

# Age re-classified
df$New_Age <- ifelse(df$CURRENT_AGE > 17 & df$CURRENT_AGE <= 21, 1,
                     ifelse(df$CURRENT_AGE > 21 & df$CURRENT_AGE <= 24, 2,
                            ifelse(df$CURRENT_AGE > 24 & df$CURRENT_AGE <= 34, 3,
                                   ifelse(df$CURRENT_AGE > 34 & df$CURRENT_AGE <= 44, 4,
                                          ifelse(df$CURRENT_AGE > 44 & df$CURRENT_AGE <= 49, 5,
                                                 ifelse(df$CURRENT_AGE > 49 & df$CURRENT_AGE <= 54, 6, 7))))))
####                                                        ifelse(df$CURRENT_AGE > 54 & df$CURRENT_AGE <= 64, 7,8)))))))

#df$New_Age <- ifelse(df$CURRENT_AGE > 17 & df$CURRENT_AGE <= 24, 1,
#                     ifelse(df$CURRENT_AGE > 24 & df$CURRENT_AGE <= 34, 2,
#                            ifelse(df$CURRENT_AGE > 34 & df$CURRENT_AGE <= 44, 3,
#                                   ifelse(df$CURRENT_AGE > 44 & df$CURRENT_AGE <= 54, 4,
#                                          ifelse(df$CURRENT_AGE > 54 & df$CURRENT_AGE <= 64, 5,6)))))

df$New_Income <- ifelse(df$Q17_INCOME > 0 & df$Q17_INCOME <= 5, 1,
                        ifelse(df$Q17_INCOME > 5 & df$Q17_INCOME <= 7, 2, 
                               ifelse(df$Q17_INCOME > 7 & df$Q17_INCOME <= 11, 3, 99)))


#Education variable recoded:
#df$New_Education <- ifelse(df$Q15_EDUCATION > 0 & df$Q15_EDUCATION <= 2, 1,
#                           ifelse(df$Q15_EDUCATION == 3, 2,
#                                  ifelse(df$Q15_EDUCATION == 4, 3, 
#                                         ifelse(df$Q15_EDUCATION == 5, 4,
#                                                ifelse(df$Q15_EDUCATION == 6, 5, 99)))))


df$New_Education <- ifelse(df$Q15_EDUCATION == 1, 1,
                           ifelse(df$Q15_EDUCATION == 2, 2,
                                  ifelse(df$Q15_EDUCATION == 3, 3, 
                                         ifelse(df$Q15_EDUCATION == 4, 4,
                                                ifelse(df$Q15_EDUCATION == 5, 5,
                                                       ifelse(df$Q15_EDUCATION == 6, 6,99))))))

#Race variable recoded:
df$New_Race <- ifelse(df$Q13_RACE == 1, 1, ifelse(df$Q13_RACE == 2, 2, 
                                                  ifelse(df$Q13_RACE == 3, 3, 
                                                         ifelse(df$Q13_RACE == 4, 4, 
                                                                ifelse(df$Q13_RACE == 5, 5, 
                                                                       ifelse(df$Q13_RACE == 6, 99, 
                                                                              ifelse(df$Q13_RACE == 7, 3, 
                                                                                     ifelse(df$Q13_RACE == 8, 3, 99))))))))

#HispanicID variable recoded:
df$New_Hispanic <- ifelse(df$Q14_HISPANICID == 1, 1,
                          ifelse(df$Q14_HISPANICID == 2, 2, 99))

#MARITAL STATUS variable recoded:
df$New_Marital <- ifelse(df$Q12_MARITALSTATUS <= 2 , 1,
                         ifelse(df$Q12_MARITALSTATUS > 2 & df$Q12_MARITALSTATUS <= 5, 2, 
                                ifelse(df$Q12_MARITALSTATUS == 6, 3, 99)))

#EMPLOYMENT variable recoded:
df$New_EMPLOYMENT <- ifelse(df$Q16_EMPLOYMENT == 1, 1, 
                            ifelse(df$Q16_EMPLOYMENT == 2, 2,
                                   ifelse(df$Q16_EMPLOYMENT > 2 & df$Q16_EMPLOYMENT <=5, 3, 99)))   #<-------3 == not working etc.


#-------------------------------------HOUSEHOLD RECODING IS REQUIRED-----------------------------------#
#                             Coding is not present in file shared by Shikha                           #
#-------------------------------------HOUSEHOLD RECODING IS REQUIRED-----------------------------------#


#Renaming Variables
names(df)[2] <- "gender"
names(df)[4] <- "region"
names(df)[16] <- "agegroup"
names(df)[17] <- "income"
names(df)[18] <- "education"
names(df)[19] <- "race"
names(df)[20] <- "hispanic"
names(df)[21] <- "marital"
names(df)[22] <- "employment"


#...gender, agegroup and region has no rejected values...#

myvars <- equiv[which(equiv$datvar %in% myvars_u), 2]
subsetdat <- data.frame(df[myvars])

subsetdat1 <- subsetdat
subsetdat1[subsetdat1 == 99] <- NA
subsetdat1$flag1 <- complete.cases(subsetdat1)

subsetdat1$ascrb_flag <- ifelse(subsetdat1$flag == FALSE, 1, 0)


ascribedusers <- subsetdat1[ ,c(1, 12)]

# ------------- This will keep only those variables which has 99 values -------------------#
chk99 <- do.call("rbind", sapply(1:ncol(subsetdat), FUN = function(i) c(myvars[i], length(which(subsetdat[i] == 99))), simplify = FALSE))
chk99 <- data.frame(chk99)

names(chk99)[1] <- "Demography_Name"
names(chk99)[2] <- "count_99"

MyVars <- chk99[which(chk99[2] != 0), 1]


#--------Calculating Goodman Gamma coefficient for association

subset <- subsetdat[ ,-1:-4]
g <- cor(subset)

#goodman <- function(x,y){
#  Rx <- outer(x,x,function(u,v) sign(u-v))
#  Ry <- outer(y,y,function(u,v) sign(u-v))
#  S1 <- Rx*Ry
#  return(sum(S1)/sum(abs(S1)))}

#subsetdat1 <- data.frame(df[, (names(df) %in% MyVars)])


#Output matrix
#g <- matrix(0, length(MyVars),length(MyVars)) # Create an empty matrix to assign the logical argurment
#colnames(g) <- MyVars
#rownames(g) <- t(MyVars)

#ncol1 <- ncol(subsetdat) - 1

#for (i in 1 : length(MyVars)){
#  for (j in 1 : length(MyVars)){
#    g[i,j] <- goodman(subsetdat1[, i], subsetdat1[, j])
#    cat(sprintf("(%g,%g, %f)\n", i,j,g))                  

#  }
#}
g1 <- data.frame(abs(g))
g1[g1 == 1] <- 0

write.table(g, "gama_coeff.csv", row.names = F)

freq_tab <- function(x,y){
  freq2 <- as.data.frame(table(x,y))
  return(freq2)
}

lst2 <- NULL
for (i in 1:ncol(g1)){
  mdat <- t(abs(g1[i]))
  coln <- which(mdat == max(g1[i]), arr.ind = TRUE)
  rnames = rownames(mdat)[coln[,1]]
  cnames = colnames(mdat)[coln[,2]]
  lst1 <- c(rnames, cnames)
  lst2 <- c(lst2, lst1)
}  
lst2 <- unique(lst2)
lst3 <- c("NA", lst2)

#----------------------------------------------------------------------------------------------------------------#

for (i in 1:floor(length(lst2)/2)){
  #  i <- 3  
  cnames <- lst3[2*i]
  rnames <- lst3[2*i + 1]
  cat(cnames, rnames, "\n")
  
  lst <- c("USERS_META_ID", rnames, cnames)
  
  d1 <- subsetdat[ ,names(subsetdat) %in% lst]
  cat(lst , "\n")
  
  #-----------------------------------------------------------------------------------------------------------#  
  uniq1 <- sort(unique(d1[, rnames]))
  #--------------------------Removing rejected from first variable--------------------------------------------#
  
  uniq1_resp <- uniq1[which(uniq1 != 99)]               
  sq <- sprintf("select * from df where %s = 99 or %s = 99", rnames, cnames)
  df99 <- sqldf(sq)
  
  sq4 <- sprintf("select * from df where USERS_META_ID not in (Select USERS_META_ID from df99)")
  df_99 <- sqldf(sq4)
  
  freq2 <- freq_tab(subsetdat[, rnames], subsetdat[, cnames])
  names(freq2)[1] <- rnames
  names(freq2)[2] <- cnames
  
  write.csv(freq2, gsub(" ", "", paste("crosstab_", rnames,"_",cnames,".csv")), row.names = F)
  
  #-------------------------------------------------------------------------------------------------------------------  
  if (any(freq2$Freq == 0) == TRUE) {
    
    del_class <- freq2[which(freq2[,3] == 0), 1]
    freq1 <- freq2[freq2[ ,1] != del_class, ]    
    
  } else {
    
    freq1 <- freq2
    
  }
  
  #-------------------------------------------------------------------------------------------------------------------
  
  freq1 <- as.data.frame(droplevels(freq1))
  
  #  uniq2 <- sort(unique(subsetdat[ ,cnames]))
  uniq2 <- sort(noquote(levels(freq1[,cnames])))
  
  dat <- as.data.frame(split(freq1, freq1[,2]))
  
  df_big <- NULL
  df1_big <- NULL
  for (j in 1:length(uniq2)){
    
    df1 <- NULL
    df1 <- subset(subsetdat, subsetdat[ ,rnames] == 99 & subsetdat[ ,cnames] == uniq2[j], select = c(myvars[1], rnames, cnames))
    
    #-------Put a check on length of data-------------#
    
    df1$randnum <- rep(runif(100, min=0, max=1), length.out = nrow(df1))
    df1 <- df1[order(-df1$randnum), ]
    
    dat1 <- dat[ ,c((1+3*(j-1)):(3+3*(j-1)))]
    dat1 <- dat1[order(dat1[,1]), ]
    rej <- dat1[nrow(dat1), 3]
    resp <- dat1[-nrow(dat1), ]
    resp$perc <- resp[ ,3]/sum(resp[ ,3])
    resp$cumsm <- cumsum(resp$perc)
    
    df1[,"newclass"]<-NULL
    nval<-length(resp[,1])
    df1[which(df1[,4]<=resp[,5][1]),"newclass"]<-resp[1,1]
    
    for (i in (1 : (nval-1))){
      df1[which(df1[,4]<=resp[,5][i+1]&df1[,4]>resp[,5][i]),"newclass"]<-resp[(i+1),1]
      cat(sprintf("(%g,%g)\n", i, j))
    }
    names(df1)[5] <- paste(rnames, "_n", sep = "")
    df1 <- df1[c(-2,-4)]
    names(df1)[3] <- rnames
    df1_big <- rbind(df1, df1_big)
    
    print(j)
    flush.console()
  }
  drops <- c(rnames, cnames)
  drop <- c(rnames)
  
  sq1 <- sprintf("select * from df where %s = 99", rnames)
  df99_1 <- sqldf(sq1)
  
  df99_1 <- df99_1[,!(names(df99_1) %in% drops)]
  df99_1 <- merge(x = df99_1, y = df1_big, by = "USERS_META_ID", all = TRUE)
  
  df99_1 <- cbind(df99_1[, !names(df99_1) %in% drops], df99_1[, names(df99_1) %in% drops])
  df99_1 <- cbind(df99_1[, !names(df99_1) %in% drop], df99_1[, names(df99_1) %in% drop])
  names(df99_1)[ncol(df99_1)] = rnames
  
  sq5 <- sprintf("select * from df where USERS_META_ID not in (Select USERS_META_ID from df99_1)")
  df_99_1 <- sqldf(sq5)
  
  df_99_1 <- cbind(df_99_1[, !names(df_99_1) %in% drops], df_99_1[, names(df_99_1) %in% drops])
  df_99_1 <- cbind(df_99_1[, !names(df_99_1) %in% drop], df_99_1[, names(df_99_1) %in% drop])
  names(df_99_1)[ncol(df_99_1)] = rnames
  
  df_big <- rbind(df_99_1, df99_1)
  #  }
  
  
  subsetdat <- NULL
  subsetdat <- data.frame(df_big[myvars])
  
  freq1 <- NULL
  freq2 <- NULL
  freq2 <- freq_tab(subsetdat[, rnames], subsetdat[, cnames])
  names(freq2)[1] <- rnames
  names(freq2)[2] <- cnames
  
  #-------------------------------------------------------------------------------------------------------------------  
  if (any(freq2$Freq == 0) == TRUE) {
    
    del_class <- freq2[which(freq2[,3] == 0), 1]
    freq1 <- freq2[freq2[ ,1] != del_class, ]    
    
  } else {
    
    freq1 <- freq2
  }
  #-------------------------------------------------------------------------------------------------------------------
  
  freq1 <- as.data.frame(droplevels(freq1))
  
  uniq1 <- NULL
  uniq1 <- sort(noquote(levels(freq1[,rnames])))
  #  uniq1 <- sort(unique(subsetdat[ ,rnames]))
  
  dat <- NULL
  dat <- as.data.frame(split(freq1, freq1[,1]))
  
  df1 <- NULL
  
  dat1 <- NULL
  rej <- NULL
  resp <- NULL      
  df1_big <- NULL
  
  for (l in 1:length(uniq1)){
    df1 <- subset(subsetdat, subsetdat[ ,cnames] == 99 & subsetdat[ ,rnames] == uniq1[l], select = c(myvars[1], rnames, cnames))
    #-----------------Put a check on data length------------------------------#
    df1$randnum <- rep(runif(100, min=0, max=1), length.out = nrow(df1))
    df1 <- df1[order(df1$randnum), ]
    
    dat1 <- dat[ ,c((1+3*(l-1)):(3+3*(l-1)))]
    dat1 <- dat1[order(dat1[,2]), ]
    rej <- dat1[nrow(dat1), 3]
    resp <- dat1[-nrow(dat1), ]
    resp$perc <- resp[ ,3]/sum(resp[ ,3])
    resp$cumsm <- cumsum(resp$perc)
    # resp <- resp[-order(resp$cumsm), ]
    
    df1[,"newclass"]<-NULL
    nval<-length(resp[,1])
    df1[which(df1[,4]<=resp[,5][1]),"newclass"]<-resp[1,2]
    
    for (i in (1 : (nval-1))){
      df1[which(df1[,4]<=resp[,5][i+1]&df1[,4]>resp[,5][i]),"newclass"]<-resp[(i+1),2]
      cat(sprintf("(%g,%g)\n", l, i))
    }  
    
    names(df1)[5] <- paste(cnames, "_n", sep = "")
    df1 <- df1[c(-3, -4)]
    names(df1)[3] <- cnames
    
    
    df1_big <- rbind(df1, df1_big) 
    #df1_big[,5][is.na(df1_big[,5])] <- resp[,1][i]
    
    print(l)
    flush.console()
  }    
  #-------------------------------------------------------------------------------------------------------#
  drops <- NULL
  drop <- NULL
  drops <- c(cnames, rnames)
  drop <- c(cnames)
  
  sq2 <- sprintf("select * from df_big where %s = 99", cnames)
  df99_2 <- sqldf(sq2)
  
  df99_2 <- df99_2[,!(names(df99_2) %in% drops)]
  df99_2 <- merge(x = df99_2, y = df1_big, by = "USERS_META_ID", all = TRUE)
  
  df99_2 <- cbind(df99_2[, !names(df99_2) %in% drops], df99_2[, names(df99_2) %in% drops])
  df99_2 <- cbind(df99_2[, !names(df99_2) %in% drop], df99_2[, names(df99_2) %in% drop])
  names(df99_2)[ncol(df99_2)] = cnames
  
  sq6 <- sprintf("select * from df_big where USERS_META_ID not in (Select USERS_META_ID from df99_2)")
  df_99_2 <- sqldf(sq6)
  
  df_99_2 <- cbind(df_99_2[, !names(df_99_2) %in% drops], df_99_2[, names(df_99_2) %in% drops])
  df_99_2 <- cbind(df_99_2[, !names(df_99_2) %in% drop], df_99_2[, names(df_99_2) %in% drop])
  names(df_99_2)[ncol(df_99_2)] = cnames
  
  df_big1 <- rbind(df_99_2, df99_2)
  
  df <- df_big1
}

write.csv(df, "modtab.csv", row.names = F)

#----------------------------------------------Weighting process---------------------------------------------#

#wkvec <- unique(df$Week_ID)

dfn <- df[, names(df) %in% myvars]
dfn <- dfn[, -1]

dfn <- as.data.frame(as.matrix(sapply(dfn, as.numeric)))

tab <- apply(dfn, 2, table)

#write.table(tab, "ascribe_tab.txt", sep = '\t')
names(df)


attach(df)
df_flag1 <- df[TV_INTAB_FLAG == 1, ]
#df_flag3 <- df[TV_INTAB_FLAG == 3, ]
detach(df)

weekID <- df[1,14]

df1 <- merge(df,ascribedusers, by = "USERS_META_ID")

write.csv(df_flag1, paste("Modified_datafile", "_", weekID, ".csv"), row.names = F)

tabGender <- prop.table(table(df$gender))
tabagegroup <- prop.table(table(df$agegroup))
tabRegion <- prop.table(table(df$region))
tabRace <- prop.table(table(df$race))
tabHispanic <- prop.table(table(df$hispanic))
tabIncome <- prop.table(table(df$income))
tabEducation <- prop.table(table(df$education))
tabMarital <- prop.table(table(df$marital))
tabEmployment <- prop.table(table(df$employment))

#----------------------------------------------------------------------------------------------------------------------------------#
#                                                    RAKE WEIGHT CALCULATION                                                       #
#----------------------------------------------------------------------------------------------------------------------------------#

#df_flag3 <- as.data.frame(as.matrix(sapply(df_flag3, as.numeric)))

#create unique id


#df_flag3$caseid <- 1:length(df_flag3$USERS_META_ID)


#create list
targets <- list(gender, agegroup, region, income, race, education, employment, hispanic, marital)
#names(targets) <- c("gender", "agegroup", "region", "income", "race","education", "employment", "hispanic", "marital")

#-----------------------------------------------------------------------------------------------------------------------------#
#weight
#outsave <- anesrake(targets, df_flag3, caseid = df_flag3$caseid, weightvec = NULL, cap = 30, verbose = FALSE, maxit = 1000, choosemethod = "total", type = "nolim", pctlim = 0.05, iterate = T, force1 = TRUE)
#-----------------------------------------------------------------------------------------------------------------------------#

#produce summary stats
#summarydata <- summary(outsave)
#write out summary stats to a csv, provide this csv as qa file 
#capture.output(summarydata, file = "summary_stats.csv")

#append weights to data file
#df_flag3$weightvec <- unlist(outsave[1])

#df_flag3_3 <- df_flag3_1[ ,c(1,24,14,15)]

#############################################################################################

#minwt <- 0.3

#df_flag3$chngwt <- ifelse(df_flag3$weightvec <= minwt, minwt, df_flag3$weightvec)
#diff = nrow(df_flag3) - sum(df_flag3$chngwt)
#delta = diff / nrow(df_flag3)

#df_flag3$chngwt1 <- df_flag3$chngwt + delta

###################################################################################################

#-----------------------------------------------------------FLAG1---------------------------------#

df_flag1$caseid <- 1:length(df_flag1$USERS_META_ID)

#-------------------------------------------------------------------------------------------------#
#weight
outsave_flag1 <- anesrake(targets, df_flag1, caseid = df_flag1$caseid, weightvec = NULL, cap = 30, verbose = TRUE, maxit = 1000, choosemethod = "total", type = "nolim", pctlim = 0.05, iterate = T, force1 = TRUE)
#-----------------------------------------------------------------------------------------------------------------------------#

#produce summary stats
summarydata_flag1 <- summary(outsave_flag1)
#write out summary stats to a csv, provide this csv as qa file 
capture.output(summarydata_flag1, file = "summary_stats_flag1.csv")

#append weights to data file
df_flag1$weightvec <- unlist(outsave_flag1[1])

#df_flag1_1 <- df_flag1[ ,c(1,24,14,15)]

#dffile <- rbind(df_flag1_1, df_flag3_3)

minwt <- 0.3

df_flag1$chngwt <- ifelse(df_flag1$weightvec <= minwt, minwt, df_flag1$weightvec)
diff_flag1 = sum(df_flag1$weightvec) - sum(df_flag1$chngwt)
delta_flag1 = diff_flag1 / nrow(df_flag1)

df_flag1$chngwt1 <- df_flag1$chngwt + delta_flag1

#df_wt = rbind(df_flag1, df_flag3)

df_wt_final <- df_flag1[ ,c(1,24,26,14,15)]

names(df_wt_final)[2] <- "WeightValue1"
names(df_wt_final)[3] <- "WeightValue2"
names(df_wt_final)[4] <- "WEEKID"
names(df_wt_final)[5] <- "TV_INTAB_FLAG"

#df2 <- merge(df_wt_final,ascribedusers, by = "USERS_META_ID")

write.csv(df_wt_final, paste("Smooth_wt", "_", weekID, ".csv"), row.names = F)

#-----------------------------------------------------------------------------------------------------------------------------#





