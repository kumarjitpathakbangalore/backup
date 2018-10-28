install.packages("reshape")
library(reshape)
library(ggplot2)

data.m <- melt(HyattAug, id=c(1:38), measure=c(38))
data.c <- cast(data.m, CG_GENDER ~ LENGTH_OF_STAY, sum)

data.c <- cast(data.m, CG_GENDER ~ LENGTH_OF_STAY, sum)

??cast
??table
table(HyattAug$CG_GENDER, data= HyattAug)


data.c <- cast(data.m, C(CG_GENDER ~ variable, sum)
??describe
p<- summary(Selectvariable)
p
p<-summary(HyattAug)
write.txt(p, file = "E:\\Hayatt\\Research on KPI\\HYATT ACTUAL DATA\\obs.txt")

table(HyattAug$RESERVATION_DATE)

table(HyattAug$NUMBER_OF_ROOMS)

table(HyattAug$PMS_ROOM_REV_USD)
table(HyattAug$CHECK_IN_DATE)

library(sqldf)
attach(HyattAug)
test<-sqldf("select RESERVATION_DATE,DIRECT_POV_CODE,avg(PMS_TOTAL_REV_USD) as Avg_Res
            from HyattAug
            group by RESERVATION_DATE,DIRECT_POV_CODE
            order by RESERVATION_DATE,DIRECT_POV_CODE")

str(test)
require(lubridate)
test$month<-month(test$RESERVATION_DATE)

year(date1)

GSS <- read.csv("E:\\Hayatt\\Research on KPI\\Yash work\\GSS_RESP_HDR_data1.txt",header=T)















