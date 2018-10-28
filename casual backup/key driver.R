Snacks.all.level.data.for.Key.driver <- read.table("E://Snacks all level data for Key driver.csv",header=T,sep=",",quote="")
kp<-scale(Snacks.all.level.data.for.Key.driver)
str(Snacks.all.level.data.for.Key.driver)
Snacks.all.level.data.for.Key.driver$Discount.Depth.Competitor..Tortinos = as.numeric(Snacks.all.level.data.for.Key.driver$Discount.Depth.Competitor..Tortinos)
Snacks.all.level.data.for.Key.driver$Discount.Depth.Heinz= as.numeric(Snacks.all.level.data.for.Key.driver$Discount.Depth.Heinz)
write.csv(kp, file= "E://kpdata.csv")


Sales<-read.table("E:/hEINZ/KP edited/Kumar/Price Sensitivity.csv",header=T,sep=",",quote="")

kk<-scale(Sales)
write.csv(kk, file= "E:/hEINZ/KP edited/Kumar/salesdata.csv")

setwd("E:/Hayatt")
data<-read.table("GSS_RESP_HDR_data1.txt",header=T,sep=" ")
data1<- read.delim("GSS_RESP_HDR_data1.txt", header=TRUE, sep="\t")
