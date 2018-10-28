 
############################# CF for Music Channel recommendation - Aha Radio app data - Userbased Recommendation##########################
require(RPostgreSQL)
require(recommenderlab)
require(reshape2)
options(scipen=999)





###Function to write recommendation into DB####################
get_data_recommendation=function()
{
	drv <- dbDriver("PostgreSQL")
	con <- dbConnect(drv, dbname="archives",host="54.160.220.236",port=5432,user="aha_user",password="livespot")
	##data <- dbGetQuery(con,"select c.user_name,c.station_id,u.gender,u.year_born,u.loc_country,u.initial_country,u.display_name,c.email,s.station_name,t.category_id,c.total_listen_time from user_listen_time_sum c left outer join users u on u.user_name=c.user_name join station s on s.station_id::text = c.station_id join station_category t on t.station_id::text = c.station_id")
	data<-dbGetQuery(con,"select c.user_name,c.station_id,u.gender,u.year_born,u.loc_country,u.initial_country,u.display_name,c.email,c.total_listen_time from user_listen_time_sum c join users u on u.user_name=c.user_name")
	data <- na.omit(data)
	retrun (data)

	
}




write_user_Recommendations = function()
{

require(RPostgreSQL)
require(recommenderlab)
require(reshape2)
options(scipen=999)
##options(digits = 20)
data=get_data_recommendation()
data.loc <- unique(data$initial_country)
for(i in 1:length(data.loc))
{

	
	country = data.loc[i]
	
	library(doSNOW)
    	library(foreach)
	cl <- makeCluster(6)
    	t1 = Sys.time()
    	registerDoSNOW(cl)
	subdata.use <- data[which(data$initial_country == country), ]

	df.snow=foreach(subdata.use=subdata.use,.combine=rbind,.packages=c("RPostgreSQL","reshape2","base","recommenderlab")) %dopar%{
	set_recommendations(subdata.use)
	sink(NULL) 	
	
	
}
##stopCluster(cl)
    t2 = Sys.time()
    t3 = t2-t1
    print(t3)
}

set_recommendations=function(subdata.use)
{
	options(scipen=999) # this is used to keep the user ID intact , if we don't use this then it will reflect the nearest highest no neighbour userID, we had 15 digit user ID and we did not want the last 3 digits to change and hecne we specified "999"
	filenameMat <- paste("rate_mat",country, sep=" ")
	fileMat <- paste(filenameMat,".csv",sep="")

	filenameSim <- paste("rated_Sim",country, sep=" ")
	fileSim <- paste(filenameSim,".csv",sep="")

      filename <- paste("FinalMatrix",country, sep=" ")
	fileMatrix <- paste(filename,".csv",sep="")

	fileAvgTime <- paste("AvgTime",country, sep=" ")
	fileTime <- paste(fileAvgTime,".csv",sep="")

	filenamMat <- paste("usrsK",country, sep=" ")
	fileat <- paste(filenamMat,".csv",sep="")


	####################start user recommendation##############################
    	UniqueCF <- "datUSALLUsers" # this is a temprary file name
	initialYear <- 1940 # this is post EDA what is the lowest birth year , the objective is to calculate age for classification
	ww <- 1;           # Will be used in for loop
	currentDate<-Sys.Date()
	monthOfYear <- format(currentDate, "%b")
	monthOfYear <- tolower(monthOfYear)

	for(ww in 1:8)   # 8 times bucket will run each for 10 years
{
	
	finalYear <- initialYear + 9
	data.users <-subdata.use[which(subdata.use$year_born >= initialYear & subdata.use$year_born <= finalYear),] # to get only eligible population under study discard rest
	uniquegen <- unique(data.users$gender)
	uniqueusers <- unique(data.users$email)
	len.uni <- length(uniqueusers) # this is to get countrywise how many users are there for less user number recomendation will not be activated
	if(len.uni > 2)
	{
	
	for(mm in 1: length(uniquegen))
	{
	dat.gen <- data.users[which(data.users$gender == uniquegen[mm]),] # now moving to subcluster level by unique gender and age group
	## put the top 10 stations for Inactive users- Start########## This is just general recomendation
	sortedData<-dat.gen[ order(-xtfrm(dat.gen[,9])), ]               # This will sort the data in descending order for the 9th column of dat.gen which is listen time
	sortedDatas <- sortedData[!duplicated(sortedData$station_id),]
	sortedStats=data.frame(v1 = character(1),v2=character(1), v3 = character(1), v4 = character(1),v5=character(1), v6=character(1),v7=character(1),v8=character(1),v9=character(1),v10=character(1),usergender = character(1),usercountry=character(1),userage=character(1),stringsAsFactors = FALSE)
	if(!is.na(sortedDatas$X2[1]))
	{
		sortedStats$v1=sortedDatas$station_id[1]
	}
	if(!is.na(sortedDatas$X2[2]))
	{
		sortedStats$v2=sortedDatas$station_id[2]
	}
	if(!is.na(sortedDatas$X2[3]))
	{
		sortedStats$v3=sortedDatas$station_id[3]
	}
	if(!is.na(sortedDatas$X2[4]))
	{
		sortedStats$v4=sortedDatas$station_id[4]
	}
	if(!is.na(sortedDatas$X2[5]))
	{
		sortedStats$v5=sortedDatas$station_id[5]
	}
	if(!is.na(sortedDatas$X2[6]))
	{
		sortedStats$v6=sortedDatas$station_id[6]
	}
	if(!is.na(sortedDatas$X2[7]))
	{
		sortedStats$v7=sortedDatas$station_id[7]
	}
	if(!is.na(sortedDatas$X2[8]))
	{
		sortedStats$v8=sortedDatas$station_id[8]
	}
	if(!is.na(sortedDatas$X2[9]))
	{
		sortedStats$v9=sortedDatas$station_id[9]
	}
	if(!is.na(sortedDatas$X2[10]))
	{
		sortedStats$v10=sortedDatas$station_id[10]
	}
	sortedStats$usergender <- uniquegen[mm]
	sortedStats$userage <- initialYear
	sortedStats$usercountry <- country
	###write sortedData into DB - Start####
	require(RPostgreSQL)
	rv <- dbDriver("PostgreSQL")
	
	con <- dbConnect(rv, dbname="archives",host="54.160.220.236",port=5432,user="aha_user",password="livespot")
	dbWriteTable(con,"general_recommendation", sortedStats,append=TRUE,row.names=FALSE)
	dbDisconnect(con)
	###write sortedData into DB - End######
	##Top 10 stations for Inactive users-End##################


# now movingg to user recomendation for active user


	unique.users.gender <- unique(dat.gen$email)
	len.usr.gen <- length(unique.users.gender)
	if(len.usr.gen > 2)
	{
	dat.gen$Rating <- 1 # one column is added to data with value 1 on each cell
	g<-acast(dat.gen, email ~ station_id,value.var="Rating")	# acast is a reshape function it will convert into matrix with first column as user id rest column as station ids
	class(g) # will give unique no of station id
	R<-as.matrix(g)
	R[R > 1] <- 1 # this is to conver all >1 no during acast to 1
	r <- as(R, "realRatingMatrix") # as is a function in recomender lab and the "realRatingMatrix" is key input for recomender to run
	r
	r_b <- binarize(r, minRating=1) # this is key step where apart from 1 all other will be termed as 0 in the matrix
	R_S <- as(r_b, "matrix")
	R_S <- cbind(rownames(R_S), R_S) # this is to add the row names in to the data, this will be nullified in firther steps as we need data only with user name and stations not row names then user names then stations
	rownames(R_S) <- NULL
	colnames(R_S)[1] <- c("users") # Changing the column name to USER


	filnamMat <- paste("simFile",UniqueCF, sep=" ") # this step we want to create subfiles as simfile_US, simfile_UK etc
	nameOfFileMat <- paste(filnamMat,country,sep="")
	filema <- paste(nameOfFileMat,".csv",sep="")

	
	rb.mat <- as(r_b, "matrix")
	nums.cols <- ncol(rb.mat)
	nums.rows <- nrow(rb.mat)
	noCompute <- 0
	if(nums.cols == 1 && nums.rows > 1) # if in the country only one station is being tuned then what is the use of recomending anything
	{
		noCompute <- 1
      }
	if(noCompute == 0)
	

{## In use  
	rec=Recommender(r_b[1:nrow(r_b)],method="UBCF", param=list(method="Jaccard")) # User recomendation is done using Jaccard and station recomendation is done using Cosign
	##Not used
	##rec=Recommender(r[1:nrow(r)],method="IBCF", param=list(normalize = "Z-score",method="Cosine",minRating=1))
	##rec=Recommender(r_b[1:nrow(r_b)],method="IBCF", param=list(method="Cosine"))
	print(rec)
	names(getModel(rec))
	recom <- predict(rec, r_b[1:nrow(r_b)], type="topNList") # this fuction may fail due to unknown reasons sometime then run another predict with NULL entry in place of 0
	recom.list <- as(recom,"list")
	recom.list <- recom.list[lapply(recom.list,length)>0]    # need to dign further why this is used as this seems to be used due to some issues
	##recom.dat <- as.data.frame(recom.list)
	recom.tab <- do.call(rbind, recom.list)    # Do.call is used as list of recomendation might be uneven and hence as.data.frame will not work
	if(is.null(recom.tab))
	{
		recom <- predict(rec, r[1:nrow(r)], type="topNList") # if recomendation does not work then we use this loop for recomendation with blanks instead of 0
	recom.list <- as(recom,"list")
	recom.list <- recom.list[lapply(recom.list,length)>0]
	##recom.dat <- as.data.frame(recom.list)
	recom.tab <- do.call(rbind, recom.list)

	
	}

# this is the practical end of the algorithm .. now we are moving to making a structure database
	
 	res <- "interResult"
	filenamMat <- paste(initialYear,country, sep=" ")
	filemanMat <- paste(filenamMat,res,sep=" ")
	finam <- paste(filemanMat,uniquegen[mm],sep=" ")
	fileMa <- paste(finam,".csv",sep="")

 
  # Output neighbour results to a file  
  write.csv(recom.tab,file=fileMa)
  AllresultFile <- read.csv(file=fileMa)    # this is twisted approach to change the list to a dataframe a. once we convert it to csv and then read back it will autometically convert in dataframe
  frame.dat <- as(r_b,"matrix")
  frame.dat <- cbind(rownames(frame.dat), frame.dat)
  rownames(frame.dat) <- NULL
  colnames(frame.dat)[1] <- c("users")
frame.dat <- as.data.frame(frame.dat) # this is critical step as all recomendation comes in once cell for each user this step splits them into seperate columns
  num.rows.result <- nrow(AllresultFile)
  num.rows.df <- nrow(frame.dat)
  isAtrue <- 0
  if(num.rows.result < num.rows.df)
  {
	for(gg in 1:num.rows.result)
	{
		AllresultFile$users[gg] <- frame.dat$users[gg] #if the recomendation has still not worked we remove those entries from the data
	}
	isAtrue <- 1
  }
  if(isAtrue == 0)
  {
  	AllresultFile$users <- frame.dat[,'users']
  }

	AllresultFile$X <- NULL
	
	columnsOfDF1 <- ncol(AllresultFile)
	columnsOfDF <- columnsOfDF1 - 1
	if(columnsOfDF > 1)
	{
		if(columnsOfDF > 10)
		{
			
			AllresultFile <- AllresultFile[,c(1:11)]
			AllresultFile$V1 <- gsub('X', '', AllresultFile$V1)
			AllresultFile$V2 <- gsub('X', '', AllresultFile$V2)
			AllresultFile$V3 <- gsub('X', '', AllresultFile$V3)
			AllresultFile$V4 <- gsub('X', '', AllresultFile$V4)
			AllresultFile$V5 <- gsub('X', '', AllresultFile$V5)
			AllresultFile$V6 <- gsub('X', '', AllresultFile$V6)
			AllresultFile$V7 <- gsub('X', '', AllresultFile$V7)
			AllresultFile$V8 <- gsub('X', '', AllresultFile$V8)
			AllresultFile$V9 <- gsub('X', '', AllresultFile$V9)
			AllresultFile$V10 <- gsub('X', '', AllresultFile$V10)
				
		}
		else if(columnsOfDF == 2)
		{
			AllresultFile$V1 <- gsub('X', '', AllresultFile$V1)
			AllresultFile$V2 <- gsub('X', '', AllresultFile$V2)
			
			
			AllresultFile <- as.data.frame(append(AllresultFile, list(V3 = NA), after = 2))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V4 = NA), after = 3))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V5 = NA), after = 4))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V6 = NA), after = 5))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V7 = NA), after = 6))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V8 = NA), after = 7))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V9 = NA), after = 8))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V10 = NA), after = 9))

			
		}
		else if (columnsOfDF == 3)
		{
			AllresultFile$V1 <- gsub('X', '', AllresultFile$V1)
			AllresultFile$V2 <- gsub('X', '', AllresultFile$V2)
			AllresultFile$V3 <- gsub('X', '', AllresultFile$V3)
						

			AllresultFile <- as.data.frame(append(AllresultFile, list(V4 = NA), after = 3))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V5 = NA), after = 4))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V6 = NA), after = 5))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V7 = NA), after = 6))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V8 = NA), after = 7))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V9 = NA), after = 8))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V10 = NA), after = 9))


		}
		else if(columnsOfDF == 4)
		{
			AllresultFile$V1 <- gsub('X', '', AllresultFile$V1)
			AllresultFile$V2 <- gsub('X', '', AllresultFile$V2)
			AllresultFile$V3 <- gsub('X', '', AllresultFile$V3)
			AllresultFile$V4 <- gsub('X', '', AllresultFile$V4)
			

			AllresultFile <- as.data.frame(append(AllresultFile, list(V5 = NA), after = 4))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V6 = NA), after = 5))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V7 = NA), after = 6))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V8 = NA), after = 7))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V9 = NA), after = 8))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V10 = NA), after = 9))


		}
		else if (columnsOfDF == 5)
		{
			AllresultFile$V1 <- gsub('X', '', AllresultFile$V1)
			AllresultFile$V2 <- gsub('X', '', AllresultFile$V2)
			AllresultFile$V3 <- gsub('X', '', AllresultFile$V3)
			AllresultFile$V4 <- gsub('X', '', AllresultFile$V4)
			AllresultFile$V5 <- gsub('X', '', AllresultFile$V5)
			


			AllresultFile <- as.data.frame(append(AllresultFile, list(V6 = NA), after = 5))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V7 = NA), after = 6))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V8 = NA), after = 7))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V9 = NA), after = 8))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V10 = NA), after = 9))


		}
		else if (columnsOfDF == 6)
		{
			AllresultFile$V1 <- gsub('X', '', AllresultFile$V1)
			AllresultFile$V2 <- gsub('X', '', AllresultFile$V2)
			AllresultFile$V3 <- gsub('X', '', AllresultFile$V3)
			AllresultFile$V4 <- gsub('X', '', AllresultFile$V4)
			AllresultFile$V5 <- gsub('X', '', AllresultFile$V5)
			AllresultFile$V6 <- gsub('X', '', AllresultFile$V6)
			

			AllresultFile <- as.data.frame(append(AllresultFile, list(V7 = NA), after = 6))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V8 = NA), after = 7))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V9 = NA), after = 8))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V10 = NA), after = 9))


		}
		else if(columnsOfDF == 7)
		{
			AllresultFile$V1 <- gsub('X', '', AllresultFile$V1)
			AllresultFile$V2 <- gsub('X', '', AllresultFile$V2)
			AllresultFile$V3 <- gsub('X', '', AllresultFile$V3)
			AllresultFile$V4 <- gsub('X', '', AllresultFile$V4)
			AllresultFile$V5 <- gsub('X', '', AllresultFile$V5)
			AllresultFile$V6 <- gsub('X', '', AllresultFile$V6)
			AllresultFile$V7 <- gsub('X', '', AllresultFile$V7)
			

			AllresultFile <- as.data.frame(append(AllresultFile, list(V8 = NA), after = 7))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V9 = NA), after = 8))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V10 = NA), after = 9))


		}
		else if (columnsOfDF == 8)
		{
			AllresultFile$V1 <- gsub('X', '', AllresultFile$V1)
			AllresultFile$V2 <- gsub('X', '', AllresultFile$V2)
			AllresultFile$V3 <- gsub('X', '', AllresultFile$V3)
			AllresultFile$V4 <- gsub('X', '', AllresultFile$V4)
			AllresultFile$V5 <- gsub('X', '', AllresultFile$V5)
			AllresultFile$V6 <- gsub('X', '', AllresultFile$V6)
			AllresultFile$V7 <- gsub('X', '', AllresultFile$V7)
			AllresultFile$V8 <- gsub('X', '', AllresultFile$V8)
			

			AllresultFile <- as.data.frame(append(AllresultFile, list(V9 = NA), after = 8))
			AllresultFile <- as.data.frame(append(AllresultFile, list(V10 = NA), after = 9))


		}
		else if (columnsOfDF == 9)
		{
			AllresultFile$V1 <- gsub('X', '', AllresultFile$V1)
			AllresultFile$V2 <- gsub('X', '', AllresultFile$V2)
			AllresultFile$V3 <- gsub('X', '', AllresultFile$V3)
			AllresultFile$V4 <- gsub('X', '', AllresultFile$V4)
			AllresultFile$V5 <- gsub('X', '', AllresultFile$V5)
			AllresultFile$V6 <- gsub('X', '', AllresultFile$V6)
			AllresultFile$V7 <- gsub('X', '', AllresultFile$V7)
			AllresultFile$V8 <- gsub('X', '', AllresultFile$V8)
			AllresultFile$V9 <- gsub('X', '', AllresultFile$V9)
			

			AllresultFile <- as.data.frame(append(AllresultFile, list(V10 = NA), after = 9))
		}
		else if (columnsOfDF == 10)
		{
			AllresultFile$V1 <- gsub('X', '', AllresultFile$V1)
			AllresultFile$V2 <- gsub('X', '', AllresultFile$V2)
			AllresultFile$V3 <- gsub('X', '', AllresultFile$V3)
			AllresultFile$V4 <- gsub('X', '', AllresultFile$V4)
			AllresultFile$V5 <- gsub('X', '', AllresultFile$V5)
			AllresultFile$V6 <- gsub('X', '', AllresultFile$V6)
			AllresultFile$V7 <- gsub('X', '', AllresultFile$V7)
			AllresultFile$V8 <- gsub('X', '', AllresultFile$V8)
			AllresultFile$V9 <- gsub('X', '', AllresultFile$V9)
			AllresultFile$V10 <- gsub('X', '', AllresultFile$V10)
		}
	
	AllresultFile$month <- monthOfYear
	AllresultFile$userAge <- initialYear
	AllresultFile$userGender <- uniquegen[mm]
	AllresultFile$userCountry <- country
	require(RPostgreSQL)
	rv <- dbDriver("PostgreSQL")
	
	con <- dbConnect(rv, dbname="archives",host="54.160.220.236",port=5432,user="aha_user",password="livespot")
	dbWriteTable(con,"userReco", AllresultFile,append=TRUE)
	dbDisconnect(con)


	     }

	##year="ALLUSERS"
	##filenamMat <- paste(initialYear,country, sep=" ")
	##finam <- paste(filenamMat,uniquegen[mm],sep=" ")
	##fileMa <- paste(finam,".csv",sep="")
	
  	##write.csv(AllresultFile,file=fileMa)

}##  end of if no.compute
 
}## end of gender > 2

}##end of mm in 1: length(uniquegen)

}##end of len.uni
initialYear <- initialYear + 10
ww <- ww + 1

 } ## end of outer for loop	
}

}


##write recommendations into DB####
write_user_Recommendations()




