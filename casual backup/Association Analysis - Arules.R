#__________________________Association Analysis ARULES______________________

getwd()
setwd("F:/Practice R")
arul <- read.csv("association_analysis_data.csv",header=T)


arul1<- arul[1:10000,]

dat<-arul1
names(dat)

head(arul,10)
tail(arul)
str(arul)


library(sqldf)
library(arulesViz)
library(arules)
library(tcltk)



dat1=dat[,c("basket_id","description")]

#dat1$basket_id=as.numeric(dat1$basket_id)
#dat1 <- dat1[order(dat1$basket_id),]
dat1=dat[,c("basket_id","description")]



# this step is for data preparation for Apriori algorithm to run
dat1=transform(dat1,tag=paste0(dat1$basket_id,dat1$description))

##removing the duplicate transactions
dat1<-sqldf("select distinct * from dat1" )
dat1=dat1[,c(1,2)]

#aggregate all transacations related to a single product. this will put all the different items of transaction coulmn wise
aggr_data <- split(dat1$description, dat1$basket_id)





head(melt(aggr_data))

require(reshape2)
install.packages("reshape2")
mat$id <- rownames(mat) 
melt(mat)
??melt




#write.csv(aggr_data, file = "F:/Practice R/MBA.csv")

#convert aggr_data set to transaction object required for apriori algorithm
txn <- as(aggr_data,"transactions")

## Summary

head(txn)
summary(txn)

#output
#4109 rows (elements/itemsets/transactions) and
#12229 columns (items) and a density of 0.0008319376 
# density =0.000831 - proportion of non zero matrix cell

## item purchased during the time
# 4109*12229*0.000831 = 41804.001 (nu,ber of rows in transaction data)

inspect(txn[1:5])
itemFrequency(txn[, 1:5])

itemFrequencyPlot(txn, support = 0.02)
itemFrequencyPlot(txn, topN=20)


#____________________Code for further visualization

# Create an item frequency plot for the top 20 items
itemFrequencyPlot(txn,topN=20,type="absolute")


# Get the rules
rules <- apriori(txn, parameter = list(supp = 0.002, conf = 0.8))

# Show the top 5 rules, but only 2 digits
options(digits=2)
inspect(rules[1:5])

##If someone buys yogurt and cereals, they are 81% likely to buy whole milk too.

summary(rules)
#The number of rules generated: 410
#The distribution of rules by length: Most rules are 4 items long
#The summary of quality measures: interesting to see ranges of support, lift, and confidence.
#The information on the data mined: total data mined, and minimum parameters.

##we want to have the most likely rules
rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:5])

##Rule 4 is perhaps excessively long. Lets say you wanted more concise rules.
##rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8,maxlen=3))

##Sometimes, rules will repeat. Remove redundant rules generated.

subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned
inspect(rules[1:5])

###Targeting Items
##1.What are customers likely to buy before buying BANANAS?
##2.What are customers likely to buy if they purchase BANANAS?

#Answering the first question we adjust our apriori() function as follows:
rules1<-apriori(data=txn, parameter=list(supp=0.001,conf = 0.08), 
                appearance = list(default="lhs",rhs="BANANAS"),
                control = list(verbose=F))
rules1<-sort(rules1, decreasing=TRUE,by="confidence")
inspect(rules1[1:5])

###we can set the left hand side to be "BANANAS" and find its antecedents.
#Note the following:
#We set the confidence to 0.15 since we get no rules with 0.8
#We set a minimum length of 2 to avoid empty left hand side items

rules2<-apriori(data=txn, parameter=list(supp=0.001,conf = 0.08, minlen=2), 
                appearance = list(default="rhs",lhs="BANANAS"),
                control = list(verbose=F))
rules2<-sort(rules2, decreasing=TRUE,by="confidence")
inspect(rules2[1:2])

###Visualization
library("RColorBrewer")
plot(rules,control=list(col=brewer.pal(30,"Spectral")),main="") # in the main you can mention the graph name

#Plot graph-based visualisation: This will show the relations or rules by shorted lift

subrules2 <- head(sort(rules, by="lift"), 10)
plot(subrules2, method="graph",control=list(type="items",main=""))



kp<- as(rules, "data.frame")
write.csv(kp,"F:/Practice R/rules1.csv")

plot(rules, shading="order", control=list(col=brewer.pal(30,"Spectral")),(main = "Two-key plot"))
# from the above plot: The resulting Two-key plot is shown in Figure 3. From the plot it is clear that order and
# support have a very strong inverse relationship, which is a known fact for association rules

# below is an interractive scatter plot with lift on Y and support on X
sel <- plot(rules, measure=c("support", "lift"), shading="confidence",control=list(col=brewer.pal(30,"Spectral")), interactive=TRUE)




#--------------------MATRIX based visualization----------------------------

subrules <- rules[quality(rules)$confidence > 0.8]
subrules
plot(subrules, method="matrix", control=list(col=brewer.pal(30,"Spectral")),measure="lift")

plot(subrules, method="matrix", measure="lift", control=list(reorder=TRUE))

#_______________________________3D plots of association

plot(subrules, method="matrix3D", measure="lift")

plot(subrules, method="matrix3D", measure="lift", control=list(reorder=TRUE))

plot(subrules, method="matrix", measure=c("lift", "confidence"))

plot(subrules, method="matrix", measure=c("lift", "confidence"), control=list(reorder=TRUE))


#____________below is an important visualization in terms of bubble graph or LHS an RHS

plot(rules, method="grouped")

#To increase the number of groups we can change k which defaults to 20.
plot(rules, method="grouped", control=list(k=30))
#An interactive version of the grouped matrix visualization is also available.
sel <- plot(rules, method="grouped", interactive=TRUE)




#___________________----Graph-based visualizations-----VVI

#The following plot represents itemsets as vertices and rules as directed edges between itemsets
subrules2 <- head(sort(rules, by="lift"), 10)
plot(subrules2, method="graph", control=list(type="items"))

#Another variant (the default type) uses items and rules as two types of vertices and edges
#indicate which items occur in which rule.
plot(subrules2, method="graph")

# to save these graphs
saveAsGraph(head(sort(rules, by="lift"),1000), file="rules.graphml")
saveGraphML(head(sort(rules, by="lift"),1000), file="rules.graphml")

#------------------------------------Parallel coordinates plot
plot(subrules2, method="paracoord")
plot(subrules2, method="paracoord", control=list(reorder=TRUE))




#---------------------------------Double Decker plots


#We randomly choose a single rule and visualize it with a double decker plot.


oneRule <- sample(rules, 1)
inspect(oneRule)
plot(oneRule, method="doubledecker", data = txn)




#__________________When to use what
Technique              Method            Rule set            Measures             Interactive        Reordering       Ease of use
Scatterplot           "scatterplot"       large                3                     X                                    ++
Two-Key plot          "scatterplot"       large              2 + order              X                                    ++
Matrix-based           "matrix"          medium               1                                       X                   0
Matrix-b.(2 measures)   "matrix"         medium               2                                       X                  --
Matrix-b. (3D bar)     "matrix3D"        small                1                                       X                   +
Grouped matrix         "grouped"          large               2                     X                 X                   0
Graph-based              "graph"           small              2                                                            ++
Graph-b. (external)      "graph"            large             2                     X                 X                    +
Parallel coordinates     "paracoord"         small            1                                       X                    -
Double decker "           doubledecker"      single rule     (2)                                                           -

  
  
  
  
  
