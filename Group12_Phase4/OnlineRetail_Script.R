getwd()
setwd("/Users/ishika/Desktop/Sem2/BigDataCSCI620/ROnline_Retail")
install.packages("arules")
install.packages("arulesViz")
install.packages("splitstackshape")
library(arules)
library(arulesViz)

#Reading Online Retail dataset 
Auto <- read.csv("Online_Retail.csv",header=T,na.strings="?")
summary(Auto)

#Filtering data for Germany
country=Auto$Country
Data=Auto[country == "Germany", ]
summary(Data)

#Removing duplicate data from Germany dataset
duData=unique(Data)
dupData=Data[!duplicated(Data), ]
summary(dupData)

#Removing the missing values from Germany dataset
dupData=na.omit(dupData)
summary(dupData)

#Removing the attributes which is not required
dupData$StockCode <- NULL
dupData$InvoiceDate <- NULL
dupData$Quantity <- NULL
dupData$Country<- NULL
dupData$UnitPrice <- NULL
dupData$CustomerID <- NULL
View(dupData)
summary(dupData)

#Aggregation of the Invoice and Product description
write.csv(x = aggregate(dupData$Description~dupData$InvoiceNo,FUN=toString),file = "Aggregation.csv")
Agg <- read.csv("Aggregation.csv")
View(Agg)

#Splitting the description for Invoice number in seperate column
library(splitstackshape)
write.csv(cSplit(Agg, "dupData.Description", ","), file = "Split.csv", row.names = FALSE)
Auto1 <- read.csv("Split.csv")
View(Auto1)

#Performing the Market Basket Analysis using Apriori algorihtm
basket_data = read.csv("Split.csv")
View(basket_data)
Data1 <- read.transactions("Split.csv", format = 'basket', sep=",")
bs_rules <- apriori(Data1, parameter = list(supp = 0.1, conf = 0.8, minlen =2, maxlen = 10))
#Removing the duplicate rules
redundant_rules<-is.redundant(bs_rules)
bs_rules <- bs_rules[!redundant_rules]
inspect(bs_rules)

#Plotting boxplot of frequency of description
attach(dupData)
summary(dupData)
t<-(summary(dupData$Description))  
value=as.vector(summary(dupData$Description))
name=as.vector(names(summary(dupData$Description)))
df <- data.frame(name,value)   
boxplot(df$value)

#Plotting boxplot of frequency of Invoice Number
t<-(summary(dupData$InvoiceNo))  
value=as.vector(summary(dupData$InvoiceNo))
name=as.vector(names(summary(dupData$InvoiceNo)))
df <- data.frame(name,value)   
boxplot(df$value)

#Plotting Absolute Item Frequency
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}
itemFrequencyPlot(Data1,topN=10,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
