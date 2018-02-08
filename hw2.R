#install package

library(DBI)
library(RPostgreSQL)
library(arulesViz)

#connect
con = dbConnect(PostgreSQL(),user ="ypd1759",password ="ypd1759_pw", host="gallery.iems.northwestern.edu",dbname ="iems308")

#read table
deptinfo <- dbGetQuery(con, "select * from pos.deptinfo")

#read purchase top 100
trnsact <- dbGetQuery(con, "select * from pos.trnsact where pos.trnsact.c1 in
(select c1 from (select pos.trnsact.c1,count(pos.trnsact.c8) as num from pos.trnsact where pos.trnsact.c7='P' group by pos.trnsact.c1 order by num desc limit 100) as a);")
trnsact<-trnsact[sample(1:nrow(trnsact),100000,replace=FALSE)]

#download the top 100
sku100 <- dbGetQuery(con, "select pos.trnsact.c1,count(pos.trnsact.c8) as num from pos.trnsact where pos.trnsact.c7='P' group by pos.trnsact.c1 order by num desc limit 100")

#hw2:select the useful columns
hw2<- data.frame(trnsact$c1,trnsact$c2,trnsact$c3,trnsact$c4,trnsact$c6)
hw2data<-na.omit(hw2)
colnames(hw2data)<-c("SKU","Store","Register","Trannum","Saledate")

#library
library("arules", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

#data 
hw2data$SKU<-as.factor(hw2data$SKU)
hw2data$Store<-as.numeric(hw2data$Store)
hw2data$Register<-as.numeric(hw2data$Register)
hw2data$Trannum<-as.numeric(hw2data$Trannum)
hw2data$Saledate<-as.Date(hw2data$Saledate)
hw2data$BasketID<-paste(hw2data$Store,hw2data$Register,hw2data$Trannum,hw2data$Saledate,collapse=NULL,sep=",")
hw2data$BasketID<-as.factor(hw2data$BasketID)

#create transaction file
transaction<-write.csv(tran,file="transactf.csv",row.names=FALSE)
transaction<-read.transactions("transactf.csv",cols=c(1,2),format="single",rm.duplicates=TRUE)

#find out rule association
rules<-apriori(transaction,parameter=list(supp=0.00001,conf=0.0125,minlen=2))
rules<-sort(rules, by="lift")
summary(rules)
inspect(rules)

#optimize rules
#check if there is subset rule of another.
subset.matrix<-is.subset(rules,rules,sparse = FALSE)
subset.matrix[lower.tri(subset.matrix,diag = T)]<-NA
redundant<-colSums(subset.matrix,na.rm=T)>=1
which(redundant)

#remove redundant rules
rules.refined<-rules[!redundant]
rules.refined<-sort(rules.refined, by="lift")
inspect(rules.refined)
summary(rules.refined)

#visualize rules
plot(rules.refined)
plot(rules.refined,method="graph",control=list(type="items"))
