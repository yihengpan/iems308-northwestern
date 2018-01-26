#load csv file
df1<-fread("Desktop/308Data science&analysis/Medicare_Provider_Util_Payment_PUF_CY2015/hw1.txt",header=T,sep="\t",fill=TRUE)

#random select sample
sample<-df1[sample(1:nrow(df1),10000,replace=FALSE)]

#omit missing values
sample<-na.omit(sample)

#explore the data
summary(df1)
plot(sample$average_Medicare_allowed_amt)
plot(sample$average_submitted_chrg_amt)
plot(sample$average_Medicare_payment_amt)
plot(sample$average_Medicare_standard_amt)

#use group by to count the health service number of a specific city 
planes<-group_by(sample,nppes_provider_city)

#select the useful information from plane to a new table
delay<-summarise(planes,count = n(),allowed=sum(average_Medicare_allowed_amt),submitted=sum(average_submitted_chrg_amt))

#normalization
delay_norm<-scale(delay[,c(2:5)])

#draw graph to decide how many clusters to choose
fviz_nbclust(delay_norm, kmeans, method = "wss")

#create cluster
cl<- kmeans(delay_norm,centers = 4)

#plot the relationship
plot(delay$count,delay$rate,col=cl$cluster)