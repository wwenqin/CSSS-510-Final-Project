library(MatchIt)
user=read.csv("yelp_academic_dataset_user_numID.csv",stringsAsFactors = FALSE)
user <- user[,-1]
review <- read.csv("finalreviews (1).csv",stringsAsFactors = FALSE)
review <- review[!duplicated(review$review_id),]
eliteuser <- read.csv("eliteUsers.csv",colClasses=c("NULL",NA,NA,NA,NA))
user <- merge(user,eliteuser,by.x="numID",by.y="id")

elitereview <- subset(review,isElite=="1")
nonelitereview <- subset(review,isElite=="0")

elitereview$reviewyear <- as.Date(elitereview$date,"%Y-%m-%d")
elitereview$reviewyear <- format(elitereview$reviewyear,'%Y')

nonelitereview$reviewyear <- as.Date(nonelitereview$date,"%Y-%m-%d")
nonelitereview$reviewyear <- format(nonelitereview$reviewyear,'%Y')

elitereview$gapbtreel <- elitereview$first_elite_in-as.numeric(elitereview$reviewyear)
nonelitereview$gapbtreel <- nonelitereview$first_elite_in-as.numeric(elitereview$reviewyear)+2012

elite.bf.review <- subset(elitereview,gapbtreel>=0)
elite.af.review <- subset(elitereview,gapbtreel<0)


nonelite.bf.review <- subset(nonelitereview,gapbtreel>=0)
nonelite.af.review <- subset(nonelitereview,gapbtreel<0)

library(data.table)
elite.bf.review.agg <- data.table(elite.bf.review)
elite.bf.review.agg <- elite.bf.review.agg[,list(isElite=mean(isElite),total.votes.cool=sum(votes.cool.y),total.votes.funny=sum(votes.funny.y),total.votes.useful=sum(votes.useful.y),average.star=mean(stars),review.counts=.N),by="id"]

elite.af.review.agg <- data.table(elite.af.review)
elite.af.review.agg <- elite.af.review.agg[,list(isElite=mean(isElite),total.votes.cool=sum(votes.cool.y),total.votes.funny=sum(votes.funny.y),total.votes.useful=sum(votes.useful.y),average.star=mean(stars),review.counts=.N),by="id"]

nonelite.bf.review.agg <- data.table(nonelite.bf.review)
nonelite.bf.review.agg <- nonelite.bf.review.agg[,list(isElite=mean(isElite),total.votes.cool=sum(votes.cool.y),total.votes.funny=sum(votes.funny.y),total.votes.useful=sum(votes.useful.y),average.star=mean(stars),review.counts=.N),by="id"]

nonelite.af.review.agg <- data.table(nonelite.af.review)
nonelite.af.review.agg <- nonelite.af.review.agg[,list(isElite=mean(isElite),total.votes.cool=sum(votes.cool.y),total.votes.funny=sum(votes.funny.y),total.votes.useful=sum(votes.useful.y),average.star=mean(stars),review.counts=.N),by="id"]

bf.review.agg <- rbind(elite.bf.review.agg,nonelite.bf.review.agg)
af.review.agg <- rbind(elite.af.review.agg,nonelite.af.review.agg)

selected.attr.user <- user[c(1,2,25,26,27,5,8,10,16,18)]

bf.review.merged <- merge(bf.review.agg,selected.attr.user,by.x="id",by.y="numID")
af.review.merged <- merge(af.review.agg,selected.attr.user,by.x="id",by.y="numID")
#mergeddf <- rbind(bf.review.merged,af.review.merged)
bf.merged.matrix <- as.matrix(bf.review.merged)

countfriend <- function(i){
  friends=as.character(bf.merged.matrix[i,10])
  friend1=strsplit(friends,"[",fixed=TRUE)
  friend1=strsplit(friend1[[1]][2],"]",fixed=TRUE)
  friend1=strsplit(friend1[[1]][1],",",fixed=TRUE)
  length(friend1[[1]])
}

friend.count <- sapply(1:length(bf.review.merged$id), countfriend)
bf.review.merged$friends <- NULL
bf.merged.df <- data.frame(bf.review.merged,friend.count)
bf.noncom.df <- subset(bf.merged.df[c(1,2,3,4,5,6,7,8,10,12,14,15,16)])

library(lubridate)
bf.noncom.df$yelping_since <- year(as.Date(paste(bf.noncom.df$yelping_since,"-01",sep="")))
bf.noncom.df$yeargap <- ifelse(bf.noncom.df$first_elite_in==0,2012-bf.noncom.df$yelping_since,bf.noncom.df$first_elite_in-bf.noncom.df$yelping_since)


af.merged.matrix <- as.matrix(af.review.merged)
af.countfriend <- function(i){
    friends=as.character(af.merged.matrix[i,10])
    friend1=strsplit(friends,"[",fixed=TRUE)
    friend1=strsplit(friend1[[1]][2],"]",fixed=TRUE)
    friend1=strsplit(friend1[[1]][1],",",fixed=TRUE)
    length(friend1[[1]])
  }

af.friend.count <- sapply(1:length(af.review.merged$id), af.countfriend)
af.review.merged$friends <- NULL
af.merged.df <- data.frame(af.review.merged,af.friend.count)
af.noncom.df <- subset(af.merged.df[c(1,2,3,4,5,6,7,8,10,12,14,15,16)])
af.noncom.df$yelping_since <- year(as.Date(paste(af.noncom.df$yelping_since,"-01",sep="")))
af.noncom.df$yeargap <- ifelse(af.noncom.df$first_elite_in==0,2012-af.noncom.df$yelping_since,af.noncom.df$first_elite_in-af.noncom.df$yelping_since)

#psm <- matchit(isElite.x~total.votes.cool+total.votes.funny+total.votes.useful+average.star+review.counts+fans+friend.count+yeargap,data=bf.noncom.df)
#mahalanobis <- matchit(isElite.x~total.votes.cool+total.votes.funny+total.votes.useful+average.star+review.counts+fans+friend.count+yeargap,data=bf.noncom.df,distance="mahalanobis")
#cem <- matchit(isElite.x~total.votes.cool+total.votes.funny+total.votes.useful+average.star+review.counts+fans+friend.count+yeargap,data=bf.noncom.df,method="cem",k2k=TRUE,distance="mahalanobis")

bf.noncom.df$norm.votes.cool <- bf.noncom.df$total.votes.cool/bf.noncom.df$review.counts
bf.noncom.df$norm.votes.funny <- bf.noncom.df$total.votes.funny/bf.noncom.df$review.counts
bf.noncom.df$norm.votes.useful <- bf.noncom.df$total.votes.useful/bf.noncom.df$review.counts

af.noncom.df$norm.votes.cool <- af.noncom.df$total.votes.cool/af.noncom.df$review.counts
af.noncom.df$norm.votes.funny <- af.noncom.df$total.votes.funny/af.noncom.df$review.counts
af.noncom.df$norm.votes.useful <- af.noncom.df$total.votes.useful/af.noncom.df$review.counts

psm <- matchit(isElite.x~norm.votes.cool+norm.votes.funny+norm.votes.useful+average.star+review.counts+fans+friend.count+yeargap,data=bf.noncom.df)
psm2 <- matchit(isElite.x~norm.votes.cool+norm.votes.funny+norm.votes.useful+average.star+review.counts+fans+friend.count,data=bf.noncom.df)
mahalanobis <- matchit(isElite.x~norm.votes.cool+norm.votes.funny+norm.votes.useful+average.star+review.counts+fans+friend.count+yeargap,data=bf.noncom.df,distance="mahalanobis")

treatment.id <- bf.noncom.df$id[psm$treat==1]
control.index <- c(as.numeric(psm$match.matrix[,1]))
control.id <- bf.noncom.df$id[control.index]

bf.treat.df <- subset(bf.noncom.df,id%in%treatment.id)
bf.control.df <- subset(bf.noncom.df,id%in%control.id)

af.treat.df <- subset(af.noncom.df,id%in%treatment.id)
af.control.df <- subset(af.noncom.df,id%in%control.id)

write.table(control.id,file="control_id.csv",sep=",",row.names = FALSE)
write.table(treatment.id,file="treatment_id.csv",sep=",",row.names = FALSE)

write.table(bf.treat.df,file="before_treatment.csv",sep=',',row.names = FALSE)
write.table(bf.control.df,file="before_control.csv",sep=',',row.names = FALSE)
write.table(af.treat.df,file="after_treatment.csv",sep=',',row.names = FALSE)
write.table(af.control.df,file="after_control.csv",sep=',',row.names = FALSE)

#Plot trend line
library(lubridate)
elitereview$join_year<- year(as.Date(paste(elitereview$join_year,"-01",sep="")))
elite.st.review <- subset(elitereview,as.numeric(reviewyear)-join_year==1)
nonelitereview$join_year<- year(as.Date(paste(nonelitereview$join_year,"-01",sep="")))
nonelite.st.review <- subset(nonelitereview,as.numeric(reviewyear)-join_year==1)

elite.st.review.agg <- data.table(elite.st.review)
elite.st.review.agg <- elite.st.review.agg[,list(isElite=mean(isElite),total.votes.cool=sum(votes.cool.y),total.votes.funny=sum(votes.funny.y),total.votes.useful=sum(votes.useful.y),average.star=mean(stars),review.counts=.N),by="id"]

nonelite.st.review.agg <- data.table(nonelite.st.review)
nonelite.st.review.agg <- nonelite.st.review.agg[,list(isElite=mean(isElite),total.votes.cool=sum(votes.cool.y),total.votes.funny=sum(votes.funny.y),total.votes.useful=sum(votes.useful.y),average.star=mean(stars),review.counts=.N),by="id"]

elite.st.review.agg$norm.votes.cool <- elite.st.review.agg$total.votes.cool/elite.st.review.agg$review.counts
elite.st.review.agg$norm.votes.funny <- elite.st.review.agg$total.votes.funny/elite.st.review.agg$review.counts
elite.st.review.agg$norm.votes.useful <- elite.st.review.agg$total.votes.useful/elite.st.review.agg$review.counts

nonelite.st.review.agg$norm.votes.cool <- nonelite.st.review.agg$total.votes.cool/nonelite.st.review.agg$review.counts
nonelite.st.review.agg$norm.votes.funny <- nonelite.st.review.agg$total.votes.funny/nonelite.st.review.agg$review.counts
nonelite.st.review.agg$norm.votes.useful <- nonelite.st.review.agg$total.votes.useful/nonelite.st.review.agg$review.counts

elite.trend <- as.data.frame(merge(elite.st.review.agg,bf.treat.df,by="id"))
nonelite.trend <- as.data.frame(merge(nonelite.st.review.agg,bf.control.df,by="id"))

elite.votes.cool <- subset(elite.trend[c(1,8,24)])
elite.votes.funny <- subset(elite.trend[c(1,9,25)])
elite.votes.useful <- subset(elite.trend[c(1,10,26)])
nonelite.votes.cool <- subset(nonelite.trend[c(1,8,24)])
nonelite.votes.funny <- subset(nonelite.trend[c(1,9,25)])
nonelite.votes.useful <- subset(nonelite.trend[c(1,10,26)])

mean.votes.cool=matrix(c(mean(elite.votes.cool$norm.votes.cool.x),mean(nonelite.votes.cool$norm.votes.cool.x),
                         mean(elite.votes.cool$norm.votes.cool.y),mean(nonelite.votes.cool$norm.votes.cool.y)),nrow=2,ncol=2)

mean.votes.funny=matrix(c(mean(elite.votes.funny$norm.votes.funny.x),mean(nonelite.votes.funny$norm.votes.funny.x),
                          mean(elite.votes.funny$norm.votes.funny.y),mean(nonelite.votes.funny$norm.votes.funny.y)),nrow=2,ncol=2)

mean.votes.useful=matrix(c(mean(elite.votes.useful$norm.votes.useful.x),mean(nonelite.votes.useful$norm.votes.useful.x),
                          mean(elite.votes.useful$norm.votes.useful.y),mean(nonelite.votes.useful$norm.votes.useful.y)),nrow=2,ncol=2)

matplot(t(mean.votes.cool),type=c("b"))
matplot(t(mean.votes.funny),type=c("b"))
matplot(t(mean.votes.useful),type=c("b"))
