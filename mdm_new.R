library(lubridate)
library(data.table)
library(MatchIt)

review2012 <- read.csv("eliteUsersReview2012.csv",stringsAsFactors = FALSE)
review2013_2015 <- read.csv("eliteUsersReview2013_2015.csv",stringsAsFactors = FALSE)
user <- read.csv("eliteUsersAll.csv",stringsAsFactors = FALSE)

#Remove unused columns
user$X <- NULL
colnames(user) <- c("yelping_since","userID","unumID","first_elite")
review2012$X <- NULL
review2012$text <- NULL
review2012$type <- NULL
review2013_2015$X <- NULL
review2013_2015$text <- NULL
review2013_2015$type <- NULL

#Filter pre-treatment reviews written by treatment group
review_treat_pre <- subset(review2012,as.Date(review2012$date,"%Y-%m-%d")>="2011-09-01"&as.Date(review2012$date,"%Y-%m-%d")<="2011-12-31")

#Get treatment user ID who wrote a review during pre-treatment period
treat_id <- unique(review_treat_pre$unumID)

#Filter post-treatment reviews written by treatment group
review_treat_post <- subset(review2012,as.Date(review2012$date,"%Y-%m-%d")>="2012-09-01"&as.Date(review2012$date,"%Y-%m-%d")<="2012-12-31")

#Get treatment user IDs who wrote reviews during both pre and post treatment periods
treat_id <- intersect(treat_id,unique(review_treat_post$unumID))

#Get all reviews written by selected treatment group before treatment
review_treat <- subset(review2012,review2012$unumID%in%treat_id& (format(as.Date(review2012$date,"%Y-%m-%d"),"%Y")) <2012)

#Get all reviews written by control group before 2012
review_control <- subset(review2013_2015,(format(as.Date(review2013_2015$date,"%Y-%m-%d"),"%Y")) <2012)

#Aggregate reviews written by treatment group
review.treat.agg <- data.table(review_treat)
review.treat.agg <- review.treat.agg[,list(total.votes.cool=sum(votes.cool),total.votes.funny=sum(votes.funny),total.votes.useful=sum(votes.useful),average.star=mean(stars),review.counts=.N),by="unumID"]

#Aggregate reviews written by control group
review.control.agg <- data.table(review_control)
review.control.agg <- review.control.agg[,list(total.votes.cool=sum(votes.cool),total.votes.funny=sum(votes.funny),total.votes.useful=sum(votes.useful),average.star=mean(stars),review.counts=.N),by="unumID"]

#Merge with user table to get yelping_since
user_treat <- merge(user,review.treat.agg,by="unumID")
user_control <- merge(user,review.control.agg,by="unumID")

#Label 1 for treatment and 0 for control
user_treat$tc <- rep(1,nrow(user_treat))
user_control$tc <- rep(0,nrow(user_control))

#Combine treament and control into one data frame, calculate yeargap:2012-yelping_since
mdm_data <- rbind(user_treat,user_control)
mdm_data$yeargap <- 2012-year(as.Date(paste(mdm_data$yelping_since,"-01",sep="")))

#Normalize votes by review.counts
mdm_data$norm.votes.cool <- mdm_data$total.votes.cool/mdm_data$review.counts
mdm_data$norm.votes.funny <- mdm_data$total.votes.funny/mdm_data$review.counts
mdm_data$norm.votes.useful <- mdm_data$total.votes.useful/mdm_data$review.counts

#1:1 MDM (if you want to do 1:k match, specify ratio=k)
mahalanobis <- matchit(tc~norm.votes.cool+norm.votes.funny+norm.votes.useful+average.star+review.counts+yeargap,data=mdm_data,distance="mahalanobis")

summary(mahalanobis)
plot(mahalanobis)

#Get control id from match.matrix
control.index <- c(as.numeric(mahalanobis$match.matrix[,1]))
control_id <- mdm_data$unumID[control.index]

#Get reviews written by control group during pre-treatment period
review_control_pre <- subset(review2013_2015,as.Date(review2013_2015$date,"%Y-%m-%d")>="2011-07-01"&as.Date(review2013_2015$date,"%Y-%m-%d")<="2011-12-31")

#Get control user ID who wrote a review during pre-treatment period
c_id <- unique(review_control_pre$unumID)

#Get reviews written by control group during post-treatment period
review_control_post <- subset(review2013_2015,as.Date(review2013_2015$date,"%Y-%m-%d")>="2012-07-01"&as.Date(review2013_2015$date,"%Y-%m-%d")<="2012-12-31")

#Get control ids who wrote reviews in both pre and post treatment period
c_id <- intersect(c_id,unique(review_control_post$unumID))

#Get final control ids who is matched with treatment group as well as wrote reviews during both periods
final_c_id <- intersect(c_id,control_id)

write.table(final_c_id,file="control_id_4m.csv",sep=",",row.names = FALSE)
write.table(treat_id,file="treat_id_4m.csv",sep=",",row.names=FALSE)

