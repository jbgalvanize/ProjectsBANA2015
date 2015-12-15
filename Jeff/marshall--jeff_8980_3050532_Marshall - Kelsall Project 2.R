        ## LOAD PROJECT AND LIBRARIES

project2 = read.csv(file.choose())
attach = project2
head(project2)
str(project2)


## Install/run 'plyr' library
install.packages('plyr')
library(plyr)

## Install/run 'caret' library
install.packages('caret')
library(caret)

## Install/run 'ggplot2' library
install.packages('ggplot2')
library(ggplot2)

## Install/run 'lubridate' library
install.packages('lubridate')
library(lubridate)

          ## CLEAN/PREPARE DATA

## Limit our dataframe to potentially useful columns/variables

project2 = project2[c("id", "kickstarter_id", "name", "slug", "blurb", "creator", "main_category",
                      "sub_category", "launched_at", "deadline", "state", "goal", "pledged",
                      "backers_count", "photo", "web_url", "currency")]

## 'id' and 'kickstarter_id' seem to be superfluous row identifiers, so they are also removed
## 'web_url' and 'photo' are also useless, and are also removed.  

str(project2$id)
str(project2$kickstarter_id)
project2 = project2[c("name", "slug", "blurb", "creator", "main_category",
                      "sub_category", "launched_at", "deadline", "state", "goal", "pledged",
                      "backers_count", "currency")]

## Remove time-field string from 'launched_at' and 'deadline' dates to make them easier to work with

project2$launched_at = format(as.POSIXct(project2$launched_at,format='%m/%d/%Y'))
project2$deadline = format(as.POSIXct(project2$deadline,format='%m/%d/%Y'))

## Create new column 'campaign_duration' expressing the time difference between the 
## 'launched_at' and 'deadline' columns in days. Then we format it as numeric. 

project2$campaign_duration = difftime(project2$deadline, project2$launched_at, units = "days")
project2$campaign_duration = as.numeric(project2$campaign_duration)

## Create two new columns showing on which day of the week a project launched and ended

project2$start_day = wday(project2$launched_at, label = TRUE, abbr = TRUE)
project2$end_day = wday(project2$deadline, label = TRUE, abbr = TRUE)

## This created ordered factors which is incorrect, so we need to unorder them

project2$start_day = factor(project2$start_day, ordered = FALSE)
project2$end_day = factor(project2$end_day, ordered = FALSE)

## 'state' column needs to be simplified. Recode 'Canceled' and 'Deleted' to 'Failed', 
## as they are functionally the same for our purposes

project2$state = revalue(project2$state, c("canceled"="failed", "deleted"="failed", " "="failed"))

## Remove rows with a State of 'live' or 'suspended' as these rows do not provide
## a clear answer as to whether a project succeeded or failed

project2 = project2[project2$state!="live" & project2$state!="suspended",]

## drop unused factor levels from 'state' so that our graphs are pretty 

project2$state = factor(project2$state)
summary(project2$state)

## Create new column 'average_pledge'

project2$average_pledge = project2$pledged / project2$backers_count

## This column has NA values because some project's received 0 pledges. So we recode 
## all NA values in 'average_pledge' to 0

project2$average_pledge[is.na(project2$average_pledge)] = 0

##  Check columns for NA values, drop rows where they occur (we only lose 2 rows)

project2 = na.omit(project2)

    ## NOW OUR DATA IS CLEANED AND READY TO ANALYZE (NAs removed, unused factor levels removed, and
    ## derived new columns with useful information)


## This code created subsetted data, one set of all 'Successful', on of all 'Failed', 
## and then droped unused levels

SuccessSet = subset(project2, project2$state=="successful")
FailedSet = subset(project2, project2$state=="failed")
levels(SuccessSet$state) = factor(SuccessSet$state)
levels(FailedSet$state) = factor(FailedSet$state)
summary(SuccessSet$state)
summary(FailedSet$start_day)
summary(SuccessSet$start_day)


## This recodes success/failure to 1/0, and drops a blank factor level

project2$state = revalue(project2$state, c("successful"="1", "failed"="0"))
summary(project2$state)
project2$state = factor(project2$state)
summary(project2$sub_category)
project2$sub_category=factor(project2$sub_category)

testproject=project2
testproject$sub_category=factor(testproject$sub_category)
summary(testproject$state)
str(testproject)

#######################  EDA ANALYSIS  ############################

# Boxplots

boxplot(project2$state,project2$goal,
        xlab="", 
        ylab="Goal Amount in Local Currency", 
        main="Project State by Funding Goal")
axis(1, at = c(1,2),labels = expression(Failed, Successful))

boxplot(project2$state,project2$pledged,
        xlab="", 
        ylab="Amount Pledged in Local Currency", 
        main="Project State by Amount Pledged")
axis(1, at = c(1,2),labels = expression(Failed, Successful))

boxplot(project2$state,project2$backers_count,
        xlab="", 
        ylab="Number of Backers", 
        main="Project State by Number of Backers")
axis(1, at = c(1,2),labels = expression(Failed, Successful))

boxplot(project2$state,project2$campaign_duration,
        xlab="", 
        ylab="Campaign Duration in Days", 
        main="Project State by Campaign Duration")
axis(1, at = c(1,2),labels = expression(Failed, Successful))

boxplot(project2$state,project2$average_pledge, 
        xlab="", 
        ylab="Average Pledge in Local Currency", 
        main="Project State by Average Pledge")
axis(1, at = c(1,2),labels = expression(Failed, Successful))

##### SEPERATE DATA INTO TWO NEW DATA SETS, Successful v Failed

SuccessSet = subset(project2, project2$state=="successful")
SuccessFailed = subset(project2, project2$state=="failed")

# Campaign Duration
summary(project2$campaign_duration)
hist(project2$campaign_duration, 
     xlab="Campaign Duration in Days",
     main="Campaign Duration - All States")
mean(project2$campaign_duration)
mean(SuccessSet$campaign_duration)
mean(SuccessFailed$campaign_duration)

summary(SuccessSet$campaign_duration)
hist(SuccessSet$campaign_duration,
     xlab="Campaign Duration in Days",
     main="Successful Campaign Duration in Days")

summary(SuccessFailed$campaign_duration)
hist(SuccessFailed$campaign_duration, 
     xlab="Campaign Duration in Days",
     main="Failed Campaign Duration")
hist(SuccessFailed$campaign_duration, 
     col=rgb(1,0,0,0.5), 
     xlab="Campaign Duration in Days", 
     main="Histogram of Campaign Duration by State")
hist(SuccessSet$campaign_duration, 
     col=rgb(0,0,1,0.5), 
     add=T)

# Barplots
plot(project2$campaign_duration, project2$backers_count, 
     xlab="Campaign Duration in Days", 
     ylab="Number of Backers", 
     main="Campaign Duration by Number of Backers")
summary(lm(project2$campaign_duration~project2$backers_count))

## Removing infinite value from average pledge
project2[mapply(is.infinite, project2)] = 0

#### How did you remove the one data point???
fix(project2)
barplot(project2$campaign_duration, project2$average_pledge)
summary(lm(project2$campaign_duration~project2$start_day))

# Currency Analysis
counts <- table(project2$state, project2$currency)
barplot(counts, 
        main="Currency Analysis",
        xlab="Currency", 
        col=c("darkblue","red"),
        legend = rownames(counts))
summary(glm(project2$state~project2$currency, binomial(link="logit")))

# Start Day Analyis
counts <- table(project2$state,project2$start_day)
barplot(counts, 
        main="Start Day Analysis",
        xlab="Start Day", 
        ylab="Frequency",
        col=c("darkblue","red"),
        legend = rownames(counts))
summary(glm(project2$state~project2$start_day, binomial(link="logit")))

# End Day Analyis
counts <- table(project2$state,project2$end_day)
barplot(counts, 
        main="End Day Analysis",
        xlab="End Day", 
        ylab="Frequency",
        col=c("darkblue","red"),
        legend = rownames(counts))
summary(glm(project2$state~project2$end_day, binomial(link="logit")))

# Category Analysis
counts <- table(project2$state,project2$main_category)
barplot(counts, 
        main="Main Category",
        xlab="Category",
        ylab="Frequency",
        col=c("darkblue","red"),)
summary(glm(project2$state~project2$main_category, binomial(link="logit")))

######## Logistic Regression Model after forward selection (based on AIC)  ####


summary(LogisticMod)

LogisticMod = glm(state~goal+backers_count+campaign_duration
                  +main_category+sub_category+start_day+end_day,
                  family = binomial(link=logit), data = project2)

summary(LogisticMod)


train_ind = sample(nrow(project2), nrow(project2)/2)
train_project2 = project2[train_ind,]
test_project2 = project2[-train_ind,]

test_project2$main_category = factor(test_project2$main_category)


str(train_project2)
str(test_project2)

LogisticModTrained = glm(state~goal+backers_count+campaign_duration
                         +main_category+sub_category+start_day+end_day,
                         family = binomial(link=logit), data = train_project2)

mod_train_preds = predict(LogisticModTrained, train_project2, type = "response")
mod_test_preds = predict(LogisticModTrained, test_project2, type = "response")

train_pred = rep("0", length(mod_train_preds))
train_pred[mod_train_preds > 0.5] = "1"

test_pred = rep("0", length(mod_test_preds))
test_pred[mod_test_preds > 0.5] = "1"

table(train_project2$state, train_pred)
table(test_project2$state, test_pred)




