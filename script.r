rm(list=ls())

source("scripts/LoadPackages.R")

RequiredPackages(
  c(
  "ggplot2",
  "reshape2",
  "plyr",
  "car",
  "stringr",
  "lubridate"
    )
  )


source("scripts/load_data_first_time.r")
source("scripts/split_dataset.r")

#####

data_main <- read.csv("data/csv/data_main.csv")
data_visits <- read.csv("data/csv/data_visits.csv")
data_birth_details <- read.csv("data/csv/data_birth_details.csv")


# Redoing with new values pasted over 


qplot(bmi, height, data=data_main)

# Suggests just one possible error now, a height under 1.3


error_df <- data.frame(
  record_number=subset(data_main, height < 1.3)$record_number,
  reason="outlier: low height"
  )



qplot(bp_ratio, data=data_main, binwidth=0.1) + geom_vline(aes(xintercept=c(0.32, 2.75)))

error_df <- rbind(
  error_df,
  data.frame(
    record_number=subset(data_main, bp_ratio < 0.32)$record_number,
    reason="bp ratio almost 0"
    ),
  data.frame(
    record_number=subset(data_main, bp_ratio>2.75)$record_number,
    reason="bp ratio over 2.75"
    )
  )


# Very high weights:
qplot(weight, data=data_visits, binwidth=0.2) + geom_vline(aes(xintercept=c(30, 170)))

error_df <- rbind(
  error_df,
  data.frame(
    record_number=subset(data_visits, weight < 30)$record_number,
    reason="weight below 30"
    ),
  data.frame(
    record_number=subset(data_visits, weight > 170)$record_number,
    reason="weight above 170"
    )
  )



## now to look at the visits data

head(data_visits)

qplot(y=weight, x=gestation, data=data_visits) + geom_vline(aes(xintercept=c(50))) + 
  geom_hline(aes(yintercept=c(20,170)))


error_df <- rbind(
  error_df,
  data.frame(
    record_number=subset(data_visits, gestation > 50)$record_number,
    reason="gestation over 50"
  )
)



# now to try lubridate

dates_of_interest <- data_visits$date

# need to change months into values 


replace_runner <- function(target, pattern, replace){  
  n <- length(pattern)
  for (i in 1:n){
    target <- str_replace_all(target, pattern=pattern[i], replace=replace[i])
  }  
  return(target)
}
  
dates_of_interest <- replace_runner(
  dates_of_interest,
  pattern=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  replace=c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  )


# now to use lubridate, specifying day, month, year format

dates_of_interest <- dmy(dates_of_interest)
data_visits$date <- dates_of_interest

# can now calculate difference in number of days and so on. 

# want to calculate dates since first visit

fn <- function(x){
  require(lubridate)

  t0 <- x$date[x$visit_number==1]
  dates <- x$date
  n <- length(dates)
  days_since_first_visit <- rep(NA, n)
  for (i in 1:n){ 
    tmp <- dates[i] - t0
    days_since_first_visit[i] <- as.numeric(tmp)
    }
  out <- data.frame(x, days_since_first_visit=days_since_first_visit)
  return(out)
}

data_visits <- ddply(data_visits, .(record_number), fn)

qplot(
  x=days_since_first_visit,
  y=weight, 
  group=record_number,
  colour=record_number,
  data=data_visits,
  geom="line"
  )

qplot(
  x=days_since_first_visit,
  y=weight, 
  data=data_visits
)

write.csv(
  data_visits,
  file="data/data_visits_with_days_since_first_visit.csv",
  row.names=F
  )
# Save it for use 
# we can now use this to look at change in weight since first visit

qplot(x=days_since_first_visit, y=weight, data=data_visits_e, group=record_number, geom="line")

# some babies have a very high reported weight

error_baby_weight <- subset(data_visits_e, subset=weight > 200)$record_number

# iterate again: remove these observations 
error_records <- c(error_records, error_baby_weight)
error_records <- sort(unique(error_records))

data_main_e <- subset(data_main, subset=!(record_number %in% error_records))
data_visits_e <- subset(data_visits, subset=!(record_number %in% error_records))
data_birth_details_e <- subset(data_birth_details, subset=!(record_number %in% error_records))


qplot(x=days_since_first_visit, y=weight, data=data_visits_e, group=record_number, geom="line")



# need to change months into values 


replace_runner <- function(target, pattern, replace){  
  n <- length(pattern)
  for (i in 1:n){
    target <- str_replace_all(target, pattern=pattern[i], replace=replace[i])
  }  
  return(target)
}

dates_of_interest <- replace_runner(
  dates_of_interest,
  pattern=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  replace=c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
)


# now to use lubridate, specifying day, month, year format

dates_of_interest <- dmy(dates_of_interest)
data_visits_e$date <- dates_of_interest

# can now calculate difference in number of days and so on. 

# want to calculate dates since first visit


fn <- function(x){
  require(lubridate)
  
  t0 <- x$date[x$visit_number==1]
  dates <- x$date
  n <- length(dates)
  days_since_first_visit <- rep(NA, n)
  for (i in 1:n){ 
    tmp <- new_interval(t0, dates[i]) 
    days_since_first_visit[i] <- tmp / ddays(1)
  }
  out <- data.frame(x, days_since_first_visit=days_since_first_visit)
  return(out)
}

data_visits_e <- ddply(data_visits_e, .(record_number), fn)

# we can now use this to look at change in weight since first visit

qplot(x=days_since_first_visit, y=weight, data=data_visits_e, group=record_number, geom="line")


# there are still a range of errors with the dates: save as csv file and send back to Tony

write.csv(data_visits_e, "data/csv/visit_date_data_to_recheck.csv")
