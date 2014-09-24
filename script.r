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


# require("scripts/load_data_first_time.r")
# require("scripts/split_dataset.r")

#####

data_main <- read.csv("data/csv/data_main.csv")
data_visits <- read.csv("data/csv/data_visits.csv")
data_birth_details <- read.csv("data/csv/data_birth_details.csv")

# Some descriptive statistics

qplot(bmi, height, data=data_main)
# suggests thare are some coding errors regarding height; a number of observations over 5m tall
data_main_height_error <- subset(data_main, height > 2)
data_main_bmi_error <- subset(data_main, bmi > 100)

data_birth_details_error1 <- subset(data_birth_details,
                                baby_birth_wta <= 7 | baby_birth_wta >= 250
                                )
data_birth_details_error2 <- subset(data_birth_details,
                                   baby_birth_weight >= 5.5
                                  )

qplot(bp_ratio, data=data_main)
# also suggests one or two coding errors

qplot(baby_birth_weight, data=subset(data_birth_details, subset=!is.na(baby_sex)), 
      geom="histogram", binwidth=.2) -> p

p + facet_grid(. ~ baby_sex) + geom_vline(x=5.5)

data_baby_birth_weight_error <- subset(data_birth_details, baby_birth_weight > 5.5)

# ACTION: ASK FOR DEFINITION OF AGE LABELS


# collate record numbers assumed to be errors 

error_records <- c(
  data_main_height_error$record_number,
  data_main_bmi_error$record_number,
  data_birth_details_error1$record_number,
  data_birth_details_error2$record_number,
  data_baby_birth_weight_error$record_number
  )
error_records <- sort(unique(error_records))

# look at the data without the errorenous errors 

data_main_e <- subset(data_main, subset=!(record_number %in% error_records))
data_visits_e <- subset(data_visits, subset=!(record_number %in% error_records))
data_birth_details_e <- subset(data_birth_details, subset=!(record_number %in% error_records))

# visualise the new subsets without errors

qplot(bmi, height, data=data_main_e)
# Additional possible errors: bmi < 10

(error_low_bmi <- subset(data_main_e, subset=bmi < 10)$record_number)
error_records <- c(error_records, error_low_bmi)
error_records <- sort(unique(error_records))

# iterate again

data_main_e <- subset(data_main, subset=!(record_number %in% error_records))
data_visits_e <- subset(data_visits, subset=!(record_number %in% error_records))
data_birth_details_e <- subset(data_birth_details, subset=!(record_number %in% error_records))


qplot(bmi, height, data=data_main_e)
# some people with low height that's not part of the usual distribution, this this
# could be plausible

(very_low_height <- subset(data_main_e, subset = height < 1.25)$record_number)

# 
(qplot(baby_birth_weight, data=subset(data_birth_details_e, subset=!is.na(baby_sex)), 
      geom="histogram") -> p)

p + facet_grid(. ~ baby_sex) + geom_vline(x=5.5)
# no definite big outliers here

head(arrange(data_birth_details_e, baby_birth_weight))


## now to look at the visits data

head(data_visits_e)

# now to try lubridate

dates_of_interest <- data_visits_e$date

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
    tmp <- dates[i] - t0
    days_since_first_visit[i] <- as.numeric(tmp)
    }
  out <- data.frame(x, days_since_first_visit=days_since_first_visit)
  return(out)
}

data_visits_e <- ddply(data_visits_e, .(record_number), fn)

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
