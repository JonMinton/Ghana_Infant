rm(list=ls())

#source("scripts/LoadPackages.R")

require(ggplot2)
require(lubridate)
require(stringr)
require(plyr)
require(dplyr)
require(tidyr)
require(car)


#source("scripts/load_data_first_time.r")
#source("scripts/split_dataset.r")

#####

data_main <- read.csv("data/csv/data_main.csv") %>% tbl_df()
data_visits <- read.csv("data/csv/data_visits.csv") %>% tbl_df()
data_birth_details <- read.csv("data/csv/data_birth_details.csv") %>% tbl_df()


###

# Start with data_main

summary(data_main)

# The following should be converted to factors

tmp <- c(
  "health_centre",
  "health_status",
  "health_insurance_status",
  "parity",
  "works",
  "occupation_main",
  "religion",
  "belief_cannot_control_health",
  "belief_things_do_affect_pregnancy",
  "behaviours_exercise",
  "behaviours_fruit_veg",
  "behaviours_less_bad_food",
  "behaviours_less_salt",
  "behaviours_eat_protein",
  "behaviours_eat_carbs",
  "behaviours_lower_blood_pressure",
  "behaviours_control_diabetes",
  "behaviours_smoke_less",
  "seen_nurse_last_year",
  "breastfeeding",
  "ante_natal_vitamins",
  "pregnancy_wanted",
  "delivery_complications",
  "family_planning_important",
  "used_contraceptives",
  "family_planning",
  "family_planning_who_responsible",
  "family_planning_charges",
  "family_planning_charges_expensive"
  )

for (i in 1:length(tmp)){
  data_main[[tmp[i]]] <- as.factor(data_main[[tmp[i]]])
}
rm(tmp)



###

qplot(bmi, height, data=data_main)

# Suggests just one possible error now, a height under 1.3


error_df <- data.frame(
  record_number=subset(data_main, height < 1.3)$record_number,
  reason="outlier: low height"
  )

# Exploring any potential outliers in bp_ratio

qplot(bp_ratio, data=data_main, binwidth=0.1) + geom_vline(aes(xintercept=c(0.32, 2.75)), linetype="dashed")

# adding additional records to error_df

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


# Exploring weights
# Very high weights:
qplot(weight, data=data_visits, binwidth=0.2) + geom_vline(aes(xintercept=c(30, 250)), linetype="dashed")

# Adding records to error_df
error_df <- rbind(
  error_df,
  data.frame(
    record_number=subset(data_visits, weight < 30)$record_number,
    reason="weight below 30"
    ),
  data.frame(
    record_number=subset(data_visits, weight > 250)$record_number,
    reason="weight above 250"
    )
  )


## now to look at the visits data

data_visits

qplot(y=weight, x=gestation, data=data_visits) + geom_vline(aes(xintercept=c(50)), linetype="dashed") + 
  geom_hline(aes(yintercept=c(30,250)), linetype="dashed")


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

# process
# 1) remove NA entries to visit number
# 2) remove negative days_since_first_visit
# 3) remove positive days_since_first_visit greater than top 1% of values

data_visits <- data_visits %>% tbl_df()

data_visits_ok <- data_visits %>% 
  filter(!is.na(date)) %>%
  filter(days_since_first_visit >= 0) %>% 
  filter(days_since_first_visit < quantile(data_visits$days_since_first_visit, 0.99, na.rm=T)) %>%
  filter(weight > 30 & weight < 250) %>%
  filter(gestation < 50)

# Plot days since first visit and visit number 
# for the records still retained


qplot(
  x=visit_number,
  y=days_since_first_visit, 
  group=record_number,
  colour=record_number,
  data=data_visits_ok,
  geom="line"
  )

qplot(
  x=days_since_first_visit,
  y=weight, 
  data=data_visits_ok
) + geom_hline(mapping=aes(yintercept=c(30,250)), linetype="dashed")


#Saving file with records that seem OK
# (Commenting out as file already created)

# write.csv(
#   data_visits_ok,
#   file="data/data_visits_with_days_since_first_visit_ok_only.csv",
#   row.names=F
#   )

# Take a random sample of 25 records and display change in weight over time

tmp <- data_visits_ok %>% select(record_number) %>% unique() %>% sample_n(25) %>% unlist() %>% as.numeric()

data_visits_ok %>% filter(record_number %in% tmp) %>% ggplot() +
  geom_line(aes(x=visit_number, y=weight)) +
  facet_wrap( ~ record_number)



