rm(list=ls())

source("scripts/LoadPackages.R")

RequiredPackages(
  c(
  "ggplot2",
  "reshape2",
  "plyr",
  "car",
  "stringr"
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

data_birth_details_error <- subset(data_birth_details,
                                baby_birth_wta <= 7 | baby_birth_wta >= 250
                                )
data_birth_details_error <- subset(data_birth_details,
                                   baby_birth_weight >= 5.5
                                  )

qplot(bp_ratio, data=data_main)
# also suggests one or two coding errors

qplot(baby_birth_weight, data=data_birth_details, 
      colour=as.factor(baby_sex), 
      group=as.factor(baby_sex), geom="density") -> p




# ACTION: ASK FOR DEFINITION OF AGE LABELS

