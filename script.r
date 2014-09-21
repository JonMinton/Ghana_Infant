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


#####

data_main <- read.csv("data/csv/data_main.csv")


# Some descriptive statistics

qplot(bmi, height, data=data_main)
# suggests thare are some coding errors regarding height; a number of observations over 5m tall


qplot(bp_ratio, data=data_main)
# also suggests one or two coding errors



# ACTION: ASK FOR DEFINITION OF AGE LABELS

