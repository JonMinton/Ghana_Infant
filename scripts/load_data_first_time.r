data <- read.csv("data/csv/dataset.csv", na.strings = "#NULL!")
names(data)

name_details <- read.csv("data/csv/definitions.csv")
names(data) <- name_details$revised

source("scripts/split_dataset.r")