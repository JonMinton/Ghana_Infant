data <- read.csv("data/csv/dataset.csv", na.strings = "#NULL!")
names(data)

# New addition (1/1/2015)

# Add corrected data

data_corrected <- read.csv("data/data_corrected_31_12_2014.csv", na.strings = "#NULL!")
data_corrected$RECNUM <- as.integer(as.character(data_corrected$RECNUM))
data$RECNUM <- as.integer(as.character(data$RECNUM))


paste_over <- function(original, corrected, key="RECNUM"){
  corrected <- corrected[order(corrected[[key]]),]
  
  output <- original
  output[
    original[[key]] %in% corrected[[key]],
    names(corrected)
    ] <- corrected
  
  return(output)
}

data <- paste_over(data, data_corrected, key="RECNUM")


name_details <- read.csv("data/csv/definitions.csv")
names(data) <- name_details$revised

source("scripts/split_dataset.r")
