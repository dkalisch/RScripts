# Get data
## Load the data from CSV file
ERWaiting <- read.csv2("data/ERWaiting.csv", header = TRUE, sep = ",", dec = ".")

## Melt data to a long format
ERWaiting <- melt(ERWaiting, variable.name = "hospital", value.name = "waitingTime", id.vars = "day")
ERWaiting$hospital <- as.factor(gsub("\\.", " ", ERWaiting$hospital)) # Clean up the names by removing the dot
