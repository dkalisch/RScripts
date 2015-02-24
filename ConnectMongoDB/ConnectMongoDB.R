Sys.setenv(NOAWT=1) # To avoid under Apple the AWT problem

# Load modules
library(RMongo) # Load MongoDB connector

# Connect to the database
mongo <- mongoDbConnect("twitter", host = '137.251.109.189', port = 27017)

# Create a query on the connector
results <- dbGetQuery(mongo, "tweets", "{}", 0, 2)

# Clean up
Sys.unsetenv("NOAWT")