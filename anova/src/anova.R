# Load data
source("src/loadData.R")


# Global Settings
alpha = 0.05 # Define the desired level of significance


# Descriptive summary
ERWaiting.summary <- describe(dcast(ERWaiting, day~hospital, value.var = "waitingTime")[,-1])


# ANOVA
## Calculating ANOVA results
fit <- lm(data = ERWaiting, waitingTime~hospital) # calculate the linear regression model
ERWaiting.aov <- aov(fit) # Get the ANOVA results of the regression model
ERWaiting.aov.summary <- summary(ERWaiting.aov) # Get the ANOVA summary
ERWaiting.tukey <- TukeyHSD(ERWaiting.aov) # Perform the Tukey-Kramer procedure

## Check if the assumptions are met
### Normality
ERWaiting.normal <- data.frame(skew = ERWaiting.summary$skew, # Get Skewness statistic
                               kurt = ERWaiting.summary$kurtosis) # Get Kurtosis statistic

## Homogenity of variance
ERWaiting.levene <- leveneTest(fit) # Conduct a Levene test


# Get values for further calculations
## Get the degrees of freedom
df <- anova(fit)[, "Df"] 
names(df) = c("between", "within") # Rename the columns 

## Get the f values
ERWaiting.f.crit <- qf(alpha, df["between"], df["within"], lower.tail = FALSE) # Get F-critical value
ERWaiting.f.value <- ERWaiting.aov.summary[[1]]$F[1] # Get actual F-value


# Plot ANOVA results
## Plot Boxplot of the data set
bp <- ggplot(ERWaiting, aes(x = hospital, y = waitingTime)) + # Create plor
        stat_boxplot(geom ='errorbar') + # Add error bars to the boxplot
        geom_boxplot() + # Add boxplot
        labs(y = "Average Waiting Time", x = "Hospitals") # Tidy up the axis title

## Save plot
ggsave("graphs/ERboxplot.pdf", bp)

## Plot F-distribution result
### Display settings
ncp <- 0 # Non centrality parameter
frameEmpty <- "black" # Color for the empty frame
areaEmpty <- "white" # Color for the empty area
frameH0 <- "green4" # Color for the H0 frame
areaH0 <- "green3" # Color for the H0 area
frameH1 <- "red4" # Color for the H1 frame
areaH1 <- "red2" # Color for the H1 area

### Distribution specific settings
df1 <- df[1] # Degree of freedom first parameter
df2 <- df[2] # Degree of freedom second parameter
length <- 500 # number of elements

# Data preperation
x <- seq(from = 0, to = ERWaiting.f.value+2, length = length) # Set vector range 
dd <- data.frame(x = seq(from = 0, to = ERWaiting.f.value+2, length = length),  # Create data frame
                 y = df(x = x, df1 = df1, df2 = df2, ncp = ncp))

# Create F-distribution plot
pf <- ggplot(data = dd) # Create the plot
pf <- pf + labs(y = "Relative frequency", x = "F-values") # Tidy up the axis title
pf <- pf + geom_area(aes(x = x, y = y),
                     color = frameH0, fill = areaH0) # Add the H0 area
pf <- pf + geom_area(data = subset(dd, x > ERWaiting.f.crit),
                     aes(x = x, y = y),
                     fill = areaH1, color = frameH1) # Add the H1 area
pf <- pf + geom_vline(xintercept = ERWaiting.f.crit, colour = frameH1, linetype = "longdash") # Add the F-critical value line
pf <- pf + geom_vline(xintercept = ERWaiting.f.value, colour = "black", linetype = "dotted") # Add the F-value line
pf <- pf + scale_x_continuous(breaks = sort(round(c(seq(from = min(dd$x), # Add tick marks for the F values
                                                        to = round(max(dd$x),0),
                                                        by = 2), ERWaiting.f.crit, ERWaiting.f.value),2)))
pf <- pf + annotate("text", y = .6, x = ERWaiting.f.value + 1,
                    label = paste("Pr(>F) = ", round(ERWaiting.aov.summary[[1]]$Pr[1],3))) # Add p-value to plot

# Save plot
ggsave("graphs/fDistribution.pdf", pf)
