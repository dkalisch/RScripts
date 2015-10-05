# Settings
## Distribution specific settings
df1 <- 3 # Degree of freedom first parameter
df2 <- 8 # Degree of freedom second parameter
alpha <- 0.05 # Alpha level
length <- 500 # number of elements

## Display settings
ncp <- 0 # Non centrality parameter
frameEmpty <- "black" # Color for the empty frame
areaEmpty <- "white" # Color for the empty area
frameH0 <- "green4" # Color for the H0 frame
areaH0 <- "green3" # Color for the H0 area
frameH1 <- "red4" # Color for the H1 frame
areaH1 <- "red2" # Color for the H1 area

# Data preperation
x <- seq(from = 0, to = 25, length = length) # Set vector range 
dd <- data.frame(x = seq(from = 0, to = 25, length = length),  # Create data frame
                 y = df(x = x, df1 = df1, df2 = df2, ncp = ncp))

# Ploting the images
## Outline plot
### Create a empty line plot
p <- ggplot(data = dd)
p <- p + labs(y = "Relative frequency")
p <- p + geom_area(aes(x = x, y = y),
                   color = frameEmpty, fill = areaEmpty)

### Set empty display
p <- p + theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour= "black"),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.background = element_rect(fill = "transparent",colour = NA))

### Safe the plot
ggsave(file="graphs/f-distributionEmptya.pdf", p)


## H0 plot
### Create a empty line plot
p <- ggplot(data = dd)
p <- p + labs(y = "Relative frequency")
p <- p + geom_area(aes(x = x, y = y),
                   color = frameH0, fill = areaH0)
p <- p + geom_area(data = subset(dd, x > qf(p = 1-alpha, df1 = df1, df2 = df2, ncp = ncp)),
                   aes(x = x, y = y),
                   fill = areaEmpty, color = frameEmpty)

### Set empty display
p <- p + theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour= "black"),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.background = element_rect(fill = "transparent",colour = NA))

### Safe the plot
ggsave(file="graphs/f-distributionH0a.pdf", p)

## H2 plot
### Create a empty line plot
p <- ggplot(data = dd)
p <- p + labs(y = "Relative frequency")
p <- p + geom_area(aes(x = x, y = y),
                   color = frameH0, fill = areaH0)
p <- p + geom_area(data = subset(dd, x > qf(p = 1-alpha, df1 = df1, df2 = df2, ncp = ncp)),
                   aes(x = x, y = y),
                   fill = areaH1, color = frameH1)

### Set empty display
p <- p + theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour= "black"),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.background = element_rect(fill = "transparent",colour = NA))

### Safe the plot
ggsave(file="graphs/f-distributionH1a.pdf", p)
