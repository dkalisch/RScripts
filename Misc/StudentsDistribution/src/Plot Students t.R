# Settings
## Distribution specific settings
df <- 5 # Degree of freedom first parameter
alpha <- 0.05 # Alpha level
length <- 500 # number of elements

## Display settings
ncp <- 0 # Non centrality parameter
frameEmpty <- "black" # Color for the empty frame
areaEmpty <- "white" # Color for the empty area
frameH0 <- "green4" # Color for the H0 frame
areaH0 <- "green2" # Color for the H0 area
frameH1 <- "red4" # Color for the H1 frame
areaH1 <- "red2" # Color for the H1 area

# Data preperation
x <- seq(from = -4, to = 4, length = length) # Set vector range 
dd <- data.frame(x = x, y = dt(x, df = df, ncp = 0)) # Create data frame

# Ploting the empty distribution image
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
ggsave(file="graphs/t-distributionEmpty.pdf", p)

# Ploting the image H0
## Outline plot
### Create a empty line plot
p <- ggplot(data = dd)
p <- p + labs(y = "Relative frequency")
p <- p + geom_area(aes(x = x, y = y),
                   fill = areaH0, color = frameH0)

p <- p + geom_area(data = subset(dd, x > qt(p = 1-alpha/2, df = df)),
                   aes(x = x, y = y),
                   fill = areaEmpty, color = frameEmpty)
p <- p + geom_area(data = subset(dd, x < qt(p = alpha/2, df = df)),
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
ggsave(file="graphs/t-distributionH0.pdf", p)

# Ploting the image H0 + H1
## Outline plot
### Create a empty line plot
p <- ggplot(data = dd)
p <- p + labs(y = "Relative frequency")
p <- p + geom_area(aes(x = x, y = y),
                   fill = areaH0, color = frameH0)
p <- p + geom_area(data = subset(dd, x > qt(p = 1-alpha/2, df = df)),
                   aes(x=x, y = y),
                   fill = areaH1, color = frameH1)
p <- p + geom_area(data = subset(dd, x < qt(p = alpha/2, df = df)),
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
ggsave(file="graphs/t-distributionH1.pdf", p)