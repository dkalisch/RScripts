# Load the required library
library(deSolve) # Solvers for Differential Equations
library(ggplot2) # Plot results
library(grid) # For use of the unit function

# Create function to calculate Lorenze curve
Lorenz <- function(t, state, parameters) {
  with(as.list(c(state, parameters)),{ # Create list with parameter results
    ## Rate of change
    dX <- a*X + Y*Z
    dY <- b * (Y-Z)
    dZ <- -X*Y + c*Y - Z
    
    # Return the rate of change
    list(c(dX, dY, dZ))
  }
  )
}

# Define Parameters
# Controlling parameters
parameters <- c(a = -8/3, b = -10, c = 28)

# Initial state
state <- c(X = 1, Y = 1, Z = 1)

# Integrations times
times <- seq(0, 100, by = 0.001)


# Perform the integration and assign it to variable 'out'
out <- ode(y = state, times = times, func = Lorenz, parms = parameters)

# Plots
## Plot with XY
pXY <- ggplot(as.data.frame(out)) +
  geom_path(aes(X, Y, col = time, alpha = Z)) + 
  theme(legend.position = "none")

ggsave(filename = "../graphs/lorenzXY.png", pXY, bg = "transparent")

# Plot with ZY
pZY <- ggplot(as.data.frame(out)) +
  geom_path(aes(Z, Y, col = time, alpha = X)) + 
  theme(legend.position = "none")

ggsave(filename = "../graphs/lorenzZY.png", pZY, bg = "transparent")

# Plot with XZ
pXZ <- ggplot(as.data.frame(out)) +
  geom_path(aes(X, Z, col = time, alpha = Y)) + 
  theme(legend.position = c(1.1,0.5))

ggsave(filename = "../graphs/lorenzXZ.png", pXZ, bg = "transparent")

# Plot 3D without plot space
base_size = 12 # Set base size for annotations

## Plot graph
p3D <- ggplot(as.data.frame(out)) +
  geom_path(aes(X*Y, X*Z, col = time, alpha = Y*Z)) +
  scale_alpha(range = c(0.4, 0.8)) + 
  theme(legend.position = 'none',
        axis.line =         element_blank(),
        axis.text.x =       element_text(colour = NA,size = base_size * 0.8 , lineheight = 0.9, vjust = 1),
        axis.text.y =       element_text(colour = NA,size = base_size * 0.8, lineheight = 0.9, hjust = 1),
        axis.ticks =        element_line(colour = NA, size = 0.2),
        axis.title.x =      element_text(colour = NA,size = base_size, vjust = 1),
        axis.title.y =      element_text(colour = NA,size = base_size, angle = 90, vjust = 0.5),
        axis.ticks.length = unit(0.3, "lines"),
        axis.ticks.margin = unit(0.5, "lines"),
        
        legend.background = element_rect(colour=NA), 
        legend.key =        element_rect(colour = NA),
        legend.key.size =   unit(1.2, "lines"),
        legend.text =       element_text(colour = NA,size = base_size * 0.8),
        legend.title =      element_text(colour = NA,size = base_size * 0.8, face = "bold", hjust = 0),
        legend.position =   "right",
        
        panel.background =  element_rect(fill = NA, colour = NA), 
        panel.border =      element_rect(fill = NA, colour=NA), 
        panel.grid.major =  element_line(colour = NA, size = 0.2),
        panel.grid.minor =  element_line(colour = NA, size = 0.5),
        panel.margin =      unit(0.25, "lines"),
        
        strip.background =  element_rect(fill = NA, colour = NA), 
        strip.text.x =      element_text(colour = NA,size = base_size * 0.8),
        strip.text.y =      element_text(colour = NA,size = base_size * 0.8, angle = -90),
        
        plot.background =   element_rect(colour = NA),
        plot.title =        element_text(colour = NA,size = base_size * 1.2),
        plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines")
        
  )

ggsave(filename = "../graphs/lorenz3D.png", p3D, bg = "transparent")
