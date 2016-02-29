# Load the required library
library(deSolve) # Solvers for Differential Equations
library(ggplot2) # Plot results
library(grid) # For use of the unit function

# Create function to calculate Lotka Volterra equaiton results
LotkaVolterra <- function (t, state, pars) {
  with(as.list(c(state, pars)), {
    dx = x*(alpha - beta*y)
    dy = -y*(gamma - delta*x)
    return(list(c(dx, dy)))
  })
}

# Input parameter for the calculation
# α, β, γ, δ are positive real parameters describing the 
# interaction of the two species the formula represent the
# growth rates of the two populations over time

pars <- c(alpha = 2, beta = .5, gamma = .2, delta = .6)
state <- c(x = 10, y = 10) # initial population parameters
t <- seq(0, 100, by = 1) # Time

# Perform the integration and assign it to variable 'out'
out <- as.data.frame(ode(func = LotkaVolterra, y = state, parms = pars, times = t))

# Plot result
p <- ggplot(out, aes(x = time)) + 
  geom_line(aes(y = y, colour = "Wolf"), linetype = "dashed") + # Wolf
  geom_line(aes(y = x, colour = "Sheep")) + # Sheep
  scale_y_continuous(name = "Population") +
  scale_x_continuous(name = "Time step") +
  theme(legend.position=c(1,1),legend.justification=c(1,1),
        legend.direction = "vertical",
        legend.box = "horizontal",
        legend.box.just = c("top"), 
        legend.background = element_rect(fill="transparent"))+
  scale_colour_manual(name = "Animal",
                      values = c(Wolf = "red", Sheep = "black"))
  

ggsave(filename = "../graphs/wolf-sheep.png", p, bg = "transparent")
