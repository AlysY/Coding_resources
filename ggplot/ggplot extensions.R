## GGPLOT2 extensions
# By chatGPT


# "Write an R script for a tutorial on packages which extend upon ggplot2"
#Load ggplot2 package
library(ggplot2)

# Load extension packages
library(ggthemes)
library(ggrepel)
library(ggbeeswarm)
library(ggExtra)

# Create data for plot
set.seed(123)
data <- data.frame(x = rnorm(100), y = rnorm(100), group = sample(c("A","B","C"), 100, replace = TRUE))

# Basic ggplot2 plot
ggplot(data, aes(x = x, y = y)) + geom_point()

# Themes
ggplot(data, aes(x = x, y = y)) + 
  geom_point() + 
  theme_tufte()

# Repel Overlapping Text
ggplot(data, aes(x = x, y = y, label = group)) + 
  geom_point() + 
  geom_text_repel()

# Bee Swarm Plot
ggplot(data, aes(x = group, y = y, fill = group)) + 
  geom_beeswarm()

# Add marginal histograms
ggplot(data, aes(x = x, y = y)) + 
  geom_point() + 
  ggMarginal(type = "histogram", size = 3, color = "black")




# gganimate ---------------------------------------------------------------
# "Write an example R script of how to use the packages gganimate"


# Load packages
library(ggplot2)
library(gganimate)

# Create data for plot
set.seed(123)
data <- data.frame(x = rnorm(100), y = rnorm(100), time = rep(1:10, each = 10))

# Create plot
p <- ggplot(data, aes(x, y)) + 
  geom_point(aes(frame = time)) + 
  transition_time(time) + 
  ease_aes("linear")

#Animate the plot 
gganimate(p, interval = 0.5)
