###############################
## Ggplot - a whirlwind tour ##
###############################

# QAEco coding club
# May 2023
# Alys Young



## Set up ------------------------------------------------------------------------------------------------------------------------------------------

# install.packages('ggplot2')
# install.packages('dplyr')

# Load packages
library(ggplot2) # ggplot2 to make ggplots
library(dplyr) # dplyr for data manipulation

# Load the iris dataset
data(iris)
# look at the dataset
head(iris)



##################################
## The basics and functionality ##
##################################

# Structure of the functions --------------------------------------------------------------
# data argument
# in the aesthetics argument, specify the attributes of the dataset to become parts of the graph
# add a geom_***() function to specify the type of plot

# Box plot of categorical data
ggplot(data = iris, aes(x = Species, y = Sepal.Width)) +
  geom_boxplot()

# Scatter plot for 2 continuous variables
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()


# Keep adding steps to the plot
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  geom_smooth()


# Add attributes of the data
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
  geom_point() +
  geom_smooth()


# Be aware of colour and fill for changing the colour - depends on the plot type
ggplot(data = iris, aes(x = Species, y = Sepal.Width, colour = Species)) +
  geom_boxplot()


# change the colour not related to an attributes of the data
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(colour = "blue") # or hexcode


# many attributes other than colour
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(alpha = 0.5, size  = 2, pch = 17)


# Move the functions into the geom
ggplot() +
  geom_point(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) 


# Move the functions into the geom
iris |>
  ggplot() +
  geom_point(aes(x = Sepal.Length, y = Petal.Width)) 

# Interesting plot types --------------------- 
library(ggbeeswarm) # - simlar effect to violin plots
ggplot(data = iris, aes(x = Species, y = Sepal.Width)) +
  geom_quasirandom()

library(ggboarderline) # - better line plots

library(ggdist) # for distributions and uncertainty

library(ggstream) # stream plot 

library(ggsvg) # use icons in plots
# emoGG - emojis
# ggimage - images



## For mapping:
# ggcountry, ggmap, ggOceanMaps - plotting spatial data
# ggsn - map components to add like north arrow
# sugarbag - hex maps of Aus
# tidyterra, ggspatial - managing spatial data with ggplot
# Annotations -------------------------------------------------------------

## Annotate with text
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  annotate("text", x = 4.5, y = 4, label = "Group 1", size = 7) +
  annotate("text", x = 6.5, y = 3.6, label = "Group 2", color = "blue") + 
  annotate("text", x = 5.5, y = 2, label = "Group 3") 


## Annotate bar plot with * for significance
ggplot(data = iris, aes(x = Species, y = Sepal.Length)) +
  geom_bar(stat = "identity") +
  annotate("text", x = 1, y = 270, label = "*", size = 20) 


library(ggsignif)


## Annotate with text and a line
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  annotate("text", x = 4.5, y = 4.2, label = "Important segment") +
  annotate("segment", x = 4.1, xend = 5, y = 4, yend = 4,
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm")))


## Annotate with an arrow
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  annotate("text", x = 7, y = 4.5, label = "Outliers", size = 7) + # try size = 3
  annotate("segment", x = 7.4, xend = 7.8, y = 4.4, yend = 3.9, # try changing these values
           linewidth = 1.2, # try 0.5 or 2
           color = "darkblue", # try "green" or "purple"
           arrow = arrow())




# esquisse ----------------------------------------------------------------
# install.packages('esquisse')
esquisse:::esquisser()







# Polishing the plot -----------------------------------------------------------------------------------------

## Labels
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  labs(title = "Sepal length by width", x = "Sepal length (cm)", y = "Sepal width (cm)") 







# Themes ------------------------------------------------------------------------------------------------------------------------------------------------------------------
# built in themes from ggplot
# https://ggplot2-book.org/polishing.html
# https://ggplot2.tidyverse.org/reference/ggtheme.html
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  theme_bw()


## more theme options -------*
# package: ggthemes
# https://yutannihilation.github.io/allYourFigureAreBelongToUs/ggthemes/
library(ggthemes)
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  theme_few() # theme_wsj()


# Package: cowplot
library(cowplot)
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + 
  geom_point() +
  theme_cowplot(12)

# Edit your theme with point and click
# package: ggthemeassist

# install.packages("ggthemeassist")
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()


# Note - you can alter these themes


# Set a theme for all plots
theme_set(theme_bw())







# edit your plot with another add in -------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggedit)

ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()



# Or ask ChatGPT



# Facetting ---------------------------------------------------------------
# Seperate plots based on an attribute in the data
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  facet_wrap(~Species, ncol = 1)






# view multiple plots together ------------------------------------------------------------------------------------------------------------------------------------------------------------
# other package options: patchwork

library(gridExtra)

p1 <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()
p2 <- ggplot(data = iris, aes(x = Sepal.Length)) +
  geom_histogram(fill = "white", colour = "black")

grid.arrange(p1, p2, ncol = 2) # use the function grid.arrange to put p1 and p2 into one plot with ncol = 2 


## Other options
# package: ggpubr - https://rpkgs.datanovia.com/ggpubr/
# function: ggpubr::ggarrange()
library(ggpubr)
ggarrange(p2, p1)

# package: cowplot
library(cowplot)
plot_grid(p1, p2, labels = c('A', 'B'), label_size = 12)




## Text labels with ggrepel--------------------------------------------------------------------------------------------
library(ggrepel)

p_text <- ggplot(data = head(iris, n = 20), aes(x = Sepal.Length, y = Sepal.Width, label = Species)) +
  geom_point(colour = "red") +
  geom_text() +
  labs(title = "Text added")

p_repelled <- ggplot(data = head(iris, n = 20), aes(x = Sepal.Length, y = Sepal.Width, label = Species)) +
  geom_point(colour = "red") +
  geom_text_repel() +
  labs(title = "Text with ggrepel")

grid.arrange(p_text, p_repelled, ncol = 2) # use the function grid.arrange to put p1 and p2 into one plot with ncol = 2 





# # Publication ready plots ---------------------------------------------------------------------------------
# library(ggpubr)
# 
# ggdensity(iris,
#           x = "Petal.Length",
#           add = "mean",
#           rug = TRUE,
#           color = "Species",
#           fill = "Species",
#           palette = c("#00AFBB", "#E7B800", "grey"))
# # try gghistogram instead of ggdensity
# 
# ggboxplot(iris,
#           x = "Species",
#           y = "Petal.Length",
#           color = "Species",
#           # palette =c("#00AFBB", "#E7B800", "#FC4E07"),
#           add = "jitter",
#           shape = "Species")





# animated plots------------------------------------------------------------------------------------------------------------------------
library(gganimate)

# good for change through time

# https://gganimate.com









# Interactive plots -------------------------------------------------------------------------------------------------------------------------------------------------------
# package: plotly
library(plotly)

p <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point()

ggplotly(p) # use the function ggplotly wrapped around the plot 








# # Marginal plos -------------------------------------------------------------------------------------------------------------------------------------------------------------
# # package: ggExtra
 library(ggExtra)

ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()

p2 <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
  geom_point()

ggExtra::ggMarginal(
  p = p2,
  groupColour = TRUE,
  groupFill = TRUE,
  type = 'density',
  margins = 'both' #,
  # size = 5,
  # colour = 'black',
  # fill = '#BFBFBF03'
)


install.packages('ggside')
library(ggside)














# Save it out -------------------------------------------------------------
# Package: ggplot2
# function: ggsave


# # Save the most recent plot by using ggsave directly afterwards
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
 geom_point()

ggsave("folder/plot.png")

# # Save a specific plot by assigning it a name
# # Save the most recent plot by using ggsave directly afterwards
p <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
 geom_point()

ggsave(p, "folder/plot.png")


# Package: ggpubr
# function: ggexport







################
## For models ##
################


## Correlation plots ----------------------------------------------------------------------------------------------------------------------------
# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
# function: chart.Correlation()
chart.Correlation(cor_out)





# Package: ggcorrplot 

## Load the package
library(ggcorrplot)


# select only the numeric data to test for correlations

# Option 1 in base R
# using an apply function, test each column to see if its numeric, and then select only those
iris_numeric <- iris[sapply(iris, is.numeric)]

# option 2 using dplyr
iris_numeric <- iris %>% dplyr::select_if(is.numeric)

## Correlation
cor_out <- cor(iris_numeric)

## Plot the correlation using ggcorrplot on cor_out
ggcorrplot(cor_out)


ggcorrplot(cor_out,  
           method = "circle",
           type = "lower")

ggcorrplot(cor_out,  
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)

# install.packages('ggcorr')
# library(ggcorr)
# ggcorr(cor_out)



# Set up a simple model  --------------------------------------------------


# simple linear model
mod1 <- lm(Sepal.Width ~ Sepal.Length + Petal.Length + Petal.Width + Species + Sepal.Length:Species,
           data = iris)
summary(mod1)





## Plot coefficient estimates----------------------------------------------------------------------
summary(mod1)

m_eval <- coef(mod1)
m_eval <- summary(mod1)$coefficients # if you need to extract the p values use summary
m_eval_df <- as.data.frame(m_eval)
m_eval_df$vars <- rownames(m_eval_df)
m_eval_df <- rename(m_eval_df, st_error = `Std. Error`) # removing the space
m_eval_df

library(broom)
tidy(mod1)

ggplot(data = m_eval_df, aes(y = vars, x = Estimate)) +
  geom_point() + 
  geom_pointrange(aes(xmin = Estimate - st_error, xmax = Estimate + st_error)) +
  labs(y = "", x = "Coefficient estimate")

# make vars a factor with the levels in the order you want
# Option 1 using base R
order <- m_eval_df[order(m_eval_df$Estimate),'vars']

# Option 2 using dplyr and pipes
order <- m_eval_df %>%  arrange(Estimate) %>% select(vars) %>% pull

# Set the levels in the correct oder
m_eval_df$vars <- factor(m_eval_df$vars, levels = order)

## Plot with the new order of variables
ggplot(data = m_eval_df, aes(y = vars, x = Estimate)) +
  geom_point() + 
  geom_pointrange(aes(xmin = Estimate - st_error, xmax = Estimate + st_error)) +
  labs(y = "", x = "Coefficient estimate", title = "In order")


## Other optiosn for dot and whisker plots
# # install.packages('jtools')
# library(jtools)
# 
# # install.packages('dotwhisker')
# # library(dotwhisker)
# 
# # install.packages('sjPlot')
# library(sjPlot)
# plot_model(mod1)

## Plotting shape of model covariates -------------------------------------------------------------------------------------------------------------------------------------------

library(effects)
plot(allEffects(m1))

## Extract the effect
eff_PW <- effect("Petal.Width", m1)
eff_PW_df <- as.data.frame(eff_PW)
eff_PW_df

## Plot the effect with ggplot
ggplot(data = eff_PW_df, aes(x = Petal.Width, y = fit)) +
  geom_line()


## Plotting shape of model covariates -------------------------------------------------------------------------------------------------------------------------------------------

## Package: ggeffects 
library(ggeffects)



## One variable ----*
ggpredict(mod1, terms = "Petal.Width") 

marginalEffect_df <- ggpredict(mod1, terms = "Petal.Length") # save the predictions as a dataframe

# Plot 
plot(marginalEffect_df) # plot the effect of the one variable



## Multiple variables ----*
marginalEffect_df <- ggpredict(mod1, terms = c("Petal.Length", "Species", "Sepal.Length"))

# Plot with baseR
plot(marginalEffect_df)














#############
## Colours ##
#############



# # Colour not related to dataset -------------------------------------------
# ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
#   geom_point()
# 
# # Colour by a name and shape
# ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
#   geom_point(color  = "hotpink")
# 
# # Colour by a HEX and size
# ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
#   geom_point(color = "#4842f5", alpha = 0.5, size = 3.5, shape = 17)







# Colour related to your dataset ------------------------------------------


# 2 types of variables that you might want to colour by:
# discrete (categorical, factor)
# continuous (numeric)

# Colour by species - discrete
p_col_discrete <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
  geom_point()

p_col_discrete

# Colour by petal width - continuous
p_col_cont <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Petal.Width)) +
  geom_point()

p_col_cont

# Ways to implement your own colours
# specifying your own colours - individually, or creating your own palette
# using pre-made palettes - e.g. viridis, rcolourbrewer



# Colour blind friendly
# - https://cran.r-project.org/web/packages/colorBlindness/vignettes/colorBlindness.html
# - packages such as: munsell, viridis, RColorBrewer, dichromat, colourblindr, shades, ggsci



## How to choose good colours?
# https://www.learnui.design/tools/data-color-picker.html


###############################
## Part 2.1 - manual colours ##
###############################



# Categorical variables ---------------------------------------------------

p_col_discrete # colour by species with default colours

# Colour by name or HEX
# NOTE: Because our variable is mapped to colour, we use scale_color_manual function

ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  scale_color_manual(values = c("darkgreen", "purple", "#4842f5"))


# Specifiy your colours by name or HEX for the specific categorical level
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  scale_color_manual(values = c('versicolor' = "darkgreen",
                                'virginica' = "purple",
                                'setosa' = "#4842f5"))



# Specify your colours using an external vector
my_pal <- c('versicolor' = "darkgreen", 'virginica' = "purple", 'setosa' = "#4842f5")

# set your vector of colours as the values
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  scale_color_manual(values = my_pal) # make sure you use "values" here









# If you have multiple palettes, save them all to one list each with their own name
project_palettes <- list()

# add the colour pallete for species
project_palettes$species <- c('versicolor' = "darkgreen", 'virginica' = "purple", 'setosa' = "#4842f5")

# use the colour palette from the list as the input for the values argument
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  scale_color_manual(values = project_palettes$species)






# # Continuous variable -----------------------------------------------------
# 
# # Colour by petal width - continuous
# ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length)) +
#   geom_point()
# 
# # Colour by name or HEX - function scale_color_gradient
# ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length)) +
#   geom_point() +
#   scale_color_gradient(low = "blue", high = "red")
# 
# # Use the function colorRampPalette on your colours
# my_pal_reds <- colorRampPalette(c("yellow", "red", "darkred"))
# my_pal_reds(4)
# 
# # Or remember we set up a list of colour palettes earlier
# # project_palettes <- list()
# # project_palettes$species <- c('versicolor' = "darkgreen", 'virginica' = "purple", 'setosa' = "#4842f5")
# project_palettes
# project_palettes$reds <- c("yellow", "red", "darkred")
# project_palettes$greens <- c("yellow", "green", "darkgreen")
# 
# 
# my_pal_reds <- colorRampPalette(project_palettes$reds)
# 
# ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length)) +
#   geom_point() +
#   scale_color_gradientn(colors = my_pal_reds(10))







######################
## Premade palettes ##
######################

# colorBlindness - test if the colours used in a plot are suitable

# # Base r ------------------------------------------------------------------
# ?heat.colors
# ?topo.colors
# topo.colors(10)
# 
# ## Continuous
# ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length)) +
#   geom_point() +
#   scale_color_gradientn(colors = heat.colors(10))# add rev to reverse the colours
# 
# ## Discrete
# ggplot(data = iris, aes(x = Species, y = Sepal.Width, fill = Species)) +
#   geom_boxplot() +
#   scale_fill_manual(values = topo.colors(3)) # make sure the number is more than your categories.
# 







# RColorBrewer ------------------------------------------------------------

library(RColorBrewer)
# Not colour-blind safe by default

# To see the color palettes from RColorBrewer, use display.brewer.all()
display.brewer.all(colorblindFriendly = TRUE)




# For colour use scale_color_*
# For fill use scale_fill_*

# For discrete variables, use scale_*_brewer
# For continuous variables, use scale_*_distiller
# For binned continuous variables, use scale_*_fermenter




# Inside the function use palette to set the palette to use

# Colour on a categorical variable
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  scale_color_brewer(palette = "Set2")

# Fill on a categorical variable
ggplot(data = iris, aes(x = Species, y = Sepal.Width, fill = Species)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Accent")


ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length)) +
  geom_point() +
  scale_color_distiller(palette = "Greens")








# Viridis -----------------------------------------------------------------
library(viridisLite)
library(viridis)
# All are colour-blind friendly

# For colour use scale_color_viridis_*
# For fill use scale_fill_viridis_*

# For a discrete variable, use scale_*_viridis_d
# For a continuous variable, use scale_*_viridis_c
# For a binned continuous variable, use scale_*_viridis_b

# discrete
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  scale_color_viridis_d() # d for discrete

# continuous, plasma palette
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length)) +
  geom_point() +
  scale_color_viridis_c(option = "plasma") # d for discrete

## If using Base R to plot,
# use the functions viridis(n), magma(n), inferno(n) and plasma(n) where n is the number of colours you want. Returns a vector of colours selected from the palette


#imgpalr and paletteR







# Test colour blindness -----------------------------------------------------------------------------------------

library(colorBlindness)

p <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length)) +
  geom_point() +
  scale_color_viridis_c(option = "plasma")

cvdPlot(p)








# Fun colour palettes! -----------------------------------------------------------------------------------------

## ggsci for specific journals
library(ggsci)

## Pirates
library(yarrr)
yarrr::piratepal(palette = "all")

## Studio ghibli
library(ghibli)

## Wes andersen
library(wesanderson)

## Ochre - Australian landscapes
# really nice, earth colours
devtools::install_github("ropenscilabs/ochRe")
library(ochRe)
pal_names <- names(ochre_palettes)

## New Zealand birds
devtools::install_github("G-Thomson/Manu")
library(manu)

## Australian birds
devtools::install_github(repo = "shandiya/feathers", ref = "main")
library(feathers)
names(feathers_palettes)

## Dutch masters - paintings
devtools::install_github("EdwinTh/dutchmasters")
library(dutchmasters)
pal_names <- names(dutchmasters)

## scico - designed for scientific publications
library(scico)

## paletteer - common interface for many palettes
library(paletteer)