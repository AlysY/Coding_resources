## Gradient Boosting Machines
# 





# Advantages of GBMs:
# Often provides predictive accuracy that cannot be beat.
# Lots of flexibility - can optimize on different loss functions and provides several hyperparameter tuning options that make the function fit very flexible.
# No data pre-processing required - often works great with categorical and numerical values as is.
# Handles missing data - imputation not required.
# 
# Disdvantages of GBMs:
# GBMs will continue improving to minimize all errors. This can overemphasize outliers and cause overfitting. Must use cross-validation to neutralize.
# Computationally expensive - GBMs often require many trees (>1000) which can be time and memory exhaustive.
# The high flexibility results in many parameters that interact and influence heavily the behavior of the approach (number of iterations, tree depth, regularization parameters, etc.). This requires a large grid search during tuning.
# Less interpretable although this is easily addressed with various tools (variable importance, partial dependence plots, LIME, etc.).
# 
#
#
# Tuning
# 1. Number of trees: The total number of trees to fit. GBMs often require many trees; however, unlike random forests GBMs can overfit so the goal is to find the optimal number of trees that minimize the loss function of interest with cross validation.
# 2. Depth of trees: The number d of splits in each tree, which controls the complexity of the boosted ensemble. More commonly, d is greater than 1
# 3. Learning rate: Controls how quickly the algorithm proceeds down the gradient descent. Smaller values reduce the chance of overfitting but also increases the time to find the optimal fit. This is also called shrinkage.
# 4. Subsampling: Controls whether or not you use a fraction of the available training observations. Using less than 100% of the training observations means you are implementing stochastic gradient descent. This can help to minimize overfitting and keep from getting stuck in a local minimum or plateau of the loss function gradient.
# 
# 
# Implementation:
# gbm: The original R implementation of GBMs
# xgboost: A fast and efficient gradient boosting framework (C++ backend).
# h2o: A powerful java-based interface that provides parallel distributed algorithms and efficient productionalization.
# Check the two links provided in the tute 
#
#
# GBM
# gbm::gbm uses the formula interface to specify your model whereas
# gbm::gbm.fit requires the separated x and y matrices.
# When working with many variables it is more efficient to use the matrix rather than formula interface.
#
#
# Default settings in gbm:
# 1. learning rate ( shrinkage ) of 0.001.
#   This is a very small learning rate and typically requires a large number of trees to find the minimum MSE.
# 2. Number of trees of 100
#   Rarely sufficient. Consequently, I crank it up to 10,000 trees
# 3. depth of each tree ( interaction.depth ) is 1

# 0.  Set up --------------------------------------------------------------

library(rsample)      # data splitting 
library(gbm)          # basic implementation
library(xgboost)      # a faster implementation of gbm
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # a java-based platform
library(pdp)          # model visualization
library(ggplot2)      # model visualization
library(lime)         # model visualization



# 1. Data -----------------------------------------------------------------

# Ames Housing data that has been included in the AmesHousing package.
library(AmesHousing)

# Create training (70%) and test (30%) sets for the AmesHousing::make_ames() data.
# Use set.seed for reproducibility

set.seed(123)
ames_split <- initial_split(AmesHousing::make_ames(), prop = .7)
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)

# tree-based methods tend to perform well on unprocessed data (i.e. without normalizing, centering, scaling features).


# 2. Model  --------------------------------------------------------------------

# for reproducibility
set.seed(123)
# train GBM model
gbm.fit <- gbm(
  formula = Sale_Price ~ .,
  distribution = "gaussian",
  data = ames_train,
  n.trees = 10000,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)
# print results
print(gbm.fit)
## gbm(formula = Sale_Price ~ ., distribution = "gaussian", data = ames_train,
## n.trees = 10000, interaction.depth = 1, shrinkage = 0.001,
## cv.folds = 5, verbose = FALSE, n.cores = NULL)
## A gradient boosted model with gaussian loss function.
## 10000 iterations were performed.
## The best cross-validation iteration was 10000.
## There were 80 predictors of which 45 had non-zero influence.

# interpretation:
# the minimum CV RMSE is 29133 (this means on average our model is about $29,133 off from the actual sales price


# get MSE and compute RMSE
sqrt(min(gbm.fit$cv.error))
## [1] 29133.33
# plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm.fit, method = "cv")
## [1] 10000

# interpretation:
# the plot also illustrates that the CV error is still decreasing at 10,000 trees.
# The small learning rate is resulting in very small incremental improvements. Therefore many trees are required. For the default learning rate and tree depth, it takes 39,906 trees for the CV error to minimize (~ 5 minutes of run time)!




# 3.  Tuning manualy --------------------------------------------------------------
# Tuning parameters:
# increase the learning rate to take larger steps down the gradient descent
# reduce the number of trees since we are reducing the learning rate
# increase the depth of each tree from using a single split to 3 splits.


# for reproducibility
set.seed(123)
# train GBM model
gbm.fit2 <- gbm(
  formula = Sale_Price ~ .,
  distribution = "gaussian",
  data = ames_train,
  n.trees = 5000,
  interaction.depth = 3,
  shrinkage = 0.1,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)
# find index for n trees with minimum CV error
min_MSE <- which.min(gbm.fit2$cv.error)
# get MSE and compute RMSE
sqrt(gbm.fit2$cv.error[min_MSE])
## [1] 23112.1
# plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm.fit2, method = "cv")
## [1] 1260


# This model takes about 90 seconds to run and achieves a significantly lower RMSE than our initial model with only 1,260 trees.



# 4. Tuning systematically ------------------------------------------------

# Grid search:
# iterate over every combination of hyperparameter values to interrogate performance

# We’re going to search across 81 models with varying:
# learning rates
# tree depth
# Minimum number of observations allowed in the trees terminal nodes ( n.minobsinnode )
# and introduce stochastic gradient descent by allowing bag.fraction < 1.

# Other parameters:
# use 5,000 trees
# to speed up the tuning process, instead of performing 5-fold CV I train on 75% of the training observations and evaluate performance on the remaining 25%
  # Note, the function uses the first 25% of the data. so ensure it is randomised

# create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.65, .8, 1),
  optimal_trees = 0, # a place to dump results
  min_RMSE = 0 # a place to dump results
)
# total number of combinations
nrow(hyper_grid)
## [1] 81


## Takes 30 mins ---


# randomize data
random_index <- sample(1:nrow(ames_train), nrow(ames_train))
random_ames_train <- ames_train[random_index, ]

# grid search
for(i in 1:nrow(hyper_grid)) {
  # reproducibility
  set.seed(123)
  # train model
  gbm.tune <- gbm(formula = Sale_Price ~ .,
                  distribution = "gaussian",
                  data = random_ames_train,
                  n.trees = 5000,
                  interaction.depth = hyper_grid$interaction.depth[i],
                  shrinkage = hyper_grid$shrinkage[i],
                  n.minobsinnode = hyper_grid$n.minobsinnode[i],
                  bag.fraction = hyper_grid$bag.fraction[i],
                  train.fraction = .75,
                  n.cores = NULL, # will use all cores by default
                  verbose = FALSE
  )
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

hyper_grid %>%
  dplyr::arrange(min_RMSE) %>%
  head(10)
## shrinkage interaction.depth n.minobsinnode bag.fraction optimal_trees min_RMSE
## 1 0.01 5 5 0.65 3867 16647.87
## 2 0.01 5 5 0.80 4209 16960.78
## 3 0.01 5 5 1.00 4281 17084.29
## 4 0.10 3 10 0.80 489 17093.77
## 5 0.01 3 5 0.80 4777 17121.26
## 6 0.01 3 10 0.80 4919 17139.59
## 7 0.01 3 5 0.65 4997 17139.88
## 8 0.01 5 10 0.80 4123 17162.60
## 9 0.01 5 10 0.65 4850 17247.72
## 10 0.01 3 10 1.00 4794 17353.36

# Interpretation: 
# Firstly, our top model has better performance with the RMSE nearly $3,000 lower
# Second, looking at the top 10 models we see that:
# 1. none of the top models used a learning rate of 0.3; small incremental steps down the gradient descent appears to work best,
# 2. none of the top models used stumps ( interaction.depth = 1 ); there are likely stome important interactions that the deeper trees are able to capture,
# 3. adding a stochastic component with bag.fraction < 1 seems to help;  there may be some local minimas in our loss function gradient,
# 4. none of the top models used n.minobsinnode = 15; the smaller nodes may allow us to capture pockets of unique feature-price point instances,
# 5. in a few instances we appear to use nearly all 5,000 trees; maybe we should increase this parameter in our next search?

# This helps us zoom into the parameters that are likely best
# Adjust our grid and zoom into closer regions of the values that produce the best results

# This grid contains 81 combinations that we’ll search across.




# modify hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.01, .05, .1),
  interaction.depth = c(3, 5, 7),
  n.minobsinnode = c(5, 7, 10),
  bag.fraction = c(.65, .8, 1),
  optimal_trees = 0, # a place to dump results
  min_RMSE = 0 # a place to dump results
)
# total number of combinations
nrow(hyper_grid)
## [1] 81



# Loop through the grid search
for(i in 1:nrow(hyper_grid)) {
  # reproducibility
  set.seed(123)
  # train model
  gbm.tune <- gbm(
    formula = Sale_Price ~ .,
    distribution = "gaussian",
    data = random_ames_train,
    n.trees = 6000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}
hyper_grid %>%
  dplyr::arrange(min_RMSE) %>%
  head(10)
## n.trees shrinkage interaction.depth n.minobsinnode bag.fraction optimal_trees min_RMSE
## 1 6000 0.10 5 5 0.65 483 20407.76
## 2 6000 0.01 5 7 0.65 4999 20598.62
## 3 6000 0.01 5 5 0.65 4644 20608.75
## 4 6000 0.05 5 7 0.80 1420 20614.77
## 5 6000 0.01 7 7 0.65 4977 20762.26
## 6 6000 0.10 3 10 0.80 1076 20822.23
## 7 6000 0.01 7 10 0.80 4995 20830.03
## 8 6000 0.01 7 5 0.80 4636 20830.18
## 9 6000 0.10 3 7 0.80 949 20839.92
## 10 6000 0.01 5 10 0.65 4980 20840.43


# Interpretation:
# our best model is the same as the best model above with an RMSE just above $20K.



## Best model
# Train a model with the parameters identified earlier parameters
# The model converged at 483 trees I train a cross validated model with 1000 trees.

# for reproducibility
set.seed(123)
# train GBM model
gbm.fit.final <- gbm(
  formula = Sale_Price ~ .,
  distribution = "gaussian",
  data = ames_train,
  n.trees = 483,
  interaction.depth = 5,
  shrinkage = 0.1,
  n.minobsinnode = 5,
  bag.fraction = .65,
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)

# best model interpretation
# Cross validated error of ~$22K



# 5. visualisation --------------------------------------------------------


## 5.1 Variable importance --------------------------------------------------------------------------------------------
# understand the variables that have the largest influence on sale price

# *cBars* allows you to adjust the number of variables to show (in order of influence).

par(mar = c(5, 8, 1, 1))
summary(
  gbm.fit.final,
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)
## var rel.inf
## Overall_Qual Overall_Qual 4.084734e+01
## Gr_Liv_Area Gr_Liv_Area 1.323956e+01
## Neighborhood Neighborhood 1.100911e+01
## Total_Bsmt_SF Total_Bsmt_SF 5.513300e+00
## Bsmt_Qual Bsmt_Qual 5.149919e+00
## First_Flr_SF First_Flr_SF 3.884696e+00
## Garage_Cars Garage_Cars 2.354694e+00
## Full_Bath Full_Bath 1.953775e+00
## MS_SubClass MS_SubClass 1.169509e+00
## Kitchen_Qual Kitchen_Qual 1.137581e+00
## ...truncated...



## 5.2 VIP --------------------------------------------------------------------------------------------
# devtools::install_github("koalaverse/vip")
vip::vip(gbm.fit.final)


## 5.3 Partial dependence plots ------------------------------------------------------------------------------------------------------------------------------------------
# understand how the response variable changes based on these variables
# use partial dependence plots (PDPs) and individual conditional expectation (ICE) curves

# PDP plot for the Gr_Liv_Area variable
# displays the average change in predicted sales price as we vary Gr_Liv_Area while holding all other variables constant
# This PDP illustrates how the predicted sales price increases as the square footage of the ground floor in a house increases
gbm.fit.final %>%
  #partial(pred.var = "Gr_Liv_Area", n.trees = gbm.fit.final$n.trees, grid.resolution) # CHECK THIS
  autoplot(rug = TRUE, train = ames_train) +
  scale_y_continuous(labels = scales::dollar)

## 5.4 individual conditional expectation (ICE) --------------------------------------------------------------------------------------------
# plot the change in the predicted response variable for each observation as we vary each predictor variable
# Layout:
# regular ICE curve plot (left) and the centered ICE curves (right)


# ice1 <- gbm.fit.final %>%
#   partial(
#     pred.var = "Gr_Liv_Area",
#     n.trees = gbm.fit.final$n.trees,
#     grid.resolution = 100,
#     ice = TRUE
#   ) %>%
#   autoplot(rug = TRUE, train = ames_train, alpha = .1) +
#   ggtitle("Non-centered") +
#   scale_y_continuous(labels = scales::dollar)
# ice2 <- gbm.fit.final %>%
#   partial(
#     pred.var = "Gr_Liv_Area",
#     n.trees = gbm.fit.final$n.trees,
#     grid.resolution = 100,
#     ice = TRUE
#   ) %>%
#   autoplot(rug = TRUE, train = ames_train, alpha = .1, center = TRUE) +
#   ggtitle("Centered") +
#   scale_y_continuous(labels = scales::dollar)
# gridExtra::grid.arrange(ice1, ice2, nrow = 1)

# # Interpretation:
# When the curves have a wide range of intercepts and are consequently “stacked” on
# each other, heterogeneity in the response variable values due to marginal
# changes in the predictor variable of interest can be difficult to discern. The
# centered ICE can help draw these inferences out and can highlight any strong
# heterogeneity in our results. The resuts show that most observations follow a
# common trend as Gr_Liv_Area increases; however, the centered ICE plot
# highlights a few observations that deviate from the common trend.


# 5.5 LIME -----------------------------------------------------------------
# understanding why a prediction resulted in a given value for a single observation.

# To use the lime package on a gbm model we need to define model type and prediction methods.
model_type.gbm <- function(x, ...) {
  return("regression")
}
predict_model.gbm <- function(x, newdata, ...) {
  pred <- predict(x, newdata, n.trees = x$n.trees)
  return(as.data.frame(pred))
}



# We can now apply to our two observations
# get a few observations to perform local interpretation on
local_obs <- ames_test[1:2, ]
# apply LIME
explainer <- lime(ames_train, gbm.fit.final)
explanation <- explain(local_obs, explainer, n_features = 5)
plot_features(explanation)

# Interpretation:
# the predicted value (Case 1: $118K, Case 2: $161K),
# local model fit (both are relatively poor), 
# and the most influential variables driving the predicted value for each observation.



# 6. Predicition ----------------------------------------------------------

# 6.1 Predict -------------------------------------------------------------
# Predict function;
# however, we also need to supply the number of trees to use (see ?predict.gbm for details).

# predict values for test data
pred <- predict(gbm.fit.final, n.trees = gbm.fit.final$n.trees, ames_test ) %>%  ## CHECK HERE
                # results
                caret::RMSE(pred, ames_test$Sale_Price)
                ## [1] 20681.88
                
                
# Interpretation:
# The RMSE for our test set is very close to the RMSE we obtained on our best gbm model



# 6.2 Extreme Gradient Boosting -------------------------------------------
# package xgboost
# Requires numerical variables
  # therefore need to encode variables. Methods: vtreat, Matrix::sparse.model.matrix , caret::dummyVars

## Vtreat the variables
# variable names
features <- setdiff(names(ames_train), "Sale_Price")
# Create the treatment plan from the training data
treatplan <- vtreat::designTreatmentsZ(ames_train, features, verbose = FALSE) # CHECK HERE
# Get the "clean" variable names from the scoreFrame
new_vars <- treatplan %>%
  magrittr::use_series(scoreFrame) %>%
  dplyr::filter(code %in% c("clean", "lev")) %>%
  magrittr::use_series(varName)
# Prepare the training data
features_train <- vtreat::prepare(treatplan, ames_train, varRestriction =) # CHECK HERE
response_train <- ames_train$Sale_Price
# Prepare the test data
features_test <- vtreat::prepare(treatplan, ames_test, varRestriction = new_vars) # CHECK HERE
response_test <- ames_test$Sale_Price
# dimensions of one-hot encoded data
dim(features_train)
## [1] 2051 208
dim(features_test)
## [1] 879 208



## xgboost parameters for the xgb.cv function
# learning rate ( eta ): 0.3
# tree depth ( max_depth ): 6
# minimum node size ( min_child_weight ): 1
# percent of training data to sample for each tree ( subsample –> equivalent o gbm ’s bag.fraction ): 100%

# reproducibility
set.seed(123)
xgb.fit1 <- xgb.cv(
  data = features_train,
  label = response_train,
  nrounds = 1000,
  nfold = 5,
  objective = "reg:linear", # for regression models
  verbose = 0 # silent,
)


# ## Model evaluation
# xgb.fit1$evaluation_log
#   to identify the minimum RMSE and the optimal number of trees for both the training data and the cross-validated error.

# get number of trees that minimize error
xgb.fit1$evaluation_log %>%
  dplyr::summarise(
    ntrees.train = which(train_rmse_mean == min(train_rmse_mean))[1],
    rmse.train = min(train_rmse_mean),
    ntrees.test = which(test_rmse_mean == min(test_rmse_mean))[1],
    rmse.test = min(test_rmse_mean),
  )
## ntrees.train rmse.train ntrees.test rmse.test
## 1 965 0.5022836 60 27572.31
# plot error vs number trees
ggplot(xgb.fit1$evaluation_log) +
  geom_line(aes(iter, train_rmse_mean), color = "red") +
  geom_line(aes(iter, test_rmse_mean), color = "blue")


# # Interpretation:
# We can see that the training error continues to decrease to 965 trees where the
# RMSE nearly reaches zero; however, the cross validated error reaches a minimum
# RMSE of $27,572 with only 60 trees



# xgb.cv feature of early stopping
# stop if we see no improvement for 10 consecutive trees
# speeds up processing

# reproducibility
set.seed(123)
xgb.fit2 <- xgb.cv(
  data = features_train,
  label = response_train,
  nrounds = 1000,
  nfold = 5,
  objective = "reg:linear", # for regression models
  verbose = 0, # silent,
  early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
)
# plot error vs number trees
ggplot(xgb.fit2$evaluation_log) +
  geom_line(aes(iter, train_rmse_mean), color = "red") +
  geom_line(aes(iter, test_rmse_mean), color = "blue")



## Turning xgboost
# pass parameters as a list object to the params argument
# 
# common parameters include:
# eta :controls the learning rate
# max_depth : tree depth
# min_child_weight : minimum number of observations required in each
# terminal node
# subsample : percent of training data to sample for each tree
# colsample_bytrees : percent of columns to sample from for each tree


# create parameter list
params <- list(
  eta = .1,
  max_depth = 5,
  min_child_weight = 2,
  subsample = .8,
  colsample_bytree = .9
)
# reproducibility
set.seed(123)
# train model
xgb.fit3 <- xgb.cv(
  params = params,
  data = features_train,
  label = response_train,
  nrounds = 1000,
  nfold = 5,
  objective = "reg:linear", # for regression models
  verbose = 0, # silent,
  early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
)
# assess results
xgb.fit3$evaluation_log %>%
  dplyr::summarise(
    ntrees.train = which(train_rmse_mean == min(train_rmse_mean))[1],
    rmse.train = min(train_rmse_mean),
    ntrees.test = which(test_rmse_mean == min(test_rmse_mean))[1],
    rmse.test = min(test_rmse_mean) # CHECK HERE - in the tutorial there was an extra commar here
  )
## ntrees.train rmse.train ntrees.test rmse.test
## 1 180 5891.703 170 24650.17



## Grid search for hyper parameters to loop through
# create hyperparameter grid
hyper_grid <- expand.grid(
  eta = c(.01, .05, .1, .3),
  max_depth = c(1, 3, 5, 7),
  min_child_weight = c(1, 3, 5, 7),
  subsample = c(.65, .8, 1),
  colsample_bytree = c(.8, .9, 1),
  optimal_trees = 0, # a place to dump results
  min_RMSE = 0 # a place to dump results
)
nrow(hyper_grid)
## [1] 576



## NOTE: This code takes 6 hours
for(i in 1:nrow(hyper_grid)) {
  # create parameter list
  params <- list(
    eta = hyper_grid$eta[i],
    max_depth = hyper_grid$max_depth[i],
    min_child_weight = hyper_grid$min_child_weight[i],
    subsample = hyper_grid$subsample[i],
    colsample_bytree = hyper_grid$colsample_bytree[i]
  )
  # reproducibility
  set.seed(123)
  # train model
  xgb.tune <- xgb.cv(
    params = params,
    data = features_train,
    label = response_train,
    nrounds = 5000,
    nfold = 5,
    objective = "reg:linear", # for regression models
    verbose = 0, # silent,
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
  )
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean
                                           hyper_grid$min_RMSE[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
}
hyper_grid %>%
  dplyr::arrange(min_RMSE) %>%
  head(10)
## eta max_depth min_child_weight subsample colsample_bytree optimal_trees min_RMSE
## 1 0.01 5 5 0.65 1 1576 23548.84
## 2 0.01 5 3 0.80 1 1626 23587.16
## 3 0.01 5 3 0.65 1 1451 23602.96
## 4 0.01 5 1 0.65 1 1480 23608.65
## 5 0.05 5 3 0.65 1 305 23743.54
## 6 0.01 5 1 0.80 1 1851 23772.90
## 7 0.05 3 3 0.65 1 552 23783.55
## 8 0.01 7 5 0.65 1 1248 23792.65
## 9 0.01 3 3 0.80 1 1923 23794.78
## 10 0.01 7 1 0.65 1

## CHECK HERE - for a link in the tute to how to tune parameters


# Once you’ve found the best model (globally optimal model), we can fit our final model with xgb.train .

# fit our final model with xgb.train

# parameter list
params <- list(
  eta = 0.01,
  max_depth = 5,
  min_child_weight = 5,
  subsample = 0.65,
  colsample_bytree = 1
)
# train final model
xgb.fit.final <- xgboost(
  params = params,
  data = features_train,
  label = response_train,
  nrounds = 1576,
  objective = "reg:linear",
  verbose = 0
)





# 6. Visualizing -------------------------------------------------------------


# 6.1 Variable importance --------------------------------------------------------------------------------------------------------------
# create the importance matrix with xgb.importance and then feed this matrix into xgb.plot.importance

# 3 variable importance measures:











          
# Playing ------------------------------------------------------------------------------------------------------------------------------------------

x <- c(seq(0, 100, 1), seq(3.5, 90, 0.7))
e <- rnorm(n = x, mean = 0, sd = 2)
y = 0.9*x + e
plot(x, y)




