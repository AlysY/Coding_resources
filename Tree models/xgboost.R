## Set up
install.packages('xgboost')     # for fitting the xgboost model
# install.packages('caret')       # for general data preparation and model fitting
install.packages('e1071') # Functions for latent class analysis, short time Fourier transform, fuzzy clustering, support vector machines, shortest path computation, bagged clustering, naive Bayes classifier, generalized k-nearest neighbour ...

library(xgboost)
library(caret)  
library(e1071)

data <- iris               # reads the dataset

head(data)           # head() returns the top 6 rows of the dataframe

summary(data)       # returns the statistical summary of the data columns

dim(data)


## Test and trainging data
# createDataPartition() function from the caret package to split the original dataset into a training and testing set and split data into training (80%) and testing set (20%)
parts = createDataPartition(data$Species, p = 0.7, list = F)
train = data[parts, ]
test = data[-parts, ]

X_train = data.matrix(train[,-5])                  # independent variables for train
y_train = train[,5]                                # dependent variables for train

X_test = data.matrix(test[,-5])                    # independent variables for test
y_test = test[,5]                                   # dependent variables for test

# convert the train and test data into xgboost matrix type.
xgboost_train = xgb.DMatrix(data = X_train, label = y_train)
xgboost_test  = xgb.DMatrix(data = X_test, label = y_test)


## The model
# train a model using our training data
model <- xgboost(data = xgboost_train,                    # the data   
                 max.depth = 3, ,                         # max depth 
                 nrounds = 50)                            # max number of boosting iterations

summary(model)

## Predict
#use model to make predictions on test data
pred_test = predict(model, xgboost_test)

pred_test

pred_test[(pred_test > 3)] = 3
pred_y = as.factor((levels(y_test))[round(pred_test)])
print(pred_y)

## Confusion matrix
conf_mat = confusionMatrix(y_test, pred_y)
print(conf_mat)
