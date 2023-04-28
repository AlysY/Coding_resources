## Random Forest model
# Using the iris data set

# https://www.blopig.com/blog/2017/04/a-very-basic-introduction-to-random-forests-using-r/


# Set random seed to make results reproducible:
set.seed(17)

# Calculate the size of each of the data sets:
data_set_size <- floor(nrow(iris)/2)
# rule of thumb: e^Nf < No (Nf = number of features, No = number of observations) to minimise overfitting
# here it is 75. 
exp(5) # 148.4132
exp(4) # 54.59815
# as  exp(4)  or e^4 is smaller than 75, try using 4 as the maximum no of features

# Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(iris), size = data_set_size)

# Assign the data to the correct sets
training <- iris[indexes,]
validation1 <- iris[-indexes,]

#import the package
install.packages("randomForest")
library(randomForest)

## Perform training:
rf_classifier = randomForest(Species ~ ., # predicting which species using the remaining columns
                             data = training,  # use the training dataset
                             ntree = 100, # test a range of values(i.e. 100,200,300,400,500) and choose the one that minimises the OOB estimate of error rate.
                             mtry = 2, 
                             importance = TRUE) # calculate variable importance.

## View the output
rf_classifier
# outputs a confusion matrix
# OOB estimate of error rate is a useful measure to discriminate between different random forest classifiers
# to reduce the OOB estimate of error rate:
  # vary the number of trees
  # vary the number of variables
# Use randomForest::rfcv to do this cross validation


# Variable importance
  # Look at the rank rather than the value
varImpPlot(rf_classifier)
# MeanDecreaseAccuracy estimate in loss of predictive ability without that variable
# MeanDecreaseGini how important that feature is to split the data correctly.


## Predict
prediction_for_table <- predict(rf_classifier, validation1[, -5]) # remove the species column
table(observed = validation1[,5], # known species 
      predicted = prediction_for_table) # predicted species


## validate with ROC and AUC
install.packages("ROCR")
library(ROCR)

# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes
prediction_for_roc_curve <- predict(rf_classifier, validation1[,-5],
                                    type = "prob")

# Use pretty colours:
pretty_colours <- c("#F8766D","#00BA38","#619CFF")
# Specify the different classes 
classes <- levels(validation1$Species)


# For each class
for (i in 1:length(classes)) {
  
  # Define which observations belong to class[i]
  true_values <- ifelse(validation1[,5]==classes[i],1,0)
  
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  
  if (i==1) {
    plot(perf,main="ROC Curve",col=pretty_colours[i]) 
  } else {
    plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
  }
  
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  
  print(auc.perf@y.values)
}
