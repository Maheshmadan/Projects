#setting the working directory
getwd()
setwd("C:\\Users\\Dell\\Desktop\\balsingh project (1)\\Classification Project")
library(readxl)


#importing excel file
df1 <- read_excel("data.xlsx")
dim(df1)
table(df1$Classes)
#View(df1)


#checking the null values across all the columns of df1 and df2
for(i in colnames(df1)){
  print(sum(is.na(df1[,i])))
}
str(df1)


#plotting
library(ggplot2)
library(GGally)
# Use a colorful palette
my_palette <- c("#FF5733", "#FFBD33", "#33FF57", "#337BFF", "#E033FF")


# Creating pair plot
# Convert 'Classes' column to factor
df1$Classes <- as.factor(df1$Classes)
# Create pair plot with custom color palette
pair_plot <- ggpairs(df1, aes(colour = Classes)) +
  theme_bw() +
  scale_color_manual(values = my_palette)
pair_plot


# Create the training and testing sets using createDataPartition()
train_proportion <- 0.8
set.seed(300)
num_rows <- train_proportion*dim(df1)[1]
random_indices <- sample(nrow(df1),num_rows)
train_dataset <- df1[random_indices,]
train_dataset
test_dataset <- df1[-random_indices,]
test_dataset_X <- test_dataset[,-11]
test_dataset_Y <- test_dataset[,11]
dim(train_dataset)
dim(test_dataset)
test_dataset_X
test_dataset_Y




## Logistic regression ------------------------

#install.packages('Metrics')
library(Metrics)
# Fit logistic regression model with weights
model1 <- glm(Classes ~ ., data =train_dataset, family = binomial)

# Print the model summary
summary(model1)

# Check for multicollinearity
library(car)
vif(model1)

## feature selection
library(MASS)
# Perform likelihood-based feature selection using stepAIC
model1 <- stepAIC(model1, direction = "both")
model1
summary(model1)
vif(model1)


## test data accuracy --------------
predictions <- predict(model1,newdata = test_dataset_X,type="response")
predictions
binary_predictions <- ifelse(predictions >= 0.5, 1, 0)
binary_predictions
# Create the confusion matrix
table1<- table(binary_predictions,test_dataset_Y$Classes)
accuracy_value11 <- sum(diag(table1)) / sum(table1)
accuracy_value11


## training data accuracy-----------------
train_predictions <- predict(model1,newdata = train_dataset[,-11])
binary_predictions_train <- ifelse(train_predictions >= 0.5, 1, 0)
binary_predictions_train
# Create the confusion matrix
table<- table(binary_predictions_train,train_dataset$Classes)
# Calculate evaluation metrics based on the confusion matrix
train12 <- sum(diag(table)) / sum(table)
train12
df=data.frame(Training_Accuracy=train12,Test_Accuracy=accuracy_value11)
df

### PRECISION MEASURE , RECALL MEASURE
library(caret)
library(MLmetrics)
# Create a confusion matrix
cm <- confusionMatrix(factor(binary_predictions), factor(test_dataset_Y$Classes))
cm

# Calculate precision, recall, and F1-score
precision <- Precision(binary_predictions,test_dataset_Y$Classes)
precision
recall <- Recall(binary_predictions,test_dataset_Y$Classes)
recall
f1_score <- F1_Score(binary_predictions,test_dataset_Y$Classes)
f1_score
df=data.frame(precision,recall,f1_score)
df




## FITIING SVM MODEL-------------

library(e1071)
svm_model <- svm(Classes ~ ., data = train_dataset, kernel = 'radial', cost = 0.01, gamma = 0.1, type = "C-classification", scale = TRUE)

svm_model
summary(svm_model)
predictions <- predict(svm_model,newdata = test_dataset_X)
predictions
# Calculate hinge loss
hinge_loss <- sum(ifelse(test_dataset_Y$Classes!= predictions, 1, 0)) / nrow(test_dataset_X)

# Print the hinge loss
print(paste("Hinge Loss:", hinge_loss))
# Create the confusion matrix
conf_matrix <- table(predictions,test_dataset_Y$Classes)
# Calculate evaluation metrics based on the confusion matrix
accuracy_valu <- sum(diag(conf_matrix)) / sum(conf_matrix)
# Print the confusion matrix and evaluation metrics
print("Confusion Matrix:")
print(conf_matrix)
print(paste("test data Accuracy:", accuracy_valu))

## train data accuracy
train_predictions <- predict(svm_model,newdata = train_dataset[,-11],type="class")
train_accuracy<- table(train_predictions, train_dataset$Classes)
trai <- sum(diag(train_accuracy)) / sum(train_accuracy)
print(paste("train dataset accuracy",train22))
df=data.frame(Training_Accuracy=trai,Test_Accuracy=accuracy_value21)
df

# Calculate hinge loss
hinge_loss <- sum(ifelse(train_dataset$Classes!= train_predictions, 1, 0)) / nrow(train_dataset[,-11])

# Print the hinge loss
print(paste("Hinge Loss:", hinge_loss))
## tunning of tthe parameter
library(e1071)
# Perform the tuning
train_dataset$Classes <- factor(train_dataset$Classes)
tune.out <- tune(svm, Classes ~ ., data = train_dataset, type = "C-classification", kernel = "radial", scale = TRUE, ranges = list(cost = c(0.1, 1, 10, 100), gamma = c(0.1, 1, 10, 100)))

# Summarize the tuning results
summary(tune.out)

# Get the best model
best_model <- tune.out$best.model
summary(best_model)

# Extract detailed performance results
perf_results <-tune.out$performances
perf_results
# Predict classes on the test dataset
predictions=predict(tune.out$best.model,test_dataset_X)
predictions
## test data accuracy
train_accuracy<- table(predictions,test_dataset_Y$Classes)
accuracy_value00 <- sum(diag(train_accuracy)) / sum(train_accuracy)
print(paste("test dataset accuracy : ",accuracy_value00))

## train data accuracy
train_predictions <- predict(tune.out$best.model,,newdata = train_dataset[,-11])
train_accuracy<- table(train_predictions,train_dataset$Classes)
train22 <- sum(diag(train_accuracy)) / sum(train_accuracy)
print(paste("train dataset accuracy : ",accuracy_value1))
print("confusion matrix for train")
print(train_accuracy)
df=data.frame(Training_Accuracy=train22,Test_Accuracy=accuracy_value00)
df

# Create a confusion matrix
library("caret")
library(MLmetrics)
conf_matrix <- confusionMatrix(predictions,as.factor(test_dataset_Y$Classes))
# Print confusion matrix
print(conf_matrix)

# Calculate precision, recall, and F1-score
precision <- Precision(predictions,test_dataset_Y$Classes)
precision
recall <- Recall(predictions,test_dataset_Y$Classes)
recall
f1_score <- F1_Score(predictions,test_dataset_Y$Classes)
f1_score
df=data.frame(precision,recall,f1_score)
df





## decision trees-----------------------------------
library(tree)
library(rpart)

# Convert Classes to a factor if it's not already
train_dataset$Classes <- as.factor(train_dataset$Classes)
fit2 <- tree(factor(Classes) ~ ., data = train_dataset)
fit2
summary(fit2)

# Plot the tree
plot(fit2)
text(fit2, pretty = 0)

tree.pred <- predict(fit2,test_dataset_X,type = "class")
tree.pred

library(ROCR)
# Create a prediction object for the ROCR package
pred <- prediction(tree.pred, test)

# Calculate performance measures, including ROC curve and AUC
perf <- performance(pred, "tpr", "fpr")
roc_obj <- performance(pred, "auc")
# Plot ROC curve
plot(perf, main = "ROC Curve for Decision Tree", col = "blue", lwd = 2)
# Add AUC to the plot
text(0.8, 0.2, paste("AUC = ", round(as.numeric(roc_obj@y.values[[1]]), 2)), adj = 1)
# Add a diagonal line for reference
abline(a = 0, b = 1, lty = 2, col = "red")

# Create the confusion matrix
conf<- table(tree.pred,test_dataset_Y$Classes)
conf

# Calculate evaluation metrics based on the confusion matrix
accuracyvalue31 <- sum(diag(conf)) / sum(conf)
accuracyvalue31

# Print the confusion matrix and evaluation metrics
print("Confusion Matrix:")
print(conf)
print(paste("Accuracy:", accuracyvalue31))

## training data accuracy
train_predictions1 <- predict(fit2,newdata = train_dataset[,-11],type = "class")
length(train_dataset$Classes)
length(train_predictions1)
conf_matrix <- table(train_predictions1, train_dataset$Classes)

# Calculate accuracy
train2 <- sum(diag(conf_matrix)) / sum(conf_matrix)
train32
df=data.frame(Training_Accuracy=train2,Test_Accuracy=accuracyvalue31)
df


# tuning decision tree
library(rpart)
library(rpart.plot)

# Define the cross-validation function
cv_error <- function(cp, train_data, test_data) {
  tree_model <- rpart(Classes ~ ., data = train_dataset, cp = cp)
  predictions <- predict(tree_model, test_dataset, type = "class")
  error_rate <- mean(predictions !=test_dataset$Classes)
  return(error_rate)
}

# Specify the values of cp to try
cp_values <- seq(0.01, 0.5, by = 0.01)
train_dataset

# Perform cross-validation to find the best cp value
cv_results <- sapply(cp_values, cv_error, train_data = train_dataset, test_data = test_dataset)

# Find the cp value with the lowest error rate
best_cp <- cp_values[which.min(cv_results)]

# Build the final pruned tree using the best cp value
final_tree <- rpart(Classes ~ ., data = train_dataset, cp = best_cp)
pruned_tree <- prune(final_tree, cp = best_cp)
pruned_tree

# Plot the pruned tree
rpart.plot(pruned_tree, type = 0, extra = 101)
tree.pred <- predict(pruned_tree,test_dataset_X,type = "class")
tree.pred

# Create the confusion matrix
conf<- table(tree.pred,test_dataset_Y$Classes)
conf
# Calculate evaluation metrics based on the confusion matrix
accuracy_value31 <- sum(diag(conf)) / sum(conf)
accuracy_value31

# Print the confusion matrix and evaluation metrics
print("Confusion Matrix:")
print(conf)
print(paste("Accuracy:", accuracy_value31))

## training data accuracy
train_predictions <- predict(pruned_tree,newdata = train_dataset[,-11],type="class")
length(train_dataset$Classes)
length(train_predictions1)
conf_matrix <- table(train_predictions, train_dataset$Classes)

# Calculate accuracy
train32 <- sum(diag(conf_matrix)) / sum(conf_matrix)
train32
df=data.frame(Training_Accuracy=train32,Test_Accuracy=accuracy_value31)
df

## PRECISON AND RECALL FOR descion tree
library(caret)
library(MLmetrics)

# Create a confusion matrix
cm <- confusionMatrix(factor(tree.pred), factor(test_dataset_Y$Classes))
cm

# Calculate precision, recall, and F1-score
precision <- Precision(tree.pred,test_dataset_Y$Classes)
precision
recall <- Recall(tree.pred,test_dataset_Y$Classes)
recall
f1_score <- F1_Score(tree.pred,test_dataset_Y$Classes)
f1_score
df=data.frame(precision,recall,f1_score)
df

# Load the rpart package
library(rpart)

# Build the final pruned tree using the best cp value
fit0<- rpart(Classes ~ ., data = train_dataset, cp = best_cp, parms = list(split = "gini"))
fit<- prune(fit0, cp = best_cp)
fit

# Display the importance of each feature based on Gini index
print(fit$variable.importance)

# Sort features by importance
importance <- fit$variable.importance
sorted_importance <- sort(importance, decreasing = TRUE)
# Select the top N features (e.g., top 2 features)
top_features <- names(sorted_importance)[1:2]
# Create a new dataset with only the top features
new_data <- train_dataset[, c("Classes", top_features)]

# Fit a new decision tree model using only the top features
new_fit1 <- rpart(Classes ~ ., data = new_data, method = "class", parms = list(split = "gini"))
# Print the new model
print(new_fit1)
# Reduce plot margins
par(mar=c(1, 1, 1, 1))  # Set the margins to 1 inch on each side
# Plot the pruned tree with a smaller size
rpart.plot(new_fit1, type=0, extra=101, cex=0.8, tweak=0.8, fallen.leaves=TRUE)
# Reset plot margins to default values
par(mar=c(5, 4, 4, 2) + 0.1)

#fitting tree after feature selection
tree.pred <- predict(new_fit1,test_dataset_X,type = "class")
tree.pred

# Create the confusion matrix
conf<- table(tree.pred,test_dataset_Y$Classes)
conf

# Calculate evaluation metrics based on the confusion matrix
accuracy_value31 <- sum(diag(conf)) / sum(conf)
accuracy_value31

# Print the confusion matrix and evaluation metrics
print("Confusion Matrix:")
print(conf)
print(paste("Accuracy:", accuracy_value31))

## training data accuracy
train_predictions1 <- predict(new_fit1,newdata = train_dataset[,-11])
length(train_dataset$Classes)
length(train_predictions1)
conf_matrix <- table(train_predictions, train_dataset$Classes)

# Calculate accuracy
train32 <- sum(diag(conf_matrix)) / sum(conf_matrix)
train32
df=data.frame(Training_Accuracy=train32,Test_Accuracy=accuracy_value31)
df




## random forest
##install.packages("randomForest")
library(randomForest)
?randomForest

# Assuming 'Classes' is numeric or character
train_dataset$Classes <- as.factor(train_dataset$Classes)
model=randomForest(train_dataset$Classes~.,data=train_dataset,ntree=10,method="classification")
model
prediction <- predict(model,newdata =test_dataset_X,type="class")
prediction
# Create the confusion matrix
conf1<- table(prediction, test_dataset_Y$Classes)
conf1
# Calculate evaluation metrics based on the confusion matrix
accuracy_value5 <- sum(diag(conf1)) / sum(conf1)
accuracy_value5
# Print the confusion matrix and evaluation metrics
print("Confusion Matrix:")
print(conf1)
print(paste("Accuracy:", accuracy_value5))
## training data accuracy
train_predictions <- predict(model,newdata = train_dataset[,-11],typr="class")
train5 <- table(train_predictions, train_dataset$Classes)
train5
# Calculate evaluation metrics based on the confusion matrix
tr <- sum(diag(train5)) / sum(train5)
tr
print(paste("train dataset accuracy",tr))
df=data.frame(Training_Accuracy=tr,Test_Accuracy=accuracy_value5)
df

# Convert Classes to factor if it's not already
train_dataset$Classes <- as.factor(train_dataset$Classes)
test_dataset_Y$Classes <- as.factor(test_dataset_Y$Classes)

library(randomForest)
library(caret)

# Define the control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 5)

# Define the tuning grid
param_grid <- expand.grid(mtry = c(2, 3, 4, 5, 6))

# Tune the Random Forest model
rf_tune <- train(
  Classes ~ .,
  data = train_dataset,
  method = "rf",
  tuneGrid = param_grid,
  trControl = ctrl
)

# Print the tuned model
print(rf_tune)

# Get the best model
best_rf_model <- rf_tune$finalModel
best_rf_model
# Fit the best model to the full training data with reduced number of trees
model <- randomForest(Classes ~ ., data = train_dataset, mtry = best_rf_model$mtry, ntree = 100)

# Print the final model
print(model)

# Make predictions on the test dataset
prediction <- predict(model, newdata = test_dataset_X)

# Create the confusion matrix
conf1 <- table(prediction, test_dataset_Y$Classes)

# Calculate evaluation metrics based on the confusion matrix
accuracy_value51 <- sum(diag(conf1)) / sum(conf1)

# Print the confusion matrix and evaluation metrics
print("Confusion Matrix:")
print(conf1)
print(paste("Accuracy:", accuracy_value51))

## training data accuracy
train_predictions <- predict(model, newdata = train_dataset[, -11])
train52 <- table(train_predictions, train_dataset$Classes)
train00=sum(diag(train52)) / sum(train52)
print(paste("Train dataset accuracy", train00))
df=data.frame(Training_Accuracy=train51,Test_Accuracy=accuracy_value52)
df
# Create a confusion matrix
cm <- confusionMatrix(prediction,as.factor(test_dataset_Y$Classes))
cm

# Calculate precision, recall, and F1-score
precision <- Precision(prediction, test_dataset_Y$Classes)
recall <- Recall(prediction, test_dataset_Y$Classes)
f1_score <- F1_Score(prediction, test_dataset_Y$Classes)

# Create a data frame with the metrics
df <- data.frame(precision, recall, f1_score)
print(df)



## combining the accuracy of data
d=c("logistic regression"," SVM","Descion tree","random forest")
d1=c(accuracy_value11,accuracy_value21,accuracy_value31,accuracy_value51)
d2=c(train12,train22,train32,train52)
d1
d2
df=data.frame(model=d,test_accuracy=d1,train_accuracy=d2)
df