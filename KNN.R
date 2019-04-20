## Apply K-Nearest neighbour algorithm on Prostate_cancer dataset
prc <- read.csv("Prostate_Cancer.csv",stringsAsFactors = FALSE) 
#This command imports the required data set and saves it to the prc data frame.This command helps to convert every character vector to a factor wherever it makes sense.
str(prc) 
#We use this command to see whether the data is structured or not.
prc <- prc[-1]  
#removes the first variable(id) from the data set.
print(table(prc$diagnosis_result)) 
# it helps us to get the numbers of patients
prc$diagnosis <- factor(prc$diagnosis_result, levels = c("B", "M"), labels = c("Benign", "Malignant"))
# creating new variable diagnosis which is a factor
str(prc) 
# printing the data structure
print (round(prop.table(table(prc$diagnosis)) * 100, digits = 1) )
# it gives the result in the percentage form rounded of to 1 decimal place( and so it’s digits = 1)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
# function to normalize the data - scaling
prc_n <- as.data.frame(lapply(prc[2:9], normalize))
# removing redundant variable diagnosis_result and creating a new data frame
print(summary(prc_n$radius))
# testing normalization
prc_train <- prc_n[1:65,]
prc_test <- prc_n[66:100,]
# creating test and training datasets
prc_train_labels <- prc[1:65, 1]
prc_test_labels <- prc[66:100, 1]
#This code takes the diagnosis factor in column 1 of the prc data frame and on turn creates prc_train_labels and prc_test_labels data frame.
library(class)
prc_test_pred <- knn(train = prc_train, test = prc_test,cl = prc_train_labels, k=10)
View(data.frame(prc_test_pred))
# training and predicting using KNN
View(prc_test_pred)
# Viewing the predictions made by the model
library(gmodels)
CrossTable(x=prc_test_labels,y=prc_test_pred,prop.chisq=FALSE)
# Generating confusion matrix
