
#################################################################
##    Marketing analytics                                      ##
##    Exercise in Segmentation AND Discriminant Analysis       ##
##    Carseats Data                                            ##
#################################################################

# free memory
rm(list = ls())
gc()

#The tree library is used to construct classification trees.
library (tree)
library (ISLR)
library(randomForest)
library(caret)
library(e1071)


data <-read.csv("C:\\Users\\Dr.ooz\\Downloads\\Churn_Modelling.csv",header=T)



#Observe a few examples
head(data)

#Remove Some Attributes 
finaldata = subset(data, select = -c(1,2,3) )





#Convert tot dummy
results <- fastDummies::dummy_cols(finaldata,select_columns = "Gender" ,remove_first_dummy = TRUE)



df = subset(results, select = -c(Gender) )


# Convert categorical variables to numeric

must_convert<-sapply(df,is.factor)       
Geography<-sapply(df[,must_convert],unclass)
f1<-cbind(df[,!must_convert],Geography) 


f1$Exited <- factor(f1$Exited)



x <- subset(f1, select = -c(Exited)) 
y <- subset(f1, select = c(Exited))

# Cross vali 

#10 folds repeat 3 times  in order to slows down our process.
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)
#Metric compare model is Accuracy
metric <- "Accuracy"
set.seed(123)
#Number randomely variable selected is mtry
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
model <- train(Exited~., 
               data=f1, 
               method='rf', 
               metric='Accuracy', 
               tuneGrid=tunegrid, 
               trControl=control)
# Summarise Results
print(model)



