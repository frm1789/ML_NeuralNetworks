## Chapter 7: NN
## Machine Learning with R

set.seed(12345)

library(neuralnet)
library(readr)
library(tidyverse)

## Step 1. Get data
url_data <- "https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/concrete.csv"
concrete <- read_csv(url(url_data))

## Step 2. Exploring and Analyzing data
str(concrete)

normalize <- function (x){
  return ((x - min(x)) / (max(x) - min(x)))
}

concrete_norm <- as.data.frame(lapply(concrete, normalize))

summary(concrete$strength)
summary(concrete_norm$strength)

concrete_train <- concrete_norm[1:773,]
concrete_test <- concrete_norm[774:1030,]

## Step 3: Creating model 
concrete_model <- neuralnet::neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, 
                                       data = concrete_train)

plot(concrete_model)

## Step 4: Evaluating the model
model_results <- neuralnet::compute(concrete_model, concrete_test[1:8])
predicted_strenght <- model_results$net.result
cor(predicted_strenght, concrete_test$strength)
# 0.7188351766

## Step 5: Improving model performance
concrete_model2 <- neuralnet::neuralnet(strength ~ cement + slag + 
                                                    ash + water + superplastic + 
                                                    coarseagg + fineagg + age, 
                                       data = concrete_train,
                                       hidden = 4)

plot(concrete_model2)

## Step 6: Evaluating the improved model
model_results2 <- neuralnet::compute(concrete_model2, concrete_test[1:8])
predicted_strenght2 <- model_results2$net.result
cor(predicted_strenght2, concrete_test$strength)
# hidden = 4 
# 0.7878222697
