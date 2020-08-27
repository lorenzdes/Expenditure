library(tidyr)
library(ggplot2)
library(leaps)
library(tidyverse)
library(dummies)
library(boot)
library(splines)
library(tree)
#load data 
data = read.csv("C:/Users/Lorenzo de Sario/Desktop/marketing/MarketingData1.csv", header = TRUE, sep = ",")
summary(data)
#switch off scientific notation
options(scipen=999, digits = 3)

data = data %>%
  drop_na(History)

#replace missing int with the median
data %>%
  group_by(Children) %>%
  mutate(AmountSpent=ifelse(is.na(AmountSpent), median(AmountSpent, na.rm=TRUE), AmountSpent)) %>%
  as.data.frame()

attach(data)
#Exploratory Data Analysis
#Quantitative variables
hist = ggplot(data, mapping = aes(x = data$AmountSpent)) 
hist + geom_histogram(color = "red2", bins = 15) + xlab('Amount Spent') + ylab('Frequency')  #Normal distribution

hist1 = ggplot(data, mapping = aes(x = data$Salary)) 
hist1 + geom_histogram(color = "green1", bins = 20) + ylab('Frequency')
#Qualitative variables and amount spent
box = ggplot(data, mapping = aes(x = data$Strategy, y = data$AmountSpent))
box + geom_boxplot(fill = "#FFDB6D", color = "#C4961A")+ylab('Amount Spent')+xlab('Strategy')

box1 = ggplot(data, mapping = aes(x = data$Location, y = data$AmountSpent))
box1 + geom_boxplot(fill = "lightgray", color = "black", outlier.colour = 'red', outlier.shape = 16)+ylab('Amount Spent')+xlab('Location')

box2 = ggplot(data, mapping = aes(x = data$History, y = data$AmountSpent))
box2 + geom_boxplot(fill = "#D16103", color = "#4E84C4")+ylab('Amount Spent')+ xlab('History')

#choosing reference categories. That is modelling with respect to
#those individuals who are targetted by a Brochure, that own a home and are single men. 
data = data %>% mutate(Strategy = relevel(Strategy, ref = "Brochure")) 
data = data %>% mutate(Gender = relevel(Gender, ref = "Male")) 
data = data %>% mutate(Married = relevel(Married, ref = "Single"))
data = data %>% mutate(OwnHome = relevel(OwnHome, ref = "Own"))

 #Encoding Ordinal categorical variable. Assume equidistance between the levels
data$Age = as.numeric(data$Age,
                     labels = c("Old", "Middle", "Young"),
                     levels = c(1, 2, 3))
data$History = as.numeric(data$History,
                          labels = c("High", "Medium", "Low"),
                          levels = c(1, 2, 3))

#fitting splines and selecting the smoothing parameter by the LOO CV
fit <- smooth.spline(AmountSpent, Salary, cv = TRUE)
plot(AmountSpent, Salary, col="darkgrey")
lines(fit, col="purple", lwd=2)

model_sal <- lm(log(AmountSpent)~ Salary, data = data)

#Bootstrapping Adjusted R-squared linear regression (p.187)
library(car)

set.seed(123) #reproducibility of the experiment

Adj_R_squared = function(data, indices){
  marketing = data[indices,] #creating my sample with replacement
  
  model_sal <- lm(log(AmountSpent)~ Salary, data = marketing)
  AR.s = summary(model_sal)$r.square
  
  model_sal1 <- lm(sqrt(AmountSpent)~ I(Salary^2), data = marketing)
  AR.s1 = summary(model_sal1)$r.square
  
  model_strat <- lm(AmountSpent~ Strategy, data = marketing)
  AR.st = summary(model_strat)$r.square
  
  model_child <- lm(AmountSpent~ Salary + Salary:Children, data = marketing)
  AR.c = summary(model_child)$r.square
  
  model_S.S <- lm(AmountSpent~ ns(Salary, df = 5) + Strategy, data = marketing)
  AR.s.s = summary(model_S.S)$r.square
  
  model_S.S.C <- lm(log(AmountSpent)~ Salary + Strategy + Children, data = marketing)
  AR.s.s.c = summary(model_S.S.C)$r.square
  
  model_S.S.C.G <- lm(log(AmountSpent)~ Salary + Strategy + Children + Gender, data = marketing)
  AR.s.s.c.g = summary(model_S.S.C.G)$r.square
  
  full_model <- lm(log(AmountSpent)~. , data = marketing)
  AR.full = summary(full_model)$r.square
  
  Adjusted_r_sq = c(AR.s, AR.s1, AR.st, AR.c, AR.s.s, AR.s.s.c, AR.s.s.c.g, AR.full)
  return(Adjusted_r_sq)
}
#use the boot() function to compute variability SE(R^2) of 5000 bootstrap estimates for the Adjusted R squared 
boot(data, Adj_R_squared, R = 1000)

#make prediction
set.seed(456)
train = sample(c(TRUE, FALSE), nrow(data), rep = TRUE)
test = (!train)
full_model <- lm(log(AmountSpent)~. , data = data[train,])
vif(full_model)
sqrt(vif(full_model))>1.5
#MSE sqrt between real data and test set
sqrt(mean((AmountSpent -predict(full_model, data))[-train]^2))

#Forward stepwise regression - model selection  (p.245)
library(leaps)
#Choosing a model with validation set approach: test error
set.seed(789)
#Forward Regression
fwd <- regsubsets(AmountSpent ~., data = data[train,], method = "forward", nvmax = 10)
test_matrix = model.matrix(AmountSpent~., data = data[-train,])
validation_error = rep(NA, 10)
for (i in 1:10){
  coeff = coef(fwd, id = i)
  predicted = test_matrix[,names(coeff)]%*%coeff
  validation_error[i] = mean((data$AmountSpent[-train]- predicted)^2)
}

which.min(validation_error) #model with best validation error
coef(fwd,9) #the best model contains 9 variables
#choosing a model with maximum adjusted r squared
summary(fwd)
plot(fwd ,scale = "Cp")

reg.summary = summary(fwd)
which.max(reg.summary$adjr2)

#decision tree
set.seed(101112)
test = data[test,]
testY = test$AmountSpent
tree <- tree(AmountSpent~., data[train,])
summary(tree)
plot(tree)
text(tree, pretty = 0)
tree_prediction = predict(tree, test)
sqrt(mean((tree_prediction - testY)^2)) #is better MSE sqrt in regression 
#high sqrt since high values of Y

#cv for pruning the tree (p.327)
cv_tree = cv.tree(tree)
plot(cv_tree$size, sqrt(cv_tree$dev), type = 'b', xlab = 'Tree Size', ylab = 'sqrt MSE')
which.min(sqrt(cv_tree$dev)) #index 3
cv_tree$size[3] #size 5

# prune the tree to size 5
prune <- prune.tree(tree, best = 5)
plot(prune)
text(prune, pretty = 0)
tree.pred = predict(prune, test)
sqrt(mean((tree.pred - testY)^2)) #by pruning there is a reduction in the mean squared error
