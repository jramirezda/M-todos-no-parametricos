
# Tomado de 
# http://www.sthda.com/english/articles/
       37-model-selection-essentials-in-r/
       154-stepwise-regression-essentials-in-r/


library(tidyverse)
library(caret)
library(leaps)
library(MASS)

# Nota: stepAIC() [MASS package], which choose the best model by AIC. 
# It has an option named direction, which can take the following values: 
# i) “both” (for stepwise regression, both forward and backward selection); 
# “backward” (for backward selection) and “forward” (for forward selection). 
# It return the best final model.

data(swiss)
head(swiss)
dim(swiss)
swiss

#---------------------------
# Fit the full model 
#---------------------------

full.model <- lm(Fertility ~., data = swiss)
summary(full.model)

#----------------------------------------------------
# Stepwise regression model usando la librería MASS
# ambos: forward y backward
#-----------------------------------------------------

step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
summary(step.model)

step.model1 <- stepAIC(full.model, direction = "backward", trace = FALSE)
summary(step.model1)

step.model2 <- stepAIC(full.model, direction = "forward", trace = FALSE)
summary(step.model2)




#------------------------------
# Usando la libería leaps
#------------------------------

# regsubsets() [leaps package], which has the tuning parameter nvmax 
# specifying the maximal number of predictors to incorporate in the model 
# (See Chapter @ref(best-subsets-regression)). It returns multiple models 
# with different size up to nvmax. You need to compare the performance of 
# the different models for choosing the best one. 
# regsubsets() has the option method, which can take the values “backward”, 
# “forward” and “seqrep” (seqrep = sequential replacement, combination of 
# forward and backward selections).

models <- regsubsets(Fertility~., data = swiss, nvmax = 5,
                     method = "backward")
summary(models)


models2 <- regsubsets(Fertility~., data = swiss, nvmax = 5,
                     method = "forward")
summary(models2)






