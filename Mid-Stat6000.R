#5
#(a)
library(ISLR)
head(Carseats)
lm.fit1 = glm(Sales ~ Price+Urban+US, data = Carseats)
summary(lm.fit1)

#(b)
confint(lm.fit1)

#(c)
lm.fit2 = glm(Sales ~ Price+US, data= Carseats)
summary(lm.fit2)
#10-fold cross validation
library(boot)
set.seed(17)
cv.error.m1=cv.glm(Carseats, lm.fit1, K=10)$delta[1]
cv.error.m1
cv.error.m2=cv.glm(Carseats, lm.fit2, K=10)$delta[1]
cv.error.m2

#(d) computing standard error of the coefficients for smaller model
#standard formulas
summary(glm(Sales ~ Price+US, data= Carseats))$coef
#bootstrap formulas
boot.fn=function(data, index){
  return(coef(glm(Sales ~ Price+US, data= data, subset = index)))
}
set.seed(1)
boot(Carseats, boot.fn, 1000)

#6 Split the college dataset into a training set and testing set
library(ISLR)
library(caret)
library(glmnet)
library(dplyr)
library(tidyr)
library(data.table)
data(College)
head(College)
set.seed(1)

inTrain <- createDataPartition(College$Apps, p = 0.75, list = FALSE)

training <- College[inTrain,]
testing <- College[-inTrain,]

x_train = model.matrix(Apps~., training)[,-1]
x_test = model.matrix(Apps~., testing)[,-1]

y_train= training  %>%
  select(Apps) %>%
  unlist() %>%
  as.numeric()
y_test= testing  %>%
  select(Apps) %>%
  unlist() %>%
  as.numeric()


#(a) Fit a linear model using least squares on the training set, and report the test error obtained.
lin_model = lm(Apps~., data=training)
pred = predict(lin_model, testing)

lin_info = postResample(pred, testing$Apps)
lin_info

#(b) Fit a ridge regression model on the training set, with  chosen by cross-validation. Report the test error obtained.
ridge_mod=glmnet(x_train, y_train, alpha = 0)

set.seed(1)
cv.out = cv.glmnet(x_train, y_train, alpha=0)
bestlam = cv.out$lambda.min

ridge_pred = predict(ridge_mod, s = bestlam, newx=x_test)
ridge_info = postResample(ridge_pred, y_test)
ridge_info

#(c) Fit a lasso model on the training set, with  chosen by cross validation. Report the test error obtained, along with the number of non-zero coecient esti-mates.
lasso_mod=glmnet(x_train, y_train, alpha = 1)

set.seed(1)
cv.out = cv.glmnet(x_train, y_train, alpha=1)
bestlam = cv.out$lambda.min

lasso_pred = predict(lasso_mod, s=bestlam, newx=x_test)
lesso_info = postResample(lasso_pred, y_test)
predict(lasso_mod, type = "coefficients" , s=bestlam)[1:18,]
lesso_info

#(d) Fit a PCR model on the training set, with M chosen by cross validation.
#Report the test error obtained, along with the value of M selected by cross-validation.

library(pls)

pcr_model = pcr(Apps~., data=training, scale=T, validation = "CV")
validationplot(pcr_model, val.type = "MSEP")

#From plot we see that we should keep 5 components
pcr_pred = predict(pcr_model, testing, ncomp = 5)
pcr_info = postResample(pcr_pred, testing$Apps)
pcr_info

#(e) Fit a PLS model on the training set, with M chosen by cross validation.
#Report the test error obtained, along with the value of M selected by cross-validation.

pls_model = plsr(Apps~., data=training, scale=T, validation = "CV")
validationplot(pls_model, val.type = "MSEP")

#Here 6 components seem better
pls_pred = predict(pls_model, testing, ncomp = 6)
pls_info = postResample(pls_pred, testing$Apps)
pls_info

#(f) Comment on the results obtained. How accurately can we predict the
#number of college applications received? Is there much difference among the test
#errors resulting from these five approaches?

as_data_frame(
  rbind(lin_info, ridge_info, lesso_info, pcr_info, pls_info)
) %>%
  mutate(model= c('Linear' , 'Ridge' , 'Lasso' , 'PCR', 'PLS')) %>%
  select(model, RMSE, Rsquared, MAE)





