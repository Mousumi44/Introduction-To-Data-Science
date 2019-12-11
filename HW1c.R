library(readr)
Auto <- read_csv("C:/Users/USER/Downloads/Auto.csv")
View(Auto)

nrow(Auto)
#missing values have been removed from the data?
Auto <- na.omit(Auto)
nrow(Auto)

#2(a) Which of the predictors are quantitative, and which are qualitative?
sapply(Auto, class)


#2(b) range of quantitative predictor
quant <- sapply(Auto, is.numeric)
obj = sapply(Auto[, quant], range)
rownames(obj) <- c( "min", "max")
obj

#2(c) What is the mean and standard deviation of each quantitative predictor?
obj = sapply(Auto[, quant], function(x) signif(c(mean(x), sd(x)), 2))
rownames(obj) <- c( "mean", "sd")
obj

#2(d) Now remove the 10th through 85th observations. What is the range, mean, and standard deviation of each predictor in the subset of the data that remains?
output <- sapply(Auto[-10:-85, quant], function(x) round(c(range(x), mean(x), sd(x)), 2))
rownames(output) <- c("min", "max", "mean", "sd")
output

#2(e) Using the full data set, investigate the predictors graphically, using scatterplots or other tools of your choice. Create some plots highlighting the relationships among the predictors. Comment on your finding
Auto$horsepower = as.factor(Auto$horsepower)
Auto$name = as.factor(Auto$name)
pairs(Auto[,1:7])