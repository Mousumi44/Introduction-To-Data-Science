library(MASS)

#(3a) rows, columns, dataset representation
nrow(Boston)
ncol(Boston)
summary(Boston)

#(3b) make some pairwise prediction
pairs(Boston)

#3(c) Are any of the predictors associated with per capita crime rate?If so, explain the relationship.
Boston.corr = cor(Boston)
Boston.corr.crim = Boston.corr[-1,1]
print(
  Boston.corr.crim[order(abs(Boston.corr.crim), decreasing = T)]
)


#3(d)d) Do any of the suburbs of Boston appear to have particularly high crime rates? Tax rates? Pupil-teacher ratios? Comment on the range of each predictor.
hist(Boston$crim)
length(Boston$crim[Boston$crim>20])

hist(Boston$tax)
length(Boston$tax[Boston$tax>500])

hist(Boston$ptratio)
length(Boston$ptratio[Boston$ptratio>20])

#3(e) How many of the suburbs in this data set bound the Charles river?
table(Boston$chas)

#3f) What is the median pupil-teacher ration among the towns in this data set?
median(Boston$ptratio)

#3(g)
subs.lw = which(Boston$medv<median(Boston$medv))
print(subs.lw)

#3(h)
hist(Boston$rm)
length(Boston$rm[Boston$rm>7])
length(Boston$rm[Boston$rm>8])
Boston[Boston$rm>8 & Boston$medv<30, ]