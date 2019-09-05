library(readr)

#(a)load college.csv
College <- read_csv("C:/Users/USER/Downloads/College.csv")
View(College)

#(b) use fix function
rownames (College)=College [,1]
fix (College )

College =College [,-1]
fix (College )

#(ci) use summary function
summary(College)

#(cii) use pairs function
College$Private <- as.factor(College$Private)
pairs(College[, 1:10])

#(ciii) plot Outstate vs. Private
Private <- as.factor(Private)
plot(Private, Outstate, xlab="Private", ylab="Outstate")


#(civ) Create a new qualitative variable, called Elite, by binning
# the Top10perc variable.
Elite =rep ("No",nrow(College ))
Elite [College$Top10perc >50]=" Yes"
Elite =as.factor (Elite)
College =data.frame(College ,Elite)

summary(College)
#plot Outstate vs. Elite
plot(Elite, Outstate, xlab="Elite", ylab="Outstate")

#(cv) Use the hist() function to produce some histograms
par(mfrow=c(2,2), mar=c(2, 2, 1, 0))
hist(College$Apps)
hist(College$Accept)
hist(College$Enroll)
hist(College$Top10perc)

#(cvi) Continue exploring the data, and provide a brief summary of what you discover.
pairs(College[, 1:4])