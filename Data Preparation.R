#Question 1
data_set<-read.csv("bank-additional-full.csv",header =TRUE,sep = ";")
library(dplyr)
data_set1<-data_set %>% select("age","education","previous","pdays","y")

#before replace 999 to NA
hist(as.numeric(data_set1$pdays), main = " before replacing 999 in pdays", xlab = "pdays")
#question 2
data_set1$pdays[data_set1$pdays ==999]<- NA

#question 4
summary(data_set1)
hist(as.numeric(data_set1$pdays), main = " After replacing 999 in pdays by NA",
     xlab = "pdays")

#question 5
#install.packages("plyr")
library(plyr)
replace_edu<- revalue(data_set1$education,c("illiterate" = 0, "basic.4y" = 4,
    "basic.6y" = 6,"basic.9y"=9,"high.school"=12,
    "professional.course"=14,"university.degree"=16,"unknown"=NA)
    ,warn_missing = TRUE)
data_set1$education=replace_edu

#question 6
age_mean <- mean(data_set1$age,na.rm = FALSE)
age_median <- median(data_set1$age, na.rm = FALSE)
#install.packages("statip")
library(statip)
age_mode <- mfv(data_set1$age)
#another way to find mode to age
age__mode <- function(x) {
  uniqx<- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
result <- age__mode(data_set1$age)
summary(data_set1$age)
boxplot(data_set1$age,data=data_set1, main="age of data", xlab="age",col = "red")
#question 7
age_z <- scale(x = data_set1$age)
data_set1$age_z<-age_z

#question 8
boxplot(data_set1$age_z,data=data_set1, main="age_z of data", xlab="age_z",
        col = "orange")
age_z_outliers <-  data_set1[ which( data_set1$agez  > 2), ]
age_z_outliers
