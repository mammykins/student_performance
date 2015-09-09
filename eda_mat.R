#Student performance practise for Brilliant Club
#https://archive.ics.uci.edu/ml/datasets/Student+Performance
#Imagine we want to measure the effect of the brilliant club on...?
#Here we repeat the work of Cortez 2008
#We are interested in is it possible to predict student performance?
#Can we identify the important variables in determining intervention?

#----------------------------------------------------------------

#SETUP
rm(list = ls()) #clear workspace

setwd("C://Users//mammykins//Google Drive//R//student_cortez") #set wd
getwd() #check wd has been changed, make sure file is here

#PACKAGES
if(!require("dplyr")) {
  install.packages("dplyr")
  require("dplyr")
}  # if dplyr is not installed, install it then load it

if(!require("C50")) {
  install.packages("C50")
  require("C50")
} 

if(!require("gmodels")) {
  install.packages("gmodels")
  require("gmodels")
} 

source("normalise.R")

#----------------------------------------------------------------

#INPUT
mydata <- "student-mat.csv"  # Check this is the file you want
mydata <- read.table(mydata, sep = ";",
                     header = TRUE) #semi colon separated, we can assign factors later

#head(mydata) #  check top and bottom looks OK
#tail(mydata)

#str(mydata) #  check variables are correct type, we need to assign as factors

#hist(mydata$G3) # We can expect the distribution of the final grade - G3

#MAKE BINARY (pass or fail)
mydata$final <- NULL
mydata$final <- factor(ifelse(mydata$G3 >= 10, 1, 0),
                       labels = c("fail", "pass")) # care of alphabetical order
#round(prop.table(table(mydata$final)), 2) # confirm it worked by matching G3 with final

#-----------------------------------------------------------------
# RANDOMISE ORDER in case it was ordered by top score, for example
set.seed(1337)
#data_rand <- tbl_df(mydata[order(runif(395)), ]) #  no need

#what variables are we interested in?
data_interest <-select(mydata, school, sex, G1, G2, Mjob, Fjob, goout, 
       absences, reason, Fjob, Mjob, failures, Fedu, Medu, final)

#normalise the data so they are on the same scale
#can you find a faster way to apply a function to each column
data_interest$G1 <- normalise(data_interest$G1)
data_interest$G2 <- normalise(data_interest$G2)
data_interest$goout <- normalise(data_interest$goout)
data_interest$absences <- normalise(data_interest$absences)
data_interest$failures <- normalise(data_interest$failures)
data_interest$Fedu <- normalise(data_interest$Fedu)
data_interest$Medu <- normalise(data_interest$Medu)

#--------------------------------------------------------------------
#create training and test sets
data_train <- data_interest[1:350, ]
data_test <- data_interest[351:395, ]

#Build the classifier
m <- C5.0(x = data_train[-13], y = data_train$final) 
#  final is the class variable so we need to exclude it from training
summary(m)
#only 5% error rate, let's see how generalisable the model is

#PREDICT
p <- predict(m, data_test)
CrossTable(data_test$final, p, prop.chisq = FALSE,
           prop.c = FALSE, prop.r = FALSE, dnn = c("actual pass",
                                                   "predicted pass"))

#93.4% model accuracy not bad, 3 studnets proved us wrong and passed anyway
#seems like a useful model for identifying students who need extra intervention
#and can be applied and interpreted by a human




