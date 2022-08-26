# Use this file to create tests/debug your functions

# Source the functions
source("FunctionsLM.R")

# load and process data set
dat = iris
dat$isSetosa <- ifelse(dat$Species == 'setosa',1,0)
dat$isVersicolor <- ifelse(dat$Species == 'versicolor',1,0)
dat <- dat[,-5]

# Test functions on iris datasets
Y  = dat$Petal.Length
X = dat[,-3]
beta_LS <- calculateBeta(X,Y)
calculatePredictionError(Y,X,beta_LS) #3.23908