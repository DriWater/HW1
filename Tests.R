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


# Test functions on simulation data with intercept term

# Model parameters
p = 10 # number of covariates
sigma = 2 # noise standard deviation
beta = c(3, rnorm(p, 1, 1)) # true vector of coefficients

# Training data generator
n = 100 # sample size for training data
X = matrix(rnorm(n * p), n, p) # n by p matrix of predictors
X = cbind(rep(1,n),X)
# Use generateY function to generate Y for training data with default seed
Y <- generateY(X,beta,sigma)
# Use calculateBeta function to calculate beta_LS
beta_LS <- calculateBeta(X,Y)
# Use calculateEstimationError to assess the estimation error measured by squared eucledian distance - ||beta - beta_LS||_2^2. Report the error in the comments.
calculateEstimationError(beta,beta_LS) #0.7077829
# Use calculatePredictionError to asses the prediction error on Ytest. Report the error in the comments.
calculatePredictionError(Y, X, calculateBeta(X, Y)) #19.90866