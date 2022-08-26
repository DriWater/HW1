# Generate n-dimensional response Y that follows linear regression model Y = Xbeta + epsilon, where epsilon is normal zero with variance sigma^2 independent across samples. Seed should be set at the beginning of the function
# X - design matrix, rows are n samples
# beta - given parameter vector (could be a vector or a matrix with 1 column)
# sigma - standard deviation of the noise, scalar
# seed  - starting seed value, integer
generateY <- function(X, beta, sigma, seed = 5832652){
  #Set seed and generate Y following linear model
  X <- as.matrix(X)
  noise <- rnorm(nrow(X),0,sigma)
  Y <- X%*%beta + noise
  # Return Y
  return(Y)
}

sigma = 1 # noise standard deviation
beta = c(2, 1) # true vector of coefficients
# Training data generator
n = 100 # sample size for training data
X = matrix(matrix(c(1,1,4,5,1,4)), 3, 2) # 3 by 3 matrix of predictors
# [generate Y for training data with default seed
Y <- generateY(X,beta,sigma)

# Calculate beta_LS - least-squares solution, do not use lm function
# You can assume that X has full rank, so X'X inverse exists
# X - design matrix, rows are n samples
# Y - response vector (could be a vector or a matrix with 1 column)
calculateBeta <- function(X, Y){
  # Calculate beta_LS
  X <- as.matrix(X)
  Y <- as.vector(Y)
  beta_LS <- solve(t(X)%*%X)%*%t(X)%*%Y
  # Return beta
  return(beta_LS)
}

beta_LS <- calculateBeta(X,Y)

# Calculate estimation error, defined as ||beta - beta_LS||_2^2
# beta - true coefficient vector (could be a vector or a matrix with 1 column)
# beta_LS - vector estimated by LS (could be a vector or a matrix with 1 column)
calculateEstimationError <- function(beta, beta_LS){
  # Calculate and return error
}


# Calculate prediction error, defined as ||Y - X beta_LS||_2^2
# Y - response vector (could be a vector or a matrix with 1 column)
# X - design matrix, rows are n samples
# beta_LS - vector estimated by LS (could be a vector or a matrix with 1 column)
calculatePredictionError <- function(Y, X, beta_LS){
  # Calculate and return error
}