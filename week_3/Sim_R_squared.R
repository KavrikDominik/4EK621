# Simulation of the issue with R squared and # of regressors  ------------------
set.seed(155) # seed for random number generator
N <- 100  
C <- c() # empty vector initialization
D <- c() # empty vector initialization
CC <- list() # empty list initialization
const <- cbind(rep(1, N)) # column vector of ones for design matrix X
X <- cbind(const) # matrix X
Y <- rnorm(N, 2, 2) # column vector of dependent variable

# now we iterate from 1 to 99 and in each iteration we a new regressor 
for (i in 1:99) {
  xi <- rnorm(N, 1, 2) # new regressor
  X <- cbind(X, xi) # add new regressor to the design matrix X
  k <- ncol(X) - 1 # number of regressors without the constant term
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y # run OLS
  Y_hat <- X %*% beta # obtain fitted values
  SSR <- sum((Y_hat - Y)^2) # calculate sum of squared residuals 
  SST <- sum((Y - mean(Y))^2) # calculate the sum of squares total
  SSE <- SST - SSR # get explained sum of squares
  R_squared <- SSE / SST # calculate R squared   
  R_adj <- 1 - (((1 - R_squared) * (N - 1)) / (N - k - 1)) #...and andjusted R^2
  print(i) 

  # now save everything to intialized objects  
  C[i] <- R_squared
  D[i] <- R_adj
  CC[["R_squared"]][i] <- R_squared
  CC[["R_adjusted"]][i] <- R_adj
}

# Plotting the results: ---------------------------------------------------

par(
  mfrow = c(2, 1),
  mar = c(2, 4, 1, 2)
)

plot.ts(C,
  col = "red",
  ylab = "R Squared",
  xlab = NULL
)

abline(h = 0, lty = 2)

abline(h = 1, lty = 2)

plot.ts(D,
  col = "blue",
  ylab = "Adj. R^2",
  xlab = "Počet regresorů",
  ylim = c(-1.5, 1.1)
)

abline(
  h = 0,
  lty = 2
)
