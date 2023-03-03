# Simulace nešvaru koeficientu determinace  -------------------------------
set.seed(155)
N <- 100
C <- c()
D <- c()
CC <- list()
const <- cbind(rep(1, N)) # vektor jednicek
X <- cbind(const) # matice X zatím pouze s jednickami
Y <- rnorm(N, 2, 2) # vysvětovaná proměnná
# od jednoho náhodného regresorů az po 100 regresorů:

for (i in 1:99) {
  xi <- rnorm(N, 1, 2) # nový regresor
  X <- cbind(X, xi) # přidáme do matice X
  k <- ncol(X) - 1 # počet regresorů bez konstanty
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  Y_hat <- X %*% beta
  SSR <- sum((Y_hat - Y)^2)
  SST <- sum((Y - mean(Y))^2)
  SSE <- SST - SSR
  R_squared <- SSE / SST
  R_adj <- 1 - (((1 - R_squared) * (N - 1)) / (N - k - 1))
  print(i)
  C[i] <- R_squared
  D[i] <- R_adj
  CC[["R_squared"]][i] <- R_squared
  CC[["R_adjusted"]][i] <- R_adj
}

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
