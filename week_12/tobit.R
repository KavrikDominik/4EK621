require(VGAM)
library(readr)
library(GGally)

dat <- read_csv("https://stats.idre.ucla.edu/stat/data/tobit.csv")
summary(dat)

# function that gives the density of normal distribution
# for given mean and sd, scaled to be on a count metric
# for the histogram: count = density * sample size * bin width
f <- function(x, var, bw = 15) {
  dnorm(x, mean = mean(var), sd(var)) * length(var)  * bw
}

# setup base plot
p <- ggplot(dat, aes(x = apt, fill=prog))

# histogram, coloured by proportion in different programs
# with a normal distribution overlayed
p + stat_bin(binwidth=15) +
  stat_function(fun = f, size = 1,
                args = list(var = dat$apt))

p + stat_bin(binwidth = 1) + stat_function(fun = f, size = 1, args = list(var = dat$apt, 
                                                                          bw = 1))
cor(dat[, c("read", "math", "apt")])


ggpairs(dat[, c("read", "math", "apt")])


m <- vglm(apt ~ read + math + prog, tobit(Upper = 800), data = dat)
summary(m)

# the linear effect is on the uncensored latent variable, not the observed outcome.


# The coefficient labeled “(Intercept):2” is an ancillary statistic. 
# If we exponentiate this value, we get a statistic that is analogous to the 
# square root of the residual variance in OLS regression. The value of 65.6773 can 
# compared to the standard deviation of academic aptitude which was 99.21,
# a substantial reduction.



# Calculate p-values from z-values ----------------------------------------


ctable <- coef(summary(m))
pvals <- 2 * pt(abs(ctable[, "z value"]), df.residual(m), lower.tail = FALSE)
cbind(ctable, pvals)


m2 <- vglm(apt ~ read + math, tobit(Upper = 800), data = dat)


# Likelihood ratio test ---------------------------------------------------

# to see if variable program is statistically significant
(p <- pchisq(2 * (logLik(m) - logLik(m2)), df = 2, lower.tail = FALSE))


b <- coef(m)
se <- sqrt(diag(vcov(m)))

cbind(LL = b - qnorm(0.975) * se, 
      UL = b + qnorm(0.975) * se)


dat$yhat <- fitted(m)[,1]
dat$rr <- resid(m, type = "response")
dat$rp <- resid(m, type = "pearson")[,1]

par(mfcol = c(2, 3))

with(dat, {
  plot(yhat, rr, main = "Fitted vs Residuals")
  qqnorm(rr)
  plot(yhat, rp, main = "Fitted vs Pearson Residuals")
  qqnorm(rp)
  plot(apt, rp, main = "Actual vs Pearson Residuals")
  plot(apt, yhat, main = "Actual vs Fitted")
})


(r <- with(dat, cor(yhat, apt)))

r^2
# predicted values share 61.23% of their variance with apt