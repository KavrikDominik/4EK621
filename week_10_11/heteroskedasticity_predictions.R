# libraries
library(dplyr)
library(ggplot2)
library(stargazer)
library(lmtest)
library(whitestrap)
library(sandwich)
library(stats)
library(readr)

options(scipen=7)  # Makes R use the scientific number format (1.234e+05) less often

########################## Heteroskedasticity ##########################

rm(list = ls())

# read data
data <- read_csv("week_10_11/data/wages.csv")

# estimate the regression model
model1 <- lm(wage ~ educ, data)
summary(model1)

### Heteroskedasticity tests ###

# 1. perform the Breusch�Pagan test to check the heteroscedasticity
bptest(model1)

# 2. perform the White test to check the heteroscedasticity
white_test(model1)

### How to fix it? ###

# 1. different specification / logarithmic transformation might help
ggplot(data, aes(x = wage)) + geom_histogram()
ggplot(data, aes(x = log(wage))) + geom_histogram()
model2 <- lm(log(wage) ~ educ, data)
summary(model2)
bptest(model2) # it helps
# sometimes it does not help
model3 <- lm(log(wage) ~ educ + exper, data)
bptest(model3)

# 2. robust standard errors
# VCE = variance-covariance estimator
VCE.OLS <- vcov(model1)
VCE.HC1 <- vcovHC(model1, type="HC1")  # Function from {sandwich}

# Extract standard error from VCE
se.OLS <- sqrt(diag(VCE.OLS))
se.HC1 <- sqrt(diag(VCE.HC1))

# Compare standard errors
round(rbind(se.OLS, se.HC1), digits = 6)

# bootstrap standard errors
n <- nobs(model1) # sample size
k <- model1$rank # number of coefficients
B <- 500 # number of bootstrap replications
bs.coeffs <- data.frame("(Intercept)" = NA, "educ" = NA) # store estimates from the bootstrapping

# bootstrapping
for (b in 1:B) {
  bs.model <- lm(wage ~ educ, data, 
                 subset = sample(n, n, replace = TRUE)) 
  bs.coeffs[b, ] <- bs.model$coef 
}

# bootstrap VCE 
VCE.bootstrap <- cov(bs.coeffs)
# Extract standard error from VCE
se.bootstrap <- sqrt(diag(VCE.bootstrap))
# Compare standard errors
round(rbind(se.OLS, se.HC1, se.bootstrap), digits = 6)

# 3. different estimator since the OLS is no longer BLUE: GLS -> WLS (FGLS)
model.aux <- lm(log(resid(model1)^2) ~ educ, data)
weights.FGLS <- 1/exp(fitted(model.aux))
model.FGLS <- lm(wage ~ educ, data, weights = weights.FGLS)
summary(model.FGLS)

# comparison
stargazer(model1,model2,model1,model.FGLS,
          column.labels = c("classical", "different spec", "robust SE", "FGLS"),
          digits=3,
          type="text",
          se = list(summary(model1)$coef[,"Std. Error"],
                    summary(model2)$coef[,"Std. Error"],
                    se.HC1,
                    summary(model.FGLS)$coef[,"Std. Error"])
)

############################# Predictions #############################

# read data_skoda_skoda
data_skoda <- read_csv("week_10_11/data/skoda.csv")

# create dummy variables
data_skoda <- data_skoda %>% 
  mutate(felicia = ifelse(model == 'Felicia', 1, 0)) %>%
  mutate(octavia = ifelse(model == 'Octavia', 1, 0)) %>%
  mutate(superb = ifelse(model == 'Superb', 1, 0)) %>%
  mutate(petrol = ifelse(fuel == 'petrol', 1, 0)) %>%
  mutate(diesel = ifelse(fuel == 'diesel', 1, 0)) %>%
  mutate(autogas = ifelse(fuel == 'autogas', 1, 0))

# Predict the price of a combi Felicia car, which has 100 000 km on the clock,
# the engine 1.9 D and was manufactured in 1998.

### First, let us use the model with the logarithimic price.
# Find the prediction, assuming normally distributed random errors. 

# 1. change the base category by centering:
data_skoda <- data_skoda %>% 
  mutate(combi_1 = combi - 1) %>%
  mutate(km_100000 = km - 100000) %>%
  mutate(year_1998 = year - 1998)

# 2. estimate the regression model
model1 <- lm(log(price) ~ km_100000 + year_1998 + combi_1 +
               octavia + superb + petrol + autogas, data_skoda)
summary(model1)

# 3. the prediction consist of two parts (see the presentation on Predictions)
# part A is the exponentiated intercept
A <- exp(coef(model1)['(Intercept)']) 
# part B assuming normally distributed random errors is calculated as
B.norm <- exp(summary(model1)$sigma^2 / 2) # 'sigma' is a standard error of regression 
paste('the predicted price of the car is', round(A * B.norm), 'CZK')

# Next, use Duan"s estimator instead.
B.Duan <- mean(exp(residuals(model1))) # part B is the mean of the exponentiated residuals
paste('the predicted price of the car is', round(A * B.Duan), 'CZK')
u.hat <- model1$residuals
# Which version is preferred? If your residuals are not normally distributed,
# version 2 is more justified.
hist(residuals(model1))  # Do they look normal?
qqnorm(residuals(model1))  # Comparison with normal quantiles
shapiro.test(u.hat)  # H0: u.hat normally distributed.


### Second, let us use the model with the price without logarithm.
model2 <- lm(price ~ km_100000 + year_1998 + combi_1 +
               octavia + superb + petrol + autogas, data_skoda)
summary(model2)
paste('the predicted price of the car is', round(coef(model2)["(Intercept)"]), 'CZK')

# alternatively
model3 <- lm(price ~ km + year + combi + felicia +
               octavia + petrol + diesel, data_skoda)
coef(model3)
point.prediction <- (coef(model3)["(Intercept)"] + 100000*coef(model3)["km"] + 1998*coef(model3)["year"] +
                       1*coef(model3)["combi"] + 1*coef(model3)["felicia"] + 0*coef(model3)["octavia"] +
                       0*coef(model3)["petrol"] + 1*coef(model3)["diesel"]) %>% unname()
paste('the predicted price of the car is', round(point.prediction), 'CZK')


### 95% CI for a mean in the category of combi Felicia cars 
# (which has 100 000 km on the clock, # the engine 1.9 D and was manufactured in 1998)
# i.e. find the 95% CI for beta0^ (model with the changed base category)
confint(model2, parm = "(Intercept)")

### 95% CI for the price of a particular combi Felicia car
# (which has 100 000 km on the clock, # the engine 1.9 D and was manufactured in 1998)
# i.e. find the so-called prediction interval

# 1. find the standard error of prediction.
se.beta0.hat <- summary(model2)$coef["(Intercept)", "Std. Error"]  # Obtain std. error of theta hat.
sigma.hat <- summary(model2)$sigma  # Obtain sigma hat, estimate of sd(u).
prediction.error <- sqrt(se.theta.hat^2 + sigma.hat^2)  # Calculate pred. error.

# 2. find the 95% prediction interval.
lower <- point.prediction - 1.96 * prediction.error  # Lower bound of 95% pred. interval.
upper <- point.prediction + 1.96 * prediction.error  # Upper bound of 95% pred. interval.
cat(paste("95% prediction interval is (", round(lower), ", ", round(upper), ")", sep = ""))

