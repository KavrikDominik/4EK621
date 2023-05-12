# settings
options(scipen=10) # displays 0.000513 instead of 5.13e-04 

# libraries
library(dplyr)
library(margins)  # marginal effects
library(AER)      # tobit function

## read the data
data <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

## view the first few rows of the data
head(data)
# variables:
# admit: admission into graduate school
# gre: Graduate Record Exam scores
# gpa: grade point average
# rank: prestige of the undergraduate institution

## rank as factor
data <- data %>% 
  mutate(rank = as.factor(
    ifelse(rank == 1, "high", 
           ifelse(rank == 2, "moderate",
                  ifelse(rank == 3, "low", "poor")))))

## descriptive statistics
summary(data)

## 1. Run a logistic regression that models the probability of 
# the admission into graduate school. 
m1 <- glm(admit ~ gre + gpa + rank, data = data, family = binomial)
summary(m1) #The ?j coefficient is roughly the relative change of the odds ratio due to a unit change in xj.
# GRE and GPA being held constant, what is the expected difference 
# in the odds for undergraduate institution with (i) moderate prestige? 
exp(round(coef(m1)["rankmoderate"],4) )-1
# And (ii) low prestige?
round(coef(m1)["ranklow"],4)

## 2. Using the model estimated in the previous task, 
# use a likelihood ratio (LR) test to address the question: 
# does the prestige of the undergraduate institution affect 
# the probability of admission? Use the conventional 5% level of significance.
m2 <- glm(admit ~ gre + gpa, data = data, family = binomial)
p <- pchisq(2 * (logLik(m1) - logLik(m2)), df = 3, lower.tail = FALSE)
print(paste0("LR test for joint significance of rank dummies: p = ", round(p, 4)))


anova(m1, m2, test = "Chisq")

## 3. Using the same model again, calculate the average marginal 
# effect (AME) of GPA. Next, report the 95% confidence interval  
# for the AME based on a standard error obtained by (i) the Delta method.
summary(margins(m1))
# And (ii) 1,000 replications of non-parametric bootstrap.
summary(margins(m1, vce = "bootstrap", iterations = 1000))

## 4. This time, use the Graduate Record Exam scores (gre) 
# as the dependent variable in the Tobit model. 
# What is the marginal effect of grade point average (gpa)
# on the Graduate Record Exam scores for a student with gpa = 3.5 
# of the undergraduate institution with high prestige?
summary(data$gre)
m3 <- tobit(gre ~ gpa + rank, data = data, x = TRUE)
summary(m3)
sigma <- m3$scale
Phi <- pnorm(c(1, 3.5, 0, 0, 0) %*% m3$coef / sigma)
sprintf("ME = %5.3f X coefficients", Phi)
Phi * m3$coef["gpa"]

## 5. Use Poisson regression to see how your explanatory variables gpa and rank
# affect the number of points of Graduate Record Exam (gre). 
# Other explanatory variables being held constant, by how
# much is the number of points expected to increase with the gpa?
m4 <- glm(gre ~ gpa + rank, data = data, family = poisson)
summary(m4)
round(coef(m4)["gpa"],4) #100*?j is roughly the percentage change in E(y|x), given a one-unit increase in xj
