library(dplyr)
library(readr)
library(ggplot2)
library(GGally)
library(modelr)

# data <- read_csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
data <- read_csv("week_10_11/data/credit_scoring_sample.csv")
ggpairs(data)

head(data)

data %>% 
  ggplot(aes(NumberOfDependents, SeriousDlqin2yrs))+
  geom_point(colour = "steelblue")+
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"))

# train-test split --------------------------------------------------------

set.seed(123) # For reproducibility
splits <- data %>%
  modelr::resample_partition(c(train = 0.6, test = 0.2, validattion=0.2))

train_data <- as_tibble(splits$train)
test_data <- as_tibble(splits$test)


# LPM ---------------------------------------------------------------------

model_lpm <- lm(SeriousDlqin2yrs ~ ., data=train_data)
summary(model_lpm)

library(sandwich)
library(lmtest)

bptest(model_lpm) # obvious result

robust_VCE <- vcovHC(model_lpm, type="HC2")  # Robustn? odhad matice VCE.
robust_se <- sqrt(diag(robust_VCE))  # Robustn? sm. chyby.
robust_se

summary_lpm <- summary(model_lpm)
summary_lpm$coefficients[,2] <- robust_se
summary_lpm

model_logit <- glm(SeriousDlqin2yrs ~ .,data=train_data, family=binomial(link='logit'))

library(caret)
library(pROC)



# Create predictions ------------------------------------------------------

lpm_prob <- predict(model_lpm, newdata = test_data)

logit_prob <- predict(model_logit, newdata = test_data, type = "response")

threshold <- 0.5

# Convert probabilities to binary predictions
lpm_pred <- ifelse(lpm_prob > threshold, 1, 0)
logit_pred <- ifelse(logit_prob > threshold, 1, 0)

# Actual target values
actual_values <- test_data$SeriousDlqin2yrs

# Confusion matrices
lpm_cm <- confusionMatrix(as.factor(lpm_pred), as.factor(actual_values))
logit_cm <- confusionMatrix(as.factor(logit_pred), as.factor(actual_values))

# Model evaluation metrics
lpm_metrics <- lpm_cm$overall
logit_metrics <- logit_cm$overall


# ROC curve ---------------------------------------------------------------
#The Receiver Operating Characteristic (ROC)

lpm_roc <- roc(response = as.numeric(actual_values), predictor = lpm_prob)
logit_roc <- roc(response = as.numeric(actual_values), predictor = logit_prob)

lpm_auc <- auc(lpm_roc)
logit_auc <- auc(logit_roc)


# Plot ROC curves
plot(logit_roc, main = "ROC Curves", col = "blue", lwd = 2)
lines(lpm_roc, col = "red", lwd = 2)
legend("bottomright", legend = c("Logit", "LPM"), col = c("blue", "red"), lwd = 2)

# Print AUC-ROC values
cat("LPM AUC-ROC:", lpm_auc, "\n")
cat("Logit AUC-ROC:", logit_auc, "\n")
# AUC of 0.5 says the model is no better then distinguishing betweeen the classes by chance

# calculating partial effects ---------------------------------------------
library(mfx)
options(scipen = 999)

# Marginal effects at the mean (MEM)
logitMEM <- logitmfx(SeriousDlqin2yrs ~ .,data=train_data, atmean = TRUE)

# Print the MEM results
print(logitMEM)

# Average marginal effects (AME)
logitAME <- logitmfx(SeriousDlqin2yrs ~ .,data=train_data, atmean = FALSE)

# Print the AME results
print(logitAME)
