library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(gganimate)

# Create the population data ----------------------------------------------
N <- 10^4 # N = population size.
pop <- data.frame(
  height = rnorm(N, 175, 10),
  u = rnorm(N, 0, 100)
) %>%
  mutate(wage = 5 + 10 * height + u) %>%
  mutate(irrelevant = rnorm(N, 10, 3))

ggplot(aes(y = wage, x = height), data = pop) +
  geom_point(alpha = 0.1) +
  geom_abline(intercept = 5, slope = 10, colour = "dodgerblue2", lwd = 1.5)

# Initialize
n <- 1e2 # n = sample size.
R <- 1e3 # R = number of replications.
beta.0 <- rep(NA, R) # Vector of beta 0 estimates.
beta.1 <- rep(NA, R) # Vector of beta 1 estimates.
results <- data.frame(beta.0, beta.1)

# one sample
sample1 <-
  pop %>%
  slice_sample(n = 100, replace = TRUE) # Choose 100 observations at random
model1 <- lm(wage ~ height, data = sample1)
summary(model1)

ggplot(aes(y = wage, x = height), data = pop) +
  geom_point(lwd = 0.5, alpha = 0.1) +
  geom_abline(intercept = 5, slope = 10, lwd = 1.5) +
  geom_point(aes(y = wage, x = height), data = sample1, colour = "red", shape = 2, lwd = 2) +
  geom_abline(intercept = coef(model1)[1], slope = coef(model1)[2], colour = "red2", lwd = 1.5)

# confidence intervals
confint(model1) # 95% CI
confint(model1, level = 0.99) # 99% CI

# manually
data.frame(
  LB = summary(model1)$coefficients[, 1] - qt(0.975, summary(model1)$df[2]) * summary(model1)$coefficients[, 2],
  UB = summary(model1)$coefficients[, 1] + qt(0.975, summary(model1)$df[2]) * summary(model1)$coefficients[, 2]
)

# approx.
qt(0.975, summary(model1)$df[2])
data.frame(
  LB = summary(model1)$coefficients[, 1] - 2 * summary(model1)$coefficients[, 2],
  UB = summary(model1)$coefficients[, 1] + 2 * summary(model1)$coefficients[, 2]
)

for (r in 1:R) { # For each replication in i = 1 to R:
  OLS <- lm(
    wage ~ height,
    data = pop,
    subset = sample(N, n, replace = TRUE)
  )
  results$beta.0[r] <- OLS$coef[1] # Save the estimated parameters.
  results$beta.1[r] <- OLS$coef[2]
  results$LB[r] <- results$beta.1[r] - qt(0.975, n - 2) * summary(OLS)$coefficients[2, 2]
  results$UB[r] <- results$beta.1[r] + qt(0.975, n - 2) * summary(OLS)$coefficients[2, 2]
}

# Resampling for vizualization --------------------------------------------
# using purrr package...

num_samples <- R


# Create nested tibble with R samples -------------------------------------

samples <- tibble(
  id = 1:num_samples,
  data = map(
    1:num_samples,
    ~ pop %>% sample_n(n, replace = TRUE)
  )
)
samples

# Save data and coefficients for wage ~ height ----------------------------
coef_data_1 <- samples %>%
  mutate(
    models = map(data, ~ lm(wage ~ height, data = .)),
    coefficients = map(models, broom::tidy)
  ) %>%
  select(id, coefficients) %>% 
  unnest(coefficients)

head(coef_data_1)



# add 95% confidence interval:

coef_data_1 %>%
  head(100) %>% 
  mutate(lower = estimate - qt(0.975, n - 2) * std.error) %>% 
  mutate(upper = estimate + qt(0.975, n - 2) * std.error) %>% 
  ggplot(aes(x= estimate, y = id, color=term)) +
  geom_point(aes(x= estimate, color = term)) +
  geom_errorbar(aes(xmin = lower, xmax=upper))+
  facet_wrap(~term, scales="free")

# %>%
#   mutate(coefficients = map(coefficients, ~ .x %>%
#                               select(term, estimate) %>%
#                               pivot_wider(names_from = "term", values_from = "estimate")))


# Save data with coefficients with irrelevant regressor -------------------
coef_data_model2 <- samples %>%
  mutate(
    models = map(data, ~ lm(wage ~ height + irrelevant, data = .)),
    coefficients = map(models, broom::tidy)
  )  %>% select(id, data, coefficients) %>% 
  unnest(coefficients)

coef_data_model2

gg_ci <- coef_data_model2 %>% 
  head(300) %>% 
  mutate(lower = estimate - qt(0.975, n - 2) * std.error) %>% 
  mutate(upper = estimate + qt(0.975, n - 2) * std.error) %>% 
  ggplot(aes(x= estimate, y = id, color=term)) +
  geom_point(aes(x= estimate, color = term)) +
  geom_errorbar(aes(xmin = lower, xmax=upper))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  facet_wrap(~term, scales="free")+
  theme_light()

gg_ci
# Lets investigate the false positives when testing significance o --------

t_false_positives <- coef_data_model2 %>% 
  mutate(lower = estimate - qt(0.975, n - 2) * std.error) %>% 
  mutate(upper = estimate + qt(0.975, n - 2) * std.error) %>% 
  mutate(incl_zero = ifelse(lower<=0&upper >=0, "yes","no")) %>% 
  group_by(term, incl_zero) %>% 
  summarise(n = n()) %>% 
  group_by(term) %>% # now group by only by term so the frequency is correct...
  mutate(freq = n / sum(n)) # ...see the sum(n) in denominator that§s why...
t_false_positives
# try increasing the number in head()

# ggplots of t-distributions ----------------------------------------------
t_dist <- coef_data_model2 %>%
  select(id, data, coefficients) %>%
  select(id, coefficients) %>%
  unnest(coefficients) %>%
  filter(!term == "(Intercept)") %>%
  select(id, term, statistic) %>%
  ggplot(aes(statistic, fill = term)) +
  geom_histogram(aes(y = ..density..), color = "white") +
  stat_function(
    fun = dt, args = list(df = 98),
    aes(color = "t_distribution"),
    size = 1.1
  ) +
  scale_color_manual(values = c("black", "black", "black")) +
  scale_fill_manual(values = c("red2", "dodgerblue3")) +
  theme_light() +
  labs(
    title = "Distribution of t-statistic for height and irrelevant",
    subtitle = "Null hypothesis: i-th parameter is insignificant; alpha = 0.05",
    caption = paste("Under the null hypothesis, t-statistic is distributed t [n-k-1]")
  )

t_dist

# mutate(plot = map2(data,coefficients, ~ ggplot(aes(x=height, y=wage), data = .x ) +
#                      geom_point() +
#                      geom_abline(data = .y, aes(intercept = `(Intercept)`, slope = height))))

# Resampling animation ----------------------------------------------------
coef_data <- samples %>%
  mutate(
    models = map(data, ~ lm(wage ~ height, data = .)),
    coefficients = map(models, broom::tidy)
  ) %>%
  select(id, data, coefficients) %>%
  mutate(coefficients = map(coefficients, ~ .x %>%
    select(term, estimate) %>%
    pivot_wider(names_from = "term", values_from = "estimate")))

require(gganimate)
anim <- coef_data %>%
  head(10) %>%
  unnest(coefficients) %>%
  rename(
    reg_height = height,
    Intercept = `(Intercept)`
  ) %>%
  unnest(data) %>%
  group_by(id) %>%
  ggplot(aes(height, wage)) +
  geom_point(data = pop, aes(height, wage), alpha = 0.1, shape = "o") +
  geom_abline(intercept = 5, slope = 10, colour = "black", linetype = "dashed", lwd = 0.5) +
  geom_point(color = "orange", size = 3) +
  geom_abline(aes(intercept = Intercept, slope = reg_height), color = "dodgerblue3", lwd = 2) +
  theme_light() +
  coord_cartesian(xlim = c(150, 200), ylim = c(1300, 2100)) +
  transition_time(id) +
  ease_aes("exponential-in") +
  labs(title = "Sampling trial: {frame_time}")


# Save GIF ----------------------------------------------------------------
animate(anim, duration = 10, width = 800, height = 800, renderer = gifski_renderer())
anim_save("week_5/goo.gif")

# Rendering animations ----------------------------------------------------

rmarkdown::render(
  input = "week_5/test.Rmd",
  output_dir = "week_5/outputs",
  output_file = "test.html"
)

# Rendering html dashboard ------------------------------------------------

rmarkdown::render(
  input = "week_5/dashboard.Rmd",
  output_dir = "week_5/outputs",
  output_file = "dashboard.html"
)


# for 95% samples, the true parameter beta1 = 10 lies within the 95% confidence interval
sum((results$LB < 10) & (results$UB > 10)) / R * 100
