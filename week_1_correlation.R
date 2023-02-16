library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

data <- readr::read_csv("week_1/data/starbucks-menu-nutrition-food.csv",
                        locale = readr::locale(encoding = "UTF-16"))

colnames(data)

data <- data %>%
  rename(Product = ...1)

data %>% 
  head()

num_vars <- data %>%
  dplyr::select(where(is.numeric))

corrplot_input <- cor(num_vars) 

corrplot::corrplot(corrplot_input, method = "number")


data %>% 
  select(Calories, `Protein (g)`) %>% 
  ggplot()+
  geom_point(aes(`Protein (g)`, Calories))

data %>% 
  pivot_longer(names_to = "variable",
               values_to = "value", 3:ncol(.)) %>% 
  ggplot(aes(value, Calories)) +
  geom_point(aes(colour = variable))+
  facet_wrap(~variable, scales = "fixed")

library(manipulate)

makeData <- function(theta, N){
  set.seed(123)
  X = rnorm(N, 2, 1)
  Y = theta*X + 2 + rnorm(N,0,1)
  return(tibble(X,Y))
}

manipulate(plot(
  makeData(theta = th, 1000),
  main = paste(
    "Correlation coefficient: ",
    round(cor(
      makeData(theta = th, 100)
    )[2])
  ),
  xlim = c(0, 8),
  ylim = c(0, 10)
),
th = slider(0.05, 2, 0.05)
)

