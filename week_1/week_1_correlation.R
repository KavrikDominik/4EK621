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


muX = 0
muY = 0
sdX = 1
sdY = 1

makeData <- function(theta, N){
  set.seed(123)
  Z = rnorm(N)
  X = muX + sdX*Z
  Y = muY + sdY*(theta*Z + sqrt(1-theta^2)*rnorm(N))
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
  xlim = c(-4, 4),
  ylim = c(-4, 5)
),
th = slider(0, 1, 0.01)
)
