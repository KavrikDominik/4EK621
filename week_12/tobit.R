require(VGAM)
library(readr)
library(GGally)

dat <- read_csv("https://stats.idre.ucla.edu/stat/data/tobit.csv")
summary(dat)