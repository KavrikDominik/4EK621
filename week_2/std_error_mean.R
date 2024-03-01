library(dplyr)

# generate_data
N <- 10e6
y <- rnorm(N, 5, 1) # 5 =true popul. mean
popul <- data.frame(y)
set.seed(123)
n <- 100
smpl <- sample_n(popul, n)
# true popul. mean
mean(popul$y)

# standard error of mean using formula
SEM <- sd(smpl$y) / sqrt( nrow(smpl) )
SEM

# initialize means tibble to store iteration and sample means
means <- tibble(idx = numeric(),
                mean = numeric())

# repeat sampling 500x and save means
for (i in 1:1000){
  smpl <- sample_n(popul, n)
  means[i,1] <- i
  means[i,2] <- mean(smpl$y)
  print(i)
}

means$mean %>%
  hist()

# standard error of mean
sd(means$mean)
