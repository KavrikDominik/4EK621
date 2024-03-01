set.seed(123)
muX <- 0
muY <- 0
sdX <- 1
sdY <- 1

rho <- 0.66
N <- 100000

makeData <- function(rho, N, muX=0, muY=0, sdX =1, sdY=1){
  Z <- rnorm(N)
  X <- muX+sdX*Z
  Y <- muY + sdY*(rho*Z + sqrt(1-rho^2)*rnorm(N))
  return(tibble(X,Y))
  
}
rho=0.95
population <- makeData(rho, N)


sample <- sample_n(population, 3000)
sample

sample %>% 
  ggplot(aes(X,Y))+
  geom_point()+
  geom_smooth(method = "lm")

x <- sample$X
y <- sample$Y

r1 <- cov(x,y)/sd(x)*sd(y)
r1
lm(y~x)

slope_param <- rho*sd(y)/sd(x)
slope_param

