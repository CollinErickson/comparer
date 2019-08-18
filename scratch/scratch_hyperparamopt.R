# HYPE
n <- 100
x <- runif(n)
y <- .6*x+.4 + rnorm(n,0,.0001)
plot(x,y)
ef <- function(m,b) {
  sqrt(mean((y - m*x-b)^2))
}
library(ContourFunctions)
gcf(function(x) {ef(x[1], x[2])})

nn <- 10
X <- matrix(runif(2*nn), ncol=2)
Y <- sapply(1:nn, function(i) {ef(X[i,1], X[i,2])})

for (i in 1:10) {
  mod <- DiceKriging::km(design=X, response=Y, covtype="matern5_2")
  predict(mod, matrix(c(.2,.2), ncol=2), type='UK', checkNames=F)
  gcf(function(x) {predict(mod, matrix(x, ncol=2), type='UK', checkNames=F)$mean}, batchmax=Inf, pts=X)
  # gcf(function(x) {predict(mod, matrix(x, ncol=2), type='UK', checkNames=F)$sd}, batchmax=Inf, pts=X)
  newX <- DiceOptim::max_EI(mod, lower=c(0,0), upper=c(1,1))
  newY <- ef(newX$par[1], newX$par[2])
  X <- rbind(X, newX$par)
  Y <- c(Y, newY)
}
