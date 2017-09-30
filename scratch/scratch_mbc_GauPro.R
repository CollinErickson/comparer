library(GauPro)
n <- 50
d <- 2
f <- TestFunctions::banana
x <- matrix(runif(n*d), n,d)
y <- f(x)
nn <- 300
xx <- matrix(runif(nn*d), nn,d)
yy <- f(xx)
# Single input
mbc(Gaussian, Matern32, Matern52, evaluator={GauPro_kernel_model$new(X=x,Z=y,kernel=.)$predict(xx)}, target=yy)
# Redo input
mbc(Gaussian, Matern32, Matern52, inputi={x <- matrix(runif(n*d), n,d);y <- f(x)}, evaluator={GauPro_kernel_model$new(X=x,Z=y,kernel=.)$predict(xx)}, target=yy)
# Call predict in post
mbc(Gaussian, Matern32, Matern52, inputi={x <- matrix(runif(n*d), n,d);y <- f(x)}, evaluator={GauPro_kernel_model$new(X=x,Z=y,kernel=.)}, post=function(x)x$predict(xx), target=yy)
# Use mis90 as metric
mbc(Gaussian, Matern32, Matern52, inputi={x <- matrix(runif(n*d), n,d);y <- f(x)}, evaluator={GauPro_kernel_model$new(X=x,Z=y,kernel=.)}, post=function(x)x$predict(xx,se=T), target=yy, metric="mis90")
