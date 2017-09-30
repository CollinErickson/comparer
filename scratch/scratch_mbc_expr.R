# Try linear model
n <- 40
d <- 2
x <- matrix(runif(n*d), ncol=d)
b <- runif(d)
y <- x %*% b + rnorm(n+1,0,.01)
ynoise <- y + rnorm(n)
input <- data.frame(x, y, yn=ynoise)
# Single LM repeated 5 times
mbc(lm=lm(y ~ X1 + X2), input=input, post=function(mod) {sum((mod$coef[2:3]-b)^2)})
# Compare to LM that adds noise to y
mbc(lm1=lm(y ~ X1 + X2),
    lm2={y <- y+10; lm(y ~ X1 + X2)},
    input=input, post=function(mod) {sum((mod$coef[2:3]-b)^2);mod$coef[1]})


mbc(lm=predict(lm(ynoise ~ X1 + X2),input), input=input, target='yn')

# Create test data

xp <- matrix(runif(n*d+d), ncol=d)
yp <- xp %*% b + rnorm(n+1,0,.01)
dfp <- data.frame(X1=xp[,1], X2=xp[,2], y=yp)
mbc(lm=lm(ynoise ~ X1 + X2), input=input, target=yp, targetin=data.frame(X1=xp[,1], X2=xp[,2]))

# Just run 1
mbc(GauPro::GauPro_kernel_model$new(X=x, Z=y, kernel=GauPro::Gaussian))
# Compared predictions of 2
mbc(GauPro::GauPro_kernel_model$new(X=x, Z=y, kernel=GauPro::Gaussian)$predict(xp), GauPro::GauPro_kernel_model$new(X=x, Z=y, kernel=GauPro::Matern52)$predict(xp), times=10, target=yp)
# Use evaluator to shorten
mbc(GauPro::Gaussian, GauPro::Matern52, times=10, target=yp, evaluator=GauPro::GauPro_kernel_model$new(X=x, Z=y, kernel=.)$predict(xp), input=list(xp=xp))

# Use different data
n <- 40
d <- 2
x <- matrix(runif(n*d), ncol=d)
f <- TestFunctions::banana
y <- f(x)
xp <- matrix(runif(n*d+d), ncol=d)
yp <- f(xp)
# Use evaluator to shorten
mbc(GauPro::Gaussian, GauPro::Matern52, times=2, target=yp, evaluator=GauPro::GauPro_kernel_model$new(X=x, Z=y, kernel=.)$predict(xp, se=T), input=list(xp=xp), metric='t')
# multiple metrics
mbc(GauPro::Gaussian, GauPro::Matern52, times=2, target=yp, evaluator=GauPro::GauPro_kernel_model$new(X=x, Z=y, kernel=.)$predict(xp, se=T), input=list(xp=xp), metric=c("t",'mis90'))

# Trying to figure out better way to get input
mbc(GauPro::Gaussian, GauPro::Matern52, times=2,
    evaluator=GauPro::GauPro_kernel_model$new(X=x, Z=y, kernel=.)$predict(xp, se=T),
    input={x <- 1:n; y <- sin(x)},
    input=list(xp=xp), target=yp, metric='t')
