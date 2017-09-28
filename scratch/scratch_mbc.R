# Try linear model
n <- 10
d <- 2
x <- matrix(runif(n*d), ncol=d)
b <- runif(d)
y <- x %*% b
input <- data.frame(x, y)
# Single LM repeated 5 times
mbc(lm=function(X) {lm(y ~ X1 + X2, data=X)}, input=input, post=function(mod) {sum((mod$coef[2:3]-b)^2)})
# Compare to LM that adds noise to y
mbc(lm1=function(X) {lm(y ~ X1 + X2, data=X)},
    lm2=function(X) {X$y <- X$y + runif(length(y),0,0.1);lm(y ~ X1 + X2, data=X)},
    input=input, post=function(mod) {sum((mod$coef[2:3]-b)^2)})

# Test predictions
xpred <- matrix(runif(n*d), ncol=d)
ypred <- xpred %*% b + rnorm(n, 0, .1)
xpreddf <- as.data.frame(xpred)
colnames(xpreddf) <- c("X1", "X2")

mbc(lm1=function(X) {lm(y ~ X1 + X2, data=X)},
    lm2=function(X) {X$y <- X$y + runif(length(y),0,0.1);lm(y ~ X1 + X2, data=X)},
    input=input, post=function(mod) {mean((predict(mod,xpreddf) - ypred)^2)})

mbc(lm1=function(X) {lm(y ~ X1 + X2, data=X)},
    lm3= . %>% lm(y ~ X1 + X2, data=.),
    lm2=function(X) {X$y <- X$y + runif(length(y),0,0.1);lm(y ~ X1 + X2, data=X)},
    input=input, post=function(mod) {mean((predict(mod,xpreddf) - ypred)^2)})


# Check coeffs with target
mbc(lm=function(X) {lm(y ~ X1 + X2, data=X)$coeff}, input=input)
mbc(lm=function(X) {lm(y ~ X1 + X2, data=X)$coeff}, input=input, target=c(0,b))
mbc(lm1=function(X) {lm(y ~ X1 + X2, data=X)$coeff},
    lm2=function(X) {X$y <- X$y + runif(length(y),0,0.1);lm(y ~ X1 + X2, data=X)$coeff},
    input=input, target=c(0,b))

# Check logical
mbc(function(){sample(c(T,F), 5, replace=T)})

# Give target input
mbc(lm=function(X) {lm(y ~ X1 + X2, data=X)$coeff}, input=input, target=function(i){c(0,b)})
mbc(lm=function(X) {lm(y ~ X1 + X2, data=X)$coeff}, input=input, target=list(c(0,b),c(0,b)+1,c(0,b)+2,c(0,b)+1,c(0,b)))

# Try inputi so paired and not sorted
mbc(lm1=function(X) {lm(y ~ X1 + X2, data=X)$coeff},
    lm2=function(X) {X$y <- X$y + runif(length(y),0,0.1);lm(y ~ X1 + X2, data=X)$coeff},
    inputi=function(i){ii <- input; ii$y <- ii$y + i; ii}, target=c(0,b))
mbc(lm1=function(X) {lm(y ~ X1 + X2, data=X)$coeff},
    lm2=function(X) {X$y <- X$y + runif(length(y),0,0.1);lm(y ~ X1 + X2, data=X)$coeff},
    lm3=function(X) {X$y <- X$y + runif(length(y),0,1);lm(y ~ X1 + X2, data=X)$coeff},
    inputi=function(i){ii <- input; ii$y <- ii$y + i; ii}, target=c(0,b))


debugonce(mbc)
