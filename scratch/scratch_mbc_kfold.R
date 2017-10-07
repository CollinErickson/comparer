mbc(mean(x), median(x), inputi={x <- (1:10)[ki]}, kfold=2, kfoldN=10)
aa <- 1:10
bb <- aa*1.8 + 10
mbc(lm(y ~ x - 1), lm(y~x), inputi={x <- aa[ki];y <- bb[ki]}, kfold=5, kfoldN=10)
mbc(lm(y ~ x - 1), lm(y~x), inputi={x <- aa[ki];y <- bb[ki]}, kfold=5, kfoldN=10, targetin = {data.frame(x=aa,y=bb)[-ki,]}, target='y')
