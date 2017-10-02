# library(testthat)

context("mbc tests")

test_that("mbc basic runs", {
  expect_error(m1 <- mbc(mean, median, input=rnorm(100)), regexp = NA)
  expect_is(m1, "mbc")
  expect_is(m1, "list")
  # expect_equal(length(m1), 5)

  # Get error when x not specified # No longer an error, actually works
  # expect_error(m1 <- mbc(mean, median, inputi={rnorm(100)}))
  # Test inputi
  expect_error(m1 <- mbc(mean(x), median(x), inputi={x=rnorm(100)}), regexp = NA)
  # Error if both input and inputi given it
  expect_error(m1 <- mbc(mean, median, input=rnorm(100), inputi={rnorm(100)}))
  # Give in names
  expect_error(m1 <- mbc(mean=mean(x), med=median(x), inputi={x=rnorm(100)}), regexp = NA)
  expect_equal(dimnames(m1$Output)[[1]], c("mean", "med"))
  # Give in only one name
  expect_error(m1 <- mbc(mean(x), med=median(x), inputi={x=rnorm(100)}), regexp = NA)
  expect_equal(dimnames(m1$Output)[[1]], c("mean(x)", "med"))

  # Give in evaluator
  expect_error(m1 <- mbc(1, 2, evaluator={.}), regexp = NA)

  # Give in no input
  expect_error(m1 <- mbc(mean(rnorm(10)), median(rnorm(10))), regexp = NA)
  # Give in functions, no input
  expect_error(m1 <- mbc(function(x)mean(rnorm(10)), function(x)median(rnorm(10))), regexp = NA)

  # Try different times
  expect_error(m1 <- mbc(mean(x), median(x), inputi={x=rnorm(100)}, times=1), regexp = NA)
  expect_error(m1 <- mbc(mean(x), median(x), inputi={x=rnorm(100)}, times=2, target=.5), regexp = NA)
  expect_error(m1 <- mbc(mean(x), median(x), inputi={x=rnorm(100)}, times=20), regexp = NA)
  expect_error(m1 <- mbc(mean(x), median(x), inputi={x=rnorm(100)}, times=20, target=.5), regexp = NA)
  expect_error(print(m1), NA)
})
test_that("test mbc print", {
  # Basic with compare
  m1 <- mbc(mean, median, inputi=function(i)rnorm(100))
  expect_error(print(m1), regexp = NA)
})

test_that("test mbc metrics", {

  m1 <- mbc(mean, median, inputi=function(i)rnorm(10))

  m1 <- mbc(mean, median, inputi=function(i)rnorm(10), target=10)

  # Test t and mis90 using lm
  x1 <- runif(10)
  x2 <- runif(10)
  y1 <- x1 * 1.2 + x2 * .43 - .76 + rnorm(10,0,.1)
  xdf <- data.frame(x1=runif(10), x2=runif(10))
  ydf <- with(xdf, x1 * 1.2 + x2 * .43 - .76 + rnorm(10,0,.1))
  # Just run, no compare of output
  expect_error(m1 <- mbc(lm(y1 ~ x1), lm(y1 ~ x1 + x2)), NA)
  expect_error(print(m1), NA)
  # Test target in
  expect_error(mbc(lm(y1 ~ x1), lm(y1 ~ x1 + x2), targetin = xdf, target=ydf), NA)
  # mbc(lm(y1 ~ x1), lm(y1 ~ x1 + x2), targetin = cbind(xdf, ydf), target="ydf")

  m1 <- mbc(lm(y1 ~ x1), lm(y1 ~ x1 + x2), target=ydf, metric="t", post=function(mod){predict(mod, xdf,se=T)})
  expect_true("Mean t" %in% m1$Output_disp$Stat)
  expect_error(print(m1), NA)
  m1 <- mbc(lm(y1 ~ x1), lm(y1 ~ x1 + x2), target=ydf, metric="mis90", post=function(mod){predict(mod, xdf,se=T)})
  expect_true("mis90" %in% m1$Output_disp$Stat)

  # t and mis90
  m1 <- mbc(lm(y1 ~ x1), lm(y1 ~ x1 + x2), target=ydf, metric=c("t","mis90"), post=function(mod){predict(mod, xdf,se=T)})
  expect_true(("mis90" %in% m1$Output_disp$Stat) && "Mean t" %in% m1$Output_disp$Stat)

  m1 <- mbc(lm(y1 ~ x1), lm(y1 ~ x1 + x2), target=ydf, metric=c("rmse","mis90"), post=function(mod){predict(mod, xdf,se=T)})
  expect_true(("mis90" %in% m1$Output_disp$Stat) && "rmse" %in% m1$Output_disp$Stat)

})

test_that("mbc plot", {
  m1 <- mbc(mn={Sys.sleep(rexp(1,10));mean(x)},med= {Sys.sleep(rexp(1,20));median(x)}, inputi={x=rnorm(100)}, target=0)
  expect_error(plot(m1), NA)
})

# test_that("GauPro", {
#   # Trying to figure out better way to get input
#   m1 <- mbc(GauPro::Gaussian, GauPro::Matern52, times=2,
#       evaluator=GauPro::GauPro_kernel_model$new(X=x, Z=y, kernel=.)$predict(xp, se=T),
#       inputi={xp <- matrix(3,1,2)}, target=yp, metric='t')
#   expect_is(m1, "mbc")
# })
