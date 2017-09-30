context("mbc tests")

test_that("mbc basic runs", {
  expect_error(m1 <- mbc(mean, median, input=rnorm(100)), regexp = NA)
  expect_is(m1, "mbc")
  expect_is(m1, "list")
  expect_equal(length(m1), 4)

  # Get error when x not specified
  expect_error(m1 <- mbc(mean, median, inputi={rnorm(100)}))
  # Test inputi
  expect_error(m1 <- mbc(mean(x), median(x), inputi={x=rnorm(100)}), regexp = NA)
  # Error if both input and inputi given it
  expect_error(m1 <- mbc(mean, median, input=rnorm(100), inputi={rnorm(100)}))
  # Give in names
  expect_error(m1 <- mbc(mean=mean(x), med=median(x), inputi={x=rnorm(100)}), regexp = NA)
  expect_equal(dimnames(m1$Output)[[1]], c("mean", "med"))
  # Give in only one name
  expect_error(m1 <- mbc(mean(x), med=median(x), inputi={x=rnorm(100)}), regexp = NA)
  expect_equal(dimnames(m1$Output)[[1]], c("f1", "med"))

  # Give in evaluator
  expect_error(m1 <- mbc(1, 2, evaluator={.}), regexp = NA)

  # Give in no input
  expect_error(m1 <- mbc(mean(rnorm(10)), median(rnorm(10))), regexp = NA)
  # Give in functions, no input
  expect_error(m1 <- mbc(function(x)mean(rnorm(10)), function(x)median(rnorm(10))), regexp = NA)

  # Try different times
  expect_error(m1 <- mbc(mean(x), median(x), inputi={x=rnorm(100)}, times=1), regexp = NA)
  expect_error(m1 <- mbc(mean(x), median(x), inputi={x=rnorm(100)}, times=20), regexp = NA)
})
test_that("test mbc print", {
  # Basic with compare
  m1 <- mbc(mean, median, inputi=function(i)rnorm(100))
  expect_error(print(m1), regexp = NA)
})

test_that("test mbc metrics", {

  m1 <- mbc(mean, median, inputi=function(i)rnorm(10))
})

# test_that("GauPro", {
#   # Trying to figure out better way to get input
#   m1 <- mbc(GauPro::Gaussian, GauPro::Matern52, times=2,
#       evaluator=GauPro::GauPro_kernel_model$new(X=x, Z=y, kernel=.)$predict(xp, se=T),
#       inputi={xp <- matrix(3,1,2)}, target=yp, metric='t')
#   expect_is(m1, "mbc")
# })
