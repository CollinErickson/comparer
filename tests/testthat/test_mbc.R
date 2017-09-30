context("mbc tests")

test_that("mbc basic runs", {
  m1 <- mbc(mean, median, input=rnorm(100))
  expect_is(m1, "mbc")
  expect_is(m1, "list")
  expect_equal(length(m1), 4)
})

# test_that("GauPro", {
#   # Trying to figure out better way to get input
#   m1 <- mbc(GauPro::Gaussian, GauPro::Matern52, times=2,
#       evaluator=GauPro::GauPro_kernel_model$new(X=x, Z=y, kernel=.)$predict(xp, se=T),
#       inputi={xp <- matrix(3,1,2)}, target=yp, metric='t')
#   expect_is(m1, "mbc")
# })
