test_that("hype works", {
  p1 <- par_hype$new()
  expect_true("par_hype" %in% class(p1))
  p1 <- par_unif$new("a", -1, 1)
  expect_true("par_unif" %in% class(p1))

  h1 <- hype$new(eval_func = function(x) {x^2}, p1, n_lhs=3)
  expect_true("hype" %in% class(h1))
  expect_equal(nrow(h1$X), 3)
  expect_equal(length(h1$Z), 0)
  expect_error(h1$run_all(), NA)
  expect_equal(length(h1$Z), 3)
})