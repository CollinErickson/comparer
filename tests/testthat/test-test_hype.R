library(testthat)
test_that("hype works", {
  # Create params
  p1 <- par_hype$new()
  expect_true("par_hype" %in% class(p1))
  p1 <- par_unif$new("a", -1, 1)
  expect_true("par_unif" %in% class(p1))
  plog <- par_log10$new("a", 1e-8, 1)
  expect_true("par_log10" %in% class(plog))

  # Create hype
  h1 <- hype$new(eval_func = function(a) {a^2}, p1, n_lhs=3)
  # Check basics
  expect_true("hype" %in% class(h1))
  # expect_equal(nrow(h1$X), 3)
  expect_true(is.null(h1$X))
  expect_equal(nrow(h1$ffexp$rungrid2()), 3)
  expect_equal(length(h1$Z), 0)
  expect_error(h1$run_all(), NA)
  expect_equal(length(h1$Z), 3)

  # Check add
  expect_error(h1$add_EI(1), NA)
  expect_error(h1$add_EI(1), NA)
  expect_error(h1$run_all(), NA)
  expect_error(h1$run_EI_for_time(1, 1), NA)

  # Check plots
  expect_error(plotorder <- h1$plotorder(), NA)
  expect_is(plotorder, 'ggplot')
  expect_error(plotX <- h1$plotX(), NA)
  expect_is(plotX, 'ggplot')
  expect_error(plotplot <- plot(h1), NA)
  expect_is(plotplot, 'ggplot')

  # Two inputs
  expect_error({
    h2 <- hype$new(eval_func = function(a, b) {a^2 - sin(2*pi*b)}, p1, par_unif$new("b", -1,1), n_lhs=3)
  }, NA)
  expect_error(h2$run_all(), NA)
  expect_error(h2$add_EI(n=3), NA)
  expect_error(h2$run_all(), NA)
  # Add LHS
  expect_error(h2$add_LHS(n=3), NA)
  expect_error(h2$run_all(), NA)
  # Plot interaction
  expect_error(plotint <- h2$plotinteractions(), NA)
  expect_is(plotint, 'ggplot')
  # Plot pairs
  expect_error(plotpairs <- h2$pairs(), NA)
  expect_is(plotpairs, 'ggplot')
  # Print object
  expect_error(printout <- capture.output(print(h2), NA))
  printout <- capture.output(print(h2))
  expect_true(is.character(printout), length(printout) >= 1)

  # Break if no params
  expect_error(hype$new(eval_func = function(a) {a^2}, n_lhs=3))
  # Break if not a param
  expect_error(hype$new(eval_func = function(a) {a^2}, sin, n_lhs=3))
  # Break if X0 given
  expect_error(hype$new(eval_func = function(a) {a^2}, par_unif$new('a',1,3), X0=matrix(1:3, ncol=1), n_lhs=3))
  # Break if n_lhs not given
  expect_error(hype$new(eval_func = function(a) {a^2}, par_unif$new('a',1,3)))

})
