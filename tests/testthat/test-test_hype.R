library(testthat)

# Hype basics ----
test_that("hype works", {
  # Create params
  p1 <- par_hype$new()
  expect_true("par_hype" %in% class(p1))
  p1 <- par_unif("a", -1, 1)
  expect_true("par_unif" %in% class(p1))
  plog <- par_log10("a", 1e-8, 1)
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

  # Check add EI
  expect_error(h1$add_EI(1), NA)
  expect_error(h1$add_EI(1), NA)
  expect_error(h1$run_all(), NA)
  expect_error({h1$run_EI_for_time(1, 1)}, NA)

  # Check plots
  expect_error(plotorder <- h1$plotorder(), NA)
  expect_is(plotorder, 'ggplot')
  expect_error(plotX <- h1$plotX(), NA)
  expect_is(plotX, 'ggplot')
  expect_error(plotplot <- plot(h1), NA)
  expect_is(plotplot, 'ggplot')
  expect_error(plotXorder <- h1$plotXorder(), NA)
  expect_is(plotXorder, 'ggplot')
  rm(h1)

  # Two inputs
  expect_error({
    h2 <- hype$new(eval_func = function(a, b) {a^2 - sin(2*pi*b)},
                   p1,
                   par_unif("b", -1,1),
                   n_lhs=3)
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
  expect_error(printout <- capture.output(print(h2)), NA)
  printout <- capture.output(print(h2))
  expect_true(is.character(printout), length(printout) >= 1)

  # Break if no params
  expect_error(hype$new(eval_func = function(a) {a^2}, n_lhs=3))
  # Break if not a param
  expect_error(hype$new(eval_func = function(a) {a^2}, sin, n_lhs=3))
  # Break if X0 given
  expect_error(hype$new(eval_func = function(a) {a^2}, par_unif('a',1,3), X0=matrix(1:3, ncol=1), n_lhs=3))
  # Break if n_lhs not given
  expect_error(hype$new(eval_func = function(a) {a^2}, par_unif('a',1,3)))

})


# Hype add data ----
test_that("Hype add data", {
  # Test adding in data using add_data

  f1 <- function(a, b, c) {-a^2*log(b,10)^2}

  n0 <- 10
  x0 <- data.frame(a=runif(n0, -1,1),
                   b=10^runif(n0, -3,4),
                   c=runif(n0,  1,2))
  y0 <- numeric(n0)
  for (i in 1:n0) {
    y0[i] <- f1(x0$a[i], x0$b[i], x0$c[i])
  }
  cbind(x0, y0)

  # 3 inputs, 2 matter, interaction
  expect_error({
    x9 <- hype$new(eval_func = f1,
                   par_unif("a", -1, 1),
                   par_log10("b", 10^-3, 10^4),
                   par_unif("c", 1,2),
                   n_lhs=6)
    x9$run_all()
  }, NA)
  expect_true(length(x9$Z) == 6)
  # x9$plotX()
  # debugonce(x9$add_data)
  expect_error({
    x9$add_data(X=x0, Z=y0)
  }, NA)
  # x9
  # x9$plotX2()
  expect_error({
    x9$add_EI(1)
    x9$run_all()
  }, NA)
  # x9$plotorder()
  # x9$plotX()
  # x9$plotinteractions()



  # Test adding data when creating object

  # Give in X0, but not Z0
  expect_error({
    r5 <- hype$new(eval_func = f1,
                   par_unif("a", -1, 1),
                   par_log10("b", 10^-3, 10^4),
                   par_unif("c", 1,2),
                   X0=x0)
  }, NA)
  expect_true(length(r5$ffexp$completed_runs) == 10,
              is.null(r5$X),
              !r5$ffexp$completed_runs)
  # r5

  # Give in X0 and Z0
  expect_error({
    r8 <- hype$new(eval_func = f1,
                   par_unif("a", -1, 1),
                   par_log10("b", 10^-3, 10^4),
                   par_unif("c", 1,2),
                   X0=x0, Z0=y0)
  }, NA)
  # r8
  # r8$plotX()

  # Test changing parameter bounds

  n2 <- hype$new(eval_func = f1,
                 par_unif("a", -1, 1),
                 par_log10("b", 10^-3, 10^4),
                 par_log10("c", 1,100),
                 n_lhs=6)
  n2$run_all()
  # n2$plotX()
  # n2$parlist
  # n2$parlowerraw
  # n2$parlowertrans
  # n2$parupperraw
  # n2$paruppertrans
  # expect_equal(n2$parlowerraw, c(-1, .001, 1))
  expect_equal(n2$parlowertrans, c(-1, -3, 0))
  # expect_equal(n2$parupperraw, c(1, 1e4, 1e2))
  expect_equal(n2$paruppertrans, c(1, 4, 2))
  n2$change_par_bounds('a', lower=0)
  n2$change_par_bounds('b', upper=10^8)
  n2$change_par_bounds('c', lower=.1, upper=1e3)
  # expect_equal(n2$parlowerraw, c(0, .001, .1))
  expect_equal(n2$parlowertrans, c(0, -3, -1))
  # expect_equal(n2$parupperraw, c(1, 1e8, 1e3))
  expect_equal(n2$paruppertrans, c(1, 8, 3))
  # expect_true(n2$)
  # n2$parlowerraw
  # n2$parlowertrans
  # n2$parupperraw
  # n2$paruppertrans
  # n2$plotX()

  expect_error({
    hype$new(eval_func = f1,
             par_unif("a", -1, 1),
             par_log10("b", 10^-3, 10^4),
             par_unif("c", 1,2),
             X0=list(a=runif(5)))
  })
})

# Discrete params ----
test_that("discrete params", {
  # Test discrete par
  expect_error({
  hp <- hype$new(eval_func = function(a, b, c) {-1e-3*a^2*log(b,10)^2*ifelse(c=='a', 1, 2) + rnorm(length(a),0,1e-1)},
                 par_unif("a", 6, 8),
                 par_log10("b", 1e-8, 1e-2),
                 par_discrete("c", c('a', 'b')),
                 n_lhs=21)
  }, NA)
  expect_true(!hp$par_all_cts)
  hp$run_all()
  expect_equal(length(hp$Z), 21)
  expect_error({
    hp$plotX(addEIlines = T, addlines = T)
  }, NA)
  expect_error({
    hp$plotXorder()
  }, NA)
  expect_error({
    hp$add_EI(1, model='gaupro')
    hp$run_all()
  }, NA)
  # print('hpZ length is'); print(length(hp$Z))
  expect_equal(length(hp$Z), 22)
})
