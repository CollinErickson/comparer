test_that("ffexp", {
  expect_error(f1 <- ffexp$new(n=c(100, 1000, 10000),
                               nulleff=c(0,1),
                               eval_func=function(n, nulleff) {
                                 samp <- rnorm(n)
                                 data.frame(mean=mean(samp), se=sd(samp)/sqrt(n))}
  ), NA)
  expect_is(f1, "R6")
  expect_is(f1, "ffexp")
  expect_error(f1$run_all(), NA)

  expect_is(f1$plot_run_times(), "gg")
  calc.eff <- f1$calculate_effects()
  expect_is(calc.eff, "list")
  expect_true(length(calc.eff) == 2)

  # Delete at end
  expect_error(f1$delete(), NA)
})

test_that("ffexp parallel", {
  folder <- paste0(tempdir(),"\\")
  expect_error(f1 <- ffexp$new(n=c(100, 1000, 10000),
                               nulleff=c(0,1),
                               eval_func=function(n, nulleff) {
                                 samp <- rnorm(n)
                                 data.frame(mean=mean(samp), se=sd(samp)/sqrt(n))},
                               parallel=T, parallel_cores=1,
                               save_output = T, folder_path=folder
  ), NA)
  expect_error(f1$run_all(parallel_temp_save = T, write_start_files = T, write_error_files = T), NA)


  # Delete at end
  expect_error(f1$delete(), NA)
})
