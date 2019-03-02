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

  prt <- f1$plot_run_times()
  expect_is(prt, "gg")
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
  expect_error(f1$run_all(parallel_temp_save = T, write_start_files = T, write_error_files = T,
                          delete_parallel_temp_save_after = F), NA)
  expect_error(f1$save_self(), NA)
  expect_error(f1$recover_parallel_temp_save(), NA)


  # Delete at end
  expect_error(f1$delete(), NA)
})


test_that("ffexp parallel 2", {
  folder <- paste0(tempdir(),"\\")
  expect_error(f1 <- ffexp$new(n=c(100, 1000, 10000),
                               nulleff=c(0,1),
                               eval_func=function(n, nulleff) {
                                 samp <- rnorm(n)
                                 data.frame(mean=mean(samp), se=sd(samp)/sqrt(n))},
                               parallel=T, parallel_cores=1,
                               save_output = T, folder_path=folder
  ), NA)
  expect_error(f1$run_all(parallel_temp_save = T, write_start_files = T,
                          write_error_files = T,
                          delete_parallel_temp_save_after = T), NA)
  expect_error(f1$save_self(), NA)

  # Delete at end
  expect_error(f1$delete(), NA)
})

test_that("ffexp with runone", {
  expect_error(f1 <- ffexp$new(n=c(100, 1000, 10000),
                               tdf=data.frame(a=1:2,b=3:4),
                               nulleff=c(0,1),
                               eval_func=function(n, nulleff, a, b) {
                                 samp <- rnorm(n)
                                 data.frame(mean=mean(samp), se=sd(samp)/sqrt(n))}
  ), NA)
  expect_is(f1, "R6")
  expect_is(f1, "ffexp")
  expect_error(f1$run_one(1), NA)
  expect_error(f1$run_one(1:3), NA)

  # Give bad run_order
  expect_error(f1$run_all(run_order = "reversebackwards", redo = T))
  expect_error(f1$run_all(run_order = "reverse", redo = T), NA)

  prt <- f1$plot_run_times()
  expect_is(prt, "gg")
  calc.eff <- f1$calculate_effects()
  expect_is(calc.eff, "list")
  expect_true(length(calc.eff) == 3)

  # Delete at end
  expect_error(f1$delete(), NA)
})

test_that("ffexp parallel detect cores, don't run", {
  folder <- paste0(tempdir(),"\\")
  expect_error(f1 <- ffexp$new(n=c(100, 1000, 10000),
                               nulleff=c(0,1),
                               eval_func=function(n, nulleff) {
                                 samp <- rnorm(n)
                                 data.frame(mean=mean(samp), se=sd(samp)/sqrt(n))},
                               parallel=T, parallel_cores="detect",
                               save_output = T, folder_path=folder
  ), NA)
  # Delete at end
  expect_error(f1$delete(), NA)
  rm(f1)
})
test_that("ffexp parallel detect cores - 1, don't run", {
  folder <- paste0(tempdir(),"\\")
  expect_error(f1 <- ffexp$new(n=c(100, 1000, 10000),
                               nulleff=c(0,1),
                               eval_func=function(n, nulleff) {
                                 samp <- rnorm(n)
                                 data.frame(mean=mean(samp), se=sd(samp)/sqrt(n))},
                               parallel=T, parallel_cores="detect-1",
                               save_output = T, folder_path=folder
  ), NA)
  # Delete at end
  expect_error(f1$delete(), NA)
  rm(f1)
})




test_that("ffexp with list", {
  expect_error(f1 <- ffexp$new(x=list(a=list(r=1,s=2,t=3),b=list(r=4,s=6,t=8)),
                               y=1:4,
                               eval_func=function(a, b, y) {
                                 samp <- rnorm(b)
                                 data.frame(mean=mean(samp), se=sd(samp)/sqrt(b))}
  ), NA)
  expect_error(f1$run_all(run_order = "reverse", redo = T), NA)

  prt <- f1$plot_run_times()
  expect_is(prt, "gg")
  calc.eff <- f1$calculate_effects()
  expect_is(calc.eff, "list")
  expect_true(length(calc.eff) == 2)

  # Delete at end
  expect_error(f1$delete(), NA)
})

test_that("ffexp with error", {
  expect_error(f1 <- ffexp$new(x=list(a=list(r=1,s=2,t=3),b=list(r=4,s=6,t=8)),
                               y=1:4,
                               eval_func=function(a, b, y) {
                                 if (y>2) stop()
                                 samp <- rnorm(b)
                                 data.frame(mean=mean(samp), se=sd(samp)/sqrt(b))},
                               save_output=T,parallel = T, parallel_cores = 1,
                               folder_path = tempdir()
  ), NA)
  expect_error(f1$run_all(run_order = "inorder", redo = T, parallel_temp_save = T))
  expect_true(sum(f1$completed_runs)==0)
  expect_error(f1$recover_parallel_temp_save(delete_after = T), NA)
  expect_true(sum(f1$completed_runs)==6)

  prt <- f1$plot_run_times()
  expect_is(prt, "gg")
  calc.eff <- f1$calculate_effects()
  expect_is(calc.eff, "list")
  expect_true(length(calc.eff) == 2)

  # Delete at end
  expect_error(f1$delete(), NA)
})
