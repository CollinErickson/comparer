context("ffexp")

test_that("test tempdir on Travis", {
  print("getting a tempdir now")
  td <- paste0(tempdir(), "//ffexptmpdir")
  if (!dir.exists(td)) {
    dir.create(td)
  }
  print(paste("tempdir is", td))
  print("going to save a temp file")
  tf <- paste0(td, "//", "test.rds")
  print(paste('temp file path is', tf))
  saveRDS(1:10, tf)
  print("saved a file, does it exist")
  print(file.exists(tf))
  expect_true(file.exists(tf))
  print("done checking Travis and tempdir")
})

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

  # Print
  t1 <- capture.output(f1)
  expect_is(t1, "character")
  expect_length(t1, 5)

  # rungrid2
  t2 <- f1$rungrid2()
  expect_is(t2, "data.frame")
  expect_equal(dim(t2), c(6, 2))
  t3 <- f1$rungrid2(rows=c(2,5,6)) # Select out rows
  expect_is(t3, "data.frame")
  expect_equal(dim(t3), c(3, 2))
  expect_true(all(t3 == t2[c(2,5,6),]))

  # Delete at end
  expect_error({rm(f1); gc()}, NA)
})

test_that("ffexp parallel", {
  # Skip tests with parallel=T on Travis, gives error
  testthat::skip_on_travis()

  folder <- paste0(tempdir(),"//ffexp3//")
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
  for (tmpfile in list.files(f1$folder_path)) {
    unlink(paste0(f1$folder_path, tmpfile))
  }
  unlink(f1$folder_path, recursive=T)
  expect_error({rm(f1); gc()}, NA)
})


test_that("ffexp parallel 2", {
  # Skip tests with parallel=T on Travis, gives error
  testthat::skip_on_travis()
  testthat::skip_on_os("linux")

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
  # expect_error(f1$save_self(), NA)
  expect_error(f1$delete_save_folder_if_empty(), NA)
  # Delete at end
  expect_error({rm(f1); gc()}, NA)
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
  expect_error(f1$run_one(1, warn_repeat=F), NA)
  expect_error(f1$run_one(1:3, warn_repeat=F), NA)

  # Give bad run_order
  expect_error(f1$run_all(run_order = "reversebackwards", redo = T))
  expect_error(f1$run_all(run_order = "reverse", redo = T, warn_repeat=F), NA)

  prt <- f1$plot_run_times()
  expect_is(prt, "gg")
  calc.eff <- f1$calculate_effects()
  expect_is(calc.eff, "list")
  expect_true(length(calc.eff) == 3)

  # Delete at end
  expect_error({rm(f1); gc()}, NA)
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
  expect_error({rm(f1); gc()}, NA)
  # rm(f1)
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
  expect_error({rm(f1); gc()}, NA)
  # rm(f1)
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
  expect_error({rm(f1); gc()}, NA)
})

test_that("ffexp with error", {
  # Skip tests with parallel=T on Travis, gives error
  testthat::skip_on_travis()

  expect_error(f1 <- ffexp$new(x=list(a=list(r=1,s=2,t=3),b=list(r=4,s=6,t=8)),
                               y=1:4,
                               eval_func=function(a, b, y) {
                                 if (y>2) stop()
                                 samp <- rnorm(b)
                                 data.frame(mean=mean(samp), se=sd(samp)/sqrt(b))},
                               save_output=T,parallel = T, parallel_cores = 1,
                               folder_path = paste0(tempdir(), "//ffexp5//")
  ), NA)
  expect_error(f1$run_all(run_order = "inorder", redo = T, parallel_temp_save = T,
                          write_error_files=T))
  expect_true(sum(f1$completed_runs)==0)
  # print(paste('folder path is', f1$folder_path))
  # print(list.files(f1$folder_path))
  expect_error(f1$recover_parallel_temp_save(delete_after = T), NA)
  # print(list.files(f1$folder_path))
  # Sys.sleep(2)
  # print(f1)
  print(paste("Completed runs is", sum(f1$completed_runs)))
  expect_true(sum(f1$completed_runs)==6)

  prt <- f1$plot_run_times()
  expect_is(prt, "gg")
  calc.eff <- f1$calculate_effects()
  expect_is(calc.eff, "list")
  expect_true(length(calc.eff) == 2)

  # Delete at end
  for (tmpfile in list.files(f1$folder_path)) {
    unlink(paste0(f1$folder_path, tmpfile))
  }
  unlink(f1$folder_path, recursive=T)
  expect_error({rm(f1); gc()}, NA)
})



test_that("ffexp with function input", {
  expect_error(f1 <- ffexp$new(n=c(100, 1000, 10000),
                               nulleff=c(0,1),
                               functi=sin,
                               eval_func=function(n, nulleff, functi) {
                                 samp <- rnorm(n)
                                 data.frame(mean=mean(samp), se=sd(samp)/sqrt(n), functi_0=functi(0))}
  ), NA)
  expect_is(f1, "R6")
  expect_is(f1, "ffexp")
  expect_error(f1$run_one(), NA)
  expect_error(f1$run_all(), NA)
  expect_error(f1$run_all(), NA)
  expect_error(f1$run_one())

  prt <- f1$plot_run_times()
  expect_is(prt, "gg")
  calc.eff <- f1$calculate_effects()
  expect_is(calc.eff, "list")
  expect_true(length(calc.eff) == 3)

  # Delete at end
  expect_error({rm(f1); gc()}, NA)
})
