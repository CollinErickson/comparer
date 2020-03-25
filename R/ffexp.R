#' Full factorial experiment
#'
#' A class for easily creating and evaluating full factorial experiments.
#'
#' @section Usage:
#' \preformatted{
#' e1 <- ffexp$new(eval_func=, )
#'
#' e1$run_all()
#'
#' e1$plot_run_times()
#'
#' e1$save_self()
#'
#' }
#'
#' @section Arguments:
#' \code{eval_func} The function called to evaluate each design point.
#'
#' \code{...} Factors and their levels to be evaluated at.
#'
#' \code{save_output} Should the output be saved?
#'
#' \code{parallel} If \code{TRUE}, function evaluations are done in parallel.
#'
#' \code{parallel_cores} Number of cores to be used in parallel.
#' If \code{"detect"}, \code{parallel::detectCores()} is used to determine
#' number. \code{"detect-1"} may be used so that the computer isn't running
#' at full capacity, which can slow down other tasks.
#'
#' @section Methods:
#' \code{$new()} Initialize an experiment. The preprocessing is done,
#' but no function evaluations are run.
#'
#' \code{$run_all()} Run all factor combinations.
#'
#' \code{$run_one()} Run a single factor combination.
#'
#' \code{$add_result_of_one()} Used to add result of evaluation to data set,
#' don't manually call.
#'
#' \code{$plot_run_times()} Plot the run times. Especially useful when
#' they have been run in parallel.
#'
#' \code{$save_self()} Save ffexp R6 object.
#'
#' \code{$recover_parallel_temp_save()} If you ran the experiment using
#' parallel with \code{parallel_temp_save=TRUE} and it crashes partway
#' through, call this to recover the runs that were completed.
#' Runs that were stopped mid-execution are not recoverable.
#'
#' @name ffexp
#' @examples
#' # Two factors, both with two levels.
#' #   The evaluation function simply prints out the combination
#' cc <- ffexp$new(a=1:2,b=c("A","B"),
#'                 eval_func=function(...) {c(...)})
#' # View the factor settings it will run (each row).
#' cc$rungrid
#' # Evaluate all four settings
#' cc$run_all()
#'
#'
#' cc <- ffexp$new(a=1:3,b=2, cd=data.frame(c=3:4,d=5:6),
#'                 eval_func=function(...) {list(...)})
NULL

#' @export
#' @field outrawdf Raw data frame of output.
#' @field outcleandf Clean output in data frame.
#' @field rungrid matrix specifying which inputs will be run
#' for each experiment.
#' @field nvars Number of variables
#' @field allvars All variables
#' @field varlist Character vector of objects to pass to a parallel
#' cluster.
#' @field arglist List of values for each argument
#' @field number_runs Total number of runs
#' @field completed_runs Logical vector of whether each run has been
#' completed.
#' @field eval_func The function that is called for each experiment trial.
#' @field outlist A list of the output from each run.
#' @field save_output Logical of whether the output should be saved.
#' @field parallel Logical whether experiment runs should be run in
#' parallel. Allows for massive speedup.
#' @field parallel_cores How many cores to use when running in parallel.
#' Can be an integer, or 'detect' will detect how many cores are
#' available, or 'detect-1' will do one less than that.
#' @field parallel_cluster The parallel cluster being used.
#' @field folder_path The path to the folder where output will be saved.
#' @field verbose How much should be printed when running. 0 is none,
#' 2 is average.
ffexp <- R6::R6Class(
  classname = "ffexp",
  public = list(
    outrawdf = data.frame(),
    outcleandf = data.frame(),
    # outdf = NULL,
    # enddf = NULL,
    rungrid = NULL, # Input settings to be run
    # rungridlist = NULL,
    nvars = NULL, # Number of input variables
    allvars = NULL, # df with name, class, and num levels for each var
    varlist = NULL, # List of variable names to pass through to parallel
    arglist = NULL, # List of values for each argument
    number_runs = NULL,
    completed_runs = NULL, # Logical vector of whether each run is done
    eval_func = NULL,
    outlist = list(), # List of outputs from each run
    save_output = NULL,
    parallel = NULL,
    parallel_cores = NULL,
    parallel_cluster = NULL,
    folder_path = NULL,
    verbose = 2,
    #' @description Create an `ffexp` object.
    #' @param ... Input arguments for the experiment
    #' @param eval_func The function to be run. It must take
    #' named arguments matching the names of ...
    #' @param save_output Should output be saved to file?
    #' @param parallel Should a parallel cluster be used?
    #' @param parallel_cores When running in parallel, how many cores should
    #' be used.
    #' @param folder_path Where the data and files should be stored.
    #' If not given, a folder in the existing directory will be created.
    #' @param varlist Character vector of names of objects that need to be
    #' passed to the parallel environment.
    #' @param verbose How much should be printed when running. 0 is none,
    #' 2 is average.
    initialize = function(..., eval_func, save_output=FALSE, parallel=FALSE,
                          parallel_cores="detect", folder_path,
                          varlist=NULL, verbose=2) {
      self$eval_func <- eval_func
      self$save_output <- save_output
      self$folder_path <- if (missing(folder_path)) {
        paste0(getwd(),
               "/ffexpobj_",gsub(" ","_",gsub(":","-",Sys.time())))}
      else {folder_path}
      self$varlist <- varlist
      self$arglist <- list(...)
      self$nvars <- sapply(self$arglist,
                           function(i) {
                             if (is.data.frame(i)) {
                               rev(dim(i))
                             } else if (is.list(i)) {
                               c(length(i), length(i[[1]]))
                             } else {
                               c(1, length(i))
                             }
                           }
      )
      self$allvars <- do.call(rbind,
                              lapply(1:length(self$arglist),
                                     function(j) {
                                       i <- self$arglist[[j]]
                                       if (is.data.frame(i)) {
                                         data.frame(name =colnames(i),
                                                    class=unlist(lapply(i, class)),
                                                    num  =nrow(i))
                                       } else if (is.list(i)) {
                                         data.frame(name =names(self$arglist[j]),
                                                    class="list",
                                                    num  =1)
                                       } else {
                                         data.frame(name =names(self$arglist[j]),
                                                    class=class(i),
                                                    num  =1)
                                       }
                                     }
                              ))
      self$rungrid <- do.call(reshape::expand.grid.df,
                              lapply(1:ncol(self$nvars),
                                     function(i){
                                       x <- self$nvars[2,i]
                                       td <- data.frame(tt=1:x)
                                       names(td) <- names(x)
                                       td
                                     }
                              )
      )

      self$number_runs <- nrow(self$rungrid)
      self$completed_runs <- rep(FALSE, self$number_runs)
      self$parallel <- parallel
      self$parallel_cores <- parallel_cores
      if (self$parallel) {
        # For now assume using parallel package
        if (parallel_cores == "detect") {
          self$parallel_cores <- parallel::detectCores()
        } else if (parallel_cores == "detect-1") {
          self$parallel_cores <- parallel::detectCores() - 1
          if (self$parallel_cores == 0) {
            "Only 1 core detected, can't do 'detect-1'"
          }
        } else {
          self$parallel_cores <- parallel_cores
        }
      }
      self$verbose <- verbose
    },
    #' @description Run an experiment. The user can choose
    #' to run all rows, or just specified ones, if it should
    #' be run in parallel, and what files should be saved.
    #' @param to_run Which rows should be run? If NULL, then all that haven't
    #' been run yet.
    #' @param redo Should already completed rows be run again?
    #' @param run_order In what order should the rows by run?
    #' Options: random, in_order, and reverse.
    #' @param save_output Should the output be saved?
    #' @param parallel Should it be run in parallel?
    #' @param parallel_temp_save Should temp files be written when running
    #' in parallel? Prevents losing results if it crashes partway through.
    #' @param write_start_files Should start files be written?
    #' @param write_error_files Should error files be written for rows that
    #' fail?
    #' @param delete_parallel_temp_save_after If using parallel temp save
    #' files, should they be deleted afterwards?
    #' @param varlist A character vector of names of variables to be passed
    #' the the parallel cluster.
    #' @param verbose How much should be printed when running. 0 is none,
    #' 2 is average.
    #' @param warn_repeat Should warnings be given when repeating already
    #' completed rows?
    run_all = function(to_run=NULL,
                       redo = FALSE, run_order,
                       save_output=self$save_output,
                       parallel=self$parallel,
                       parallel_temp_save=save_output,
                       write_start_files=save_output,
                       write_error_files=save_output,
                       delete_parallel_temp_save_after=FALSE,
                       varlist=self$varlist,
                       verbose=self$verbose,
                       warn_repeat=TRUE) {
      if (missing(run_order)) { # random for parallel for load balancing
        if (parallel) {run_order <- "random"}
        else {run_order <- "inorder"}
      }
      if (!is.null(to_run)) {
        # to_run given
      } else if (!redo) { # Only run ones that haven't been run yet
        to_run <- which(self$completed_runs == FALSE)
      } else {
        to_run <- 1:self$number_runs
      }
      if (run_order == "inorder") {} # Leave it in order
      else if (run_order == "reverse") {to_run <- rev(to_run)}
      else if (run_order == "random") {
        # sample of a single number is bad
        if (length(to_run)> 1) {
          to_run <- sample(to_run)
        } else {
          to_run <- to_run
        }
      } else {stop(paste("run_order not recognized, should be one of inorder,",
                         "reverse, or random #567128"))}

      if (parallel && length(to_run > 0)) {
        # pc <- parallel::detectCores()
        # cl1 <- parallel::makeCluster(spec=pc, type="SOCK")
        # parallel::parSapply(cl=cl1, to_run,function(ii){self$run_one(ii)})
        if (is.null(self$parallel_cluster)) {
          # Make cluster
          self$create_save_folder_if_nonexistent()
          self$parallel_cluster <- parallel::makeCluster(
            spec=self$parallel_cores, type = "SOCK",
            outfile=paste0(self$folder_path,
                           "/parallel_MASTEROUTPUT.txt"))
          # Export any variables
          if (!is.null(varlist)) {
            parallel::clusterExport(cl=self$parallel_cluster,
                                    varlist=varlist)
          }
        }
        if (parallel_temp_save) {self$create_save_folder_if_nonexistent()}
        cat("About to start run in parallel, run order is:\n    ",
            to_run,
            "\n")
        # cat("\tCluster is"); print(self$parallel_cluster)
        # browser()
        parout <- parallel::clusterApplyLB(
          cl=self$parallel_cluster,
          x=to_run,
          fun=function(ii){
            tout <- self$run_one(ii, is_parallel=TRUE,
                                 write_start_files=write_start_files,
                                 write_error_files=write_error_files,
                                 verbose=verbose)
            if (parallel_temp_save) {
              saveRDS(object=tout,
                      file=paste0(self$folder_path,
                                  "/parallel_temp_output_",ii,".rds"))
            }
            tout
          })
        lapply(parout,
               function(oneout) {do.call(self$add_result_of_one, oneout)})
        parallel::stopCluster(self$parallel_cluster)
        self$parallel_cluster <- NULL
        # Delete temp files if needed
        if (parallel_temp_save && delete_parallel_temp_save_after) {
          sapply(to_run,
                 function(ii) {
                   unlink(
                     paste0(self$folder_path,
                            "/parallel_temp_output_",ii,".rds"))
                 })
          self$delete_save_folder_if_empty()
        }
      } else { # Not parallel
        sapply(to_run,
               function (ii) {
                 tout <- self$run_one(ii, write_start_files=write_start_files,
                                      write_error_files=write_error_files,
                                      warn_repeat=warn_repeat,
                                      save_output=save_output,
                                      verbose=verbose)
                 if (parallel_temp_save) {
                   saveRDS(object=tout,
                           file=paste0(self$folder_path,
                                       "/parallel_temp_output_",ii,".rds"))
                 }
                 tout
               })
      }
      # self$postprocess_outdf()
      invisible(self)
    },
    #' @description Run a single row of the experiment.
    #' You can specify which one to run.
    #' Generally this should not be used by users, use `run_all`
    #' instead.
    #' @param irow Which row should be run?
    #' @param save_output Should the output be saved?
    #' @param write_start_files Should a file be written when starting
    #' the experiment?
    #' @param write_error_files Should a file be written if there is
    #' an error?
    #' @param warn_repeat Should a warning be given if repeating a row?
    #' @param is_parallel Is this being run in parallel?
    #' @param verbose How much should be printed when running. 0 is none,
    #' 2 is average.
    run_one = function(irow=NULL, save_output=self$save_output,
                       write_start_files=save_output,
                       write_error_files=save_output,
                       warn_repeat=TRUE,
                       is_parallel=FALSE,
                       verbose=self$verbose) {
      # Set up single row to run
      if (is.null(irow)) { # If irow not given, set to next not run
        if (any(self$completed_runs == FALSE)) {
          irow <- which(self$completed_runs == 0)[1]
        } else {
          stop("irow not given and all runs completed")
        }
      } else if (length(irow) > 1) { # If more than one, run each separately
        sapply(irow,
               function(ii){self$run_one(irow=ii, save_output=save_output,
                                         warn_repeat=warn_repeat,
                                         verbose=verbose)})
        return(invisible(self))
      } else if ((self$completed_runs[irow] == TRUE) && warn_repeat) {
        warning("irow already run, will run again anyways")
      }

      # Get ready for single run
      if (verbose >= 1) {
        cat("Running ", irow, ", completed ", sum(self$completed_runs),"/",
            length(self$completed_runs), " ",
            format(Sys.time(), "%a %b %d %X %Y"), "\n", sep="")
      }
      row_grid <- self$rungrid[irow, , drop=FALSE] #rungrid row for current run
      # if (!is.na(row_grid$seed)) {set.seed(row_grid$seed)}
      # Can't just set row_list <- lapply since a function can't be named
      #  so to get names there we need to handle functions separately.
      row_list <- list()
      lapply(1:ncol(self$nvars),
             function(i) {
               ar <- self$arglist[[i]]
               if (is.data.frame(ar)) {
                 tr <- as.list(ar[row_grid[1,i, drop=TRUE],])
               } else if (is.list(ar)) {
                 # tr <- ar[[row_grid[1,i]]]
                 tr <- lapply(ar, function(x) x[[row_grid[1,i]]])
               } else if (is.function(ar)) {
                 tr <- ar # If single value is a function
               } else {
                 tr <- ar[row_grid[1,i]]
               }
               if (is.null(names(tr))) {
                 if (is.function(tr)) {
                   # can't set names of function
                   # tr <- c(names(self$arglist)[i]=tr)
                   # row_list[names(self$arglist)[i]] <- tr
                   # row_list <- c(row_list,
                   #               names(self$arglist)[i]=tr)
                   row_list <<- c(row_list, tmpnameforfunc=tr)
                   names(row_list)[
                     names(row_list)=="tmpnameforfunc"] <<-
                     names(self$arglist)[i]
                   return()
                 } else {
                   names(tr) <- names(self$arglist)[i]
                 }
               }
               row_list <<- c(row_list, tr)
               # tr
             })
      # Need to get list of lists out into single list
      # row_list <- as.list(unlist(row_list, recursive = FALSE))
      if (verbose >= 2) {
        print(row_list)
      }

      # Get df for output row, must be number of string, no functions
      row_df_list <- lapply(1:ncol(self$nvars),
                            function(i) {
                              ar <- self$arglist[[i]]
                              if (is.data.frame(ar)) {
                                tr <- as.list(ar[row_grid[1,i, drop=TRUE],])
                              } else if (is.list(ar)) {
                                # tr <- ar[[row_grid[1,i]]]
                                tr <- lapply(ar, function(x) x[[row_grid[1,i]]])
                              } else if (is.function(ar)) {
                                tr <- ar # If single value is a function
                              } else {
                                tr <- ar[row_grid[1,i]]
                              }
                              # Can't use functions
                              #  and deparse(quote(tr)) gives "tr"
                              if (is.function(tr)) {
                                tr <- row_grid[1,i]
                              }
                              if (is.null(names(tr))) {
                                names(tr) <- names(self$arglist)[i]
                              }
                              tr
                            })
      # Need to get list of lists out into single list
      row_df <- as.list(unlist(row_df_list, recursive = FALSE))

      # Write start file so user can see which ones are currently
      #  running and when they started.
      if (write_start_files) {
        self$create_save_folder_if_nonexistent()
        write_start_file_path <- paste0(self$folder_path,
                                        "/STARTED_parallel_temp_output_",
                                        irow,".txt")
        cat(timestamp(), "\n", file=write_start_file_path)
        # cat row_list, but can't cat a list, so convert to string
        format_row_list <- format(row_list)
        for (i in seq.int(1, length(format_row_list), 1)) {
          cat(names(format_row_list)[i], "\n",
              file=write_start_file_path, append=TRUE)
          cat(format_row_list[i],        "\n\n",
              file=write_start_file_path, append=TRUE)
        }
        rm(i, format_row_list)
      }

      # Run and time it
      try.run <- try({
        start_time <- Sys.time()
        systime <- system.time(output <- do.call(self$eval_func, row_list))
        end_time <- Sys.time()
      })

      # Delete write start file
      if (write_start_files && file.exists(write_start_file_path)) {
        unlink(write_start_file_path)
        self$delete_save_folder_if_empty()
      }

      # If error while running, write out error file/message
      #  Want this after delete write start file so that file
      #  gets deleted either way
      if (inherits(try.run, "try-error")) {
        if (write_error_files) {
          self$create_save_folder_if_nonexistent()
          write_error_file_path <- paste0(self$folder_path,
                                          "/ERROR_parallel_temp_output_",
                                          irow,".txt")
          cat(Sys.time(),"\n", file=write_error_file_path)
          format_row_list <- format(row_list)
          for (i in seq.int(1, length(format_row_list), 1)) {
            cat(names(format_row_list)[i], "\n",
                file=write_error_file_path, append=TRUE)
            cat(format_row_list[i],        "\n\n",
                file=write_error_file_path, append=TRUE)
          }
          rm(i, format_row_list)
          cat("Error was:\n\n", file=write_error_file_path)
          cat(try.run[1], "\n", file=write_error_file_path)
        }
        stop(paste0("Error in run_one for irow=",irow,"\n",try.run))
      }


      # If parallel need to return everything to be added to original object
      if (is_parallel) {
        return(list(output=output, systime=systime, irow=irow,
                    row_grid=row_grid, row_df=row_df,
                    start_time=start_time, end_time=end_time,
                    save_output=save_output))
      }
      # If not parallel
      # Add results using function
      self$add_result_of_one(output=output, systime=systime, irow=irow,
                             row_grid=row_grid, row_df=row_df,
                             start_time=start_time, end_time=end_time,
                             save_output=save_output)

      # Return invisible self
      invisible(self)
    },
    #' @description Add the result of a single experiment to the object.
    #' This shouldn't be used by users.
    #' @param output The output of the experiment.
    #' @param systime The time it took to run
    #' @param irow The row of inputs used.
    #' @param row_grid The corresponding row in the run grid.
    #' @param row_df The corresponding row data frame.
    #' @param start_time The start time of the experiment.
    #' @param end_time The end time of the experiment.
    #' @param save_output Should the output be saved?
    add_result_of_one = function(output, systime, irow, row_grid, row_df,
                                 start_time, end_time, save_output) {
      # This is used to save results after running an item

      # Save entire output as list
      self$outlist[[irow]] <- output

      # Now try to create clean data frame with outputs
      # (exclude lists) and include runtime.
      if (is.data.frame(output)) {
        output$runtime <- systime[3]
        newdf0 <- output
        # newdf0$start_time <- start_time
        # newdf0$end_time <- end_time
      } else if (is.vector(output) && !is.list(output)) {
        newdf0 <- data.frame(t(output), stringsAsFactors=FALSE)
        newdf0$runtime <- systime[3]
      } else {
        newdf0 <- data.frame(runtime=systime[3])
        # newdf0 <- data.frame()
        # newdf0$start_time <- start_time
        # newdf0$end_time <- end_time
      }
      # newdf0$runtime <- systime[3]
      newdf0$start_time <- start_time
      newdf0$end_time <- end_time
      newdf0$run_number <- irow

      # Add to outrawdf
      newdf1 <- cbind(row_grid, newdf0, row.names=NULL,
                      stringsAsFactors=FALSE)
      newdf_clean <- cbind(row_df, newdf0, row.names=NULL,
                           stringsAsFactors=FALSE)
      nr <- nrow(newdf1)
      if (nrow(self$outrawdf) == 0) {
        # If outrawdf not yet created, created blank df with correct names
        #   and size
        self$outrawdf <- as.data.frame(
          matrix(data=NA,
                 nrow=nrow(self$rungrid) * nrow(newdf1),
                 ncol=ncol(newdf1)))
        colnames(self$outrawdf) <- colnames(newdf1)
        for (i in 1:ncol(self$outrawdf)) {
          class(self$outrawdf[,i]) <- class(newdf1[1,i])
        }
        # Create outcleandf too
        self$outcleandf <- as.data.frame(
          matrix(data=NA, nrow=nrow(self$rungrid) * nrow(newdf_clean),
                 ncol=ncol(newdf_clean)))
        colnames(self$outcleandf) <- colnames(newdf_clean)

        for (i in 1:ncol(self$outcleandf)) {
          if ("factor" %in% class(newdf_clean[1,i])) {
            # Use %in% since POSIXct and POSIXt
            self$outcleandf[,i] <- factor(self$outcleandf[,i],
                                          levels=levels(row_df[[i]]))
          } else {
            class(self$outcleandf[,i]) <- class(newdf_clean[1,i])
          }
        }
      }

      # Put it in place, but only if dimensions match up
      if (self$number_runs * nrow(newdf1) == nrow(self$outrawdf)) {
        self$outrawdf[((irow-1)*nr+1):(irow*nr), ] <- newdf1
        self$outcleandf[((irow-1)*nr+1):(irow*nr), ] <- newdf_clean
      }


      if (save_output) {
        if (file.exists(paste0(self$folder_path,"/data_cat.csv"))) {
          # append new row
          write.table(x=newdf1,
                      file=paste0(self$folder_path,"/data_cat.csv"),
                      append=T, sep=",", col.names=F)
        } else { #create file
          write.table(x=newdf1,
                      file=paste0(self$folder_path,"/data_cat.csv"),
                      append=F, sep=",", col.names=T)
        }
      }
      self$completed_runs[irow] <- TRUE
    },
    #' @description Plot the run times of each trial.
    plot_run_times = function() {
      ggplot2::ggplot(self$outcleandf) +
        ggplot2::geom_segment(
          ggplot2::aes(x=start_time, xend=end_time,
                       y=run_number, yend=run_number)) +
        ggplot2::xlab("Start and end time") +
        ggplot2::ylab("Run number")
      # invisible(self)
    },
    #' @description Calculate the effects of each variable as if this
    #' was an experiment using a linear model.
    calculate_effects = function() {
      nvar <- ncol(self$rungrid)
      sapply(1:nvar,
             function(i) {
               outputcols <- (nvar+1):(ncol(self$outrawdf)-3)
               tdf <- plyr::ddply(self$outrawdf[,c(i, outputcols)],
                                  names(self$rungrid)[i],
                                  colMeans)
               # Need df first, then list, then vector
               if (is.data.frame(self$arglist[[i]])) {
                 tdf[,1] <- rownames(self$arglist[[i]])[tdf[,1]]
               } else if (is.list(self$arglist[[i]])) {
                 tdf[,1] <- names(self$arglist[[i]][[1]])[tdf[,1]]
               } else if (is.vector(self$arglist[[i]])) {
                 tdf[,1] <- self$arglist[[i]][tdf[,1]]
               }
               nlev <- nrow(tdf)
               if (nlev > 1) {
                 for (j in (2:nlev)) {
                   for (k in 1:(j-1)) {
                     # newdf <- data.frame(diffname=paste0(tdf[j,1],"-",
                     #                     tdf[k,1]),
                     #                     mean=tdf[j,2]-tdf[k,2],
                     #                     se=tdf[j,3]-tdf[k,3],
                     #                     runtime=tdf[j,4]-tdf[k,4])
                     newdf <- data.frame(tempname=paste0(tdf[j,1],"-",tdf[k,1]))
                     colnames(newdf)[1] <- names(self$rungrid)[i]
                     newdf <- cbind(newdf,
                                    tdf[j,outputcols-nvar+1]-
                                      tdf[k,outputcols-nvar+1])
                     tdf <- rbind(tdf, newdf)
                   }
                 }
               }
               # Convert to list here so I can give it a name
               tl <- list(tdf)
               names(tl) <- paste0("Mean for levels of ", names(self$rungrid)[i])
               tl
             }
      )
    },
    #' @description Save this R6 object
    save_self = function() {
      file_path <- paste0(self$folder_path,"/object.rds")
      cat("Saving to ", file_path, "\n")
      self$create_save_folder_if_nonexistent()
      saveRDS(object = self, file = file_path)
      invisible(self)
    },
    #' @description Create the save folder if it doesn't already exist.
    create_save_folder_if_nonexistent = function() {
      # Need to remove tailing slashes when using dir.exists
      fp <- self$folder_path
      while(substr(fp, nchar(fp), nchar(fp)) %in% c("/", "\\")) {
        fp <- substr(fp, 1, nchar(fp) - 1)
      }
      if (!dir.exists(fp)) {
        dir.create(fp)
      }
      invisible(self)
    },
    #' @description Delete the save folder if it is empty.
    #' Used to prevent leaving behind empty folders.
    delete_save_folder_if_empty = function() {
      if (length(list.files(path=self$folder_path,
                            all.files = TRUE, no.. = TRUE)) == 0) {
        unlink(self$folder_path, recursive = TRUE)
      } else {
        message("Folder is not empty")
      }
      invisible(self)
    },
    #' @description Running this loads the information saved to files
    #' if `save_parallel_temp_save=TRUE` was used when running.
    #' Useful when running long jobs in parallel so that you don't
    #' lose all results if it crashes before finishing.
    #' @param delete_after Should the temp files be deleted after they
    #' are recovered? If TRUE, make sure you save the ffexp object after
    #' running this function so
    #' you don't lose the data.
    recover_parallel_temp_save = function(delete_after=TRUE) {
      # Read in and save
      for (ii in 1:nrow(self$rungrid)) {
        # Check for file
        file_ii <- paste0(self$folder_path,"/parallel_temp_output_",ii,".rds")
        if (file.exists(file_ii)) {
          # Read in
          oneout <- readRDS(file=file_ii)
          do.call(self$add_result_of_one, oneout)
          # Delete it
          if (delete_after) {
            unlink(file_ii)
          }
        }
      }
      self$delete_save_folder_if_empty()
    },
    #' @description Display the input rows of the experiment.
    #' rungrid just gives integers, this gives the actual values.
    #' @param rows Which rows to display the inputs for? On big
    #' experiments, specifying the rows can be much faster.
    rungrid2 = function(rows=1:nrow(self$rungrid)) {
      if (all(is.logical(rows))) {
        rows <- which(rows)
      }
      # Most of this was taken from run_one where it starts to make outcleandf
      rowsdf <- lapply(
        # 1:nrow(self$rungrid),
        rows,
        function(iirow) {
          row_grid <- self$rungrid[iirow, , drop=FALSE]
          row_list <- lapply(1:ncol(self$nvars),
                             function(i) {
                               ar <- self$arglist[[i]]
                               if (is.data.frame(ar)) {
                                 tr <- as.list(ar[row_grid[1,i, drop=TRUE],])
                               } else if (is.list(ar)) {
                                 # tr <- ar[[row_grid[1,i]]]
                                 tr <- lapply(ar,
                                              function(x) x[[row_grid[1,i]]])
                               } else if (is.function(ar)) {
                                 tr <- ar # If single value is a function
                               } else {
                                 tr <- ar[row_grid[1,i]]
                               }
                               # Can't use functions
                               #  and deparse(quote(tr)) gives "tr"
                               if (is.function(tr)) {
                                 tr <- row_grid[1,i]
                               }
                               if (is.null(names(tr))) {
                                 names(tr) <- names(self$arglist)[i]
                               }
                               tr
                               # Convert to list so names work
                               tl <- list(tr)
                               # Don't rename lists since it will be unlisted,
                               #  and we only want the inner name.
                               if (!is.list(tr)) {
                                 names(tl) <- names(self$arglist)[i]
                               }
                               tl
                             })
          row_list2 <- as.list(unlist(row_list, recursive = FALSE))
          row_df <- as.data.frame(row_list2, stringsAsFactors=FALSE)
          # names(row_df) <- sapply(row_list2, names)
          # Only needed b/c of factor vars
          row_df
        })
      rowsdf <- do.call(rbind, rowsdf)
      for (i in 1:nrow(self$allvars)) {
        if (class(rowsdf[,i]) != as.character(self$allvars$class[i])) {
          class(rowsdf[,i]) <- as.character(self$allvars$class[i])
        }
      }
      rownames(rowsdf) <- rows
      rowsdf
    },
    #' @description Add a variable to the experiment. You must specify the
    #' value of the variable for all existing rows, and then also the values
    #' of the variable which haven't been run yet.
    #' @param name Name of the variable being added.
    #' @param existing_value Which existing argument is a level being added to?
    #' @param new_values The values of the new variable which have not been
    #' run. This should not include `arg_name`, the name of the new variable
    #' at the existing values.
    #' @param suppressMessage Should the message be suppressed? The message
    #' tells the user a new variable was added and it is being returned in a new
    #' object. Default FALSE.
    add_variable = function(name, existing_value, new_values,
                            suppressMessage=FALSE) {
      # Need to update outlist, completed_runs, number_runs,
      # arglist, nvars, rungrid, outcleandf, outrawdf
      if (name %in% names(self$arglist)) {
        stop("name is already the name of an argument")
      }
      if (is.data.frame(new_values)) {
        all_values <- rbind(existing_value, new_values)
        length_all_values <- nrow(all_values)
      } else {
        all_values <- c(existing_value, new_values)
        length_all_values <- length(all_values)
      }
      new_exp <- self$clone(deep=TRUE)
      new_exp$arglist[[name]] <- all_values
      new_exp$number_runs <- self$number_runs * length_all_values
      new_exp$completed_runs <- rep(FALSE, new_exp$number_runs)
      # outlist should be good, or else set them all to null
      # rungrid
      new_exp$nvars <- sapply(new_exp$arglist,
                              function(i) {
                                if (is.data.frame(i)) {
                                  rev(dim(i))
                                } else if (is.list(i)) {
                                  c(length(i), length(i[[1]]))
                                } else {
                                  c(1, length(i))
                                }
                              }
      )
      new_exp$rungrid <- do.call(reshape::expand.grid.df,
                                 lapply(1:ncol(new_exp$nvars),
                                        function(i){
                                          x <- new_exp$nvars[2,i]
                                          td <- data.frame(tt=1:x)
                                          names(td) <- names(x)
                                          td
                                        }
                                 )
      )
      # self$number_runs <- nrow(self$rungrid)
      existing_indexes <- which(new_exp$rungrid[,name]==1)
      for(old_index in existing_indexes) {
        old_rg_row <- self$rungrid[old_index,]
        new_index <- which(apply(new_exp$rungrid,1,
                                 function(x) {all(x==c(old_rg_row, 1))}))
        # new_index <- 1
        new_exp$completed_runs[new_index] <- self$completed_runs[old_index]
        new_exp$outlist[[new_index]] <- self$outlist[[old_index]]
      }
      if (!suppressMessage) {
        message("Returning new object with added variable")
      }
      return(new_exp)
    },
    #' @description Add a level to one of the arguments. This returns a new
    #' object. The existing object is not changed.
    #' @param arg_name Which existing argument is a level being added to?
    #' @param new_values The value of the new levels to be added to `arg_name`.
    #' @param suppressMessage Should the message be suppressed? The message
    #' tells the user a new level was added and it is being returned in a new
    #' object. Default FALSE.
    add_level = function(arg_name, new_values, suppressMessage=FALSE) {
      # Need to update outlist, completed_runs, number_runs,
      # arglist, nvars, rungrid, outcleandf, outrawdf
      if (!(arg_name %in% names(self$arglist))) {
        stop("arg_name is not the name of an existing argument")
      }
      existing_values <- self$arglist[[arg_name]]
      if (is.data.frame(existing_values)) {
        all_values <- rbind(existing_values, new_values)
        existing_level_indexes <- 1:nrow(existing_values)
      } else {
        all_values <- c(existing_values, new_values)
        existing_level_indexes <- 1:length(existing_values)
      }
      new_exp <- self$clone(deep=TRUE)
      new_exp$arglist[[arg_name]] <- all_values
      # outlist should be good, or else set them all to null
      # rungrid
      new_exp$nvars <- sapply(new_exp$arglist,
                              function(i) {
                                if (is.data.frame(i)) {
                                  rev(dim(i))
                                } else if (is.list(i)) {
                                  c(length(i), length(i[[1]]))
                                } else {
                                  c(1, length(i))
                                }
                              }
      )
      new_exp$rungrid <- do.call(reshape::expand.grid.df,
                                 lapply(1:ncol(new_exp$nvars),
                                        function(i){
                                          x <- new_exp$nvars[2,i]
                                          td <- data.frame(tt=1:x)
                                          names(td) <- names(x)
                                          td
                                        }
                                 )
      )
      new_exp$number_runs <- nrow(new_exp$rungrid)
      new_exp$completed_runs <- rep(FALSE, new_exp$number_runs)
      # self$number_runs <- nrow(self$rungrid)
      # Or could use rungrid and replace new_values with biggest number
      # existing_indexes <- which(
      #   new_exp$rungrid[,arg_name] < max(new_exp$rungrid[,arg_name]))
      existing_indexes <- which(
        new_exp$rungrid[,arg_name] %in% existing_level_indexes)
      for(old_index in existing_indexes) {
        old_rg_row <- self$rungrid[old_index,]
        new_index <- which(apply(new_exp$rungrid,1,
                                 function(x) {
                                   all(x==old_rg_row)
                                 }))
        # new_index <- 1
        new_exp$completed_runs[new_index] <- self$completed_runs[old_index]
        new_exp$outlist[[new_index]] <- self$outlist[[old_index]]
      }; rm(old_rg_row, new_index, old_index)
      for (new_index in setdiff(1:new_exp$number_runs, existing_indexes)) {
        stopifnot(!new_exp$completed_runs[new_index])
        # Needed in order to run add_level twice in a row without having run it
        new_exp$outlist[[new_index]] <- list(NULL)
      }
      # Notify user that it wasn't changed in place
      if (!suppressMessage) {
        message("Returning new object with added variable")
      }
      return(new_exp)
    },
    #' @description Printing the object shows some
    #' summary information.
    print = function() {
      s <- paste0(
        c(
          "ffexp object from the comparer package\n",
          "   ", sum(self$completed_runs), " / ",
          length(self$completed_runs), " completed\n",
          "   parallel : ", self$parallel, "\n",
          "   Use $run_all() to run all remaining\n",
          "   Use $run_one() to run a single\n"
        )
      )
      cat(s, sep="")
    },
    #' @description  Stop the parallel cluster.
    stop_cluster = function() {
      # cat("Deleting...\n")
      if (!is.null(self$parallel_cluster)) {
        # This gives error for unknown reason
        try(
          {
            parallel::stopCluster(self$parallel_cluster)
            message("Stopped cluster")
          },
          silent=FALSE
        )
      }
      invisible(self)
    },
    #' @description Cleanup after deleting object.
    finalize = function() {
      self$stop_cluster()
    }
  ),
  private = list(

  )
)
if (F) {
  cc <- ffexp$new(a=1:3,b=2, cd=data.frame(c=3:4,d=5:6),
                  eval_func=function(...) {list(...)})
  cc <- ffexp$new(a=1:3,b=2, cd=data.frame(c=3:4,d=5:6),
                  eval_func=function(...,a,b) {data.frame(apb=a+b)})
  cc$arglist
  cc$run_one()
  cc$run_all()
  # Try parallel
  cc <- ffexp$new(a=1:3,b=2, cd=data.frame(c=3:4,d=5:6),
                  eval_func=function(...,a,b) {
                    Sys.sleep(rexp(1, 10));data.frame(apb=a+b)},
                  parallel=T)
  cc <- ffexp$new(a=1:10, b=list(sin),
                  eval_func=function(...,a) {
                    Sys.sleep(rexp(1, 5));data.frame(apb=a^2)}, parallel=F)
  system.time(cc$run_all())
  cc$outrawdf
  cd <- ffexp$new(a=5:2,
                  bc=data.frame(b=LETTERS[14:17], c=6:9,
                                stringsAsFactors=F),
                  eval_func=function(a,b,c){data.frame(ac=a+c)}
  ); cd$run_all()
  cd <- ffexp$new(a=5:2,
                  bc=data.frame(b=LETTERS[14:17], c=6:9,
                                stringsAsFactors=F), d=cos,
                  eval_func=function(a,b,c,...){data.frame(ac=a+c)}
  ); cd$run_all()
  cd <- ffexp$new(a=5:2,
                  bc=data.frame(b=LETTERS[14:17], c=6:9,
                                stringsAsFactors=F), g='a',
                  eval_func=function(a,b,c,...){data.frame(ac=a)}
  ); cd$run_all(); cd$outcleandf
  # Calculate parallel efficiency
  with(data = cc$outcleandf,
       sum(runtime) / as.numeric(max(end_time) - min(start_time)),
       units="secs")
  # parallel 'detect-1'
  system.time(ffexp$new(a=1:4, eval_func=function(a){Sys.sleep(1)},
                        parallel = T,
                        parallel_cores = 'detect-1')$run_all())

  # Try to get useful output
  f1 <- ffexp$new(n=c(100, 1000, 10000),
                  nulleff=c(0,1),
                  eval_func=function(n, nulleff) {
                    samp <- rnorm(n)
                    data.frame(mean=mean(samp), se=sd(samp)/sqrt(n))}
  )
  f1$run_all()
  f1$outcleandf
  f1$calculate_effects()
  cc <- ffexp$new(a=5:7,b=22, cd=data.frame(c=3:4,d=5:6,
                                            row.names = c("35", "46")),
                  eval_func=function(...,a,b) {data.frame(apb=a+b)});
  cc$run_all(); cc$calculate_effects()
}
