comparer <- R6::R6Class(
  classname = "comparer",
  public = list(
    outrawdf = data.frame(),
    outcleandf = data.frame(),
    outdf = NULL,
    enddf = NULL,
    rungrid = NULL,
    # rungridlist = NULL,
    nvars = NULL,
    arglist = NULL,
    number_runs = NULL,
    completed_runs = NULL,
    eval_func = NULL,
    outlist = NULL,
    save_output = NULL,
    parallel = NULL,
    parallel_cores = NULL,
    parallel_cluster = NULL,
    folder_path = NULL,
    initialize = function(..., eval_func, save_output=FALSE, parallel=FALSE, parallel_cores="detect", folder_path) {#browser()
      self$eval_func <- eval_func
      self$save_output <- save_output
      self$folder_path <- if (missing(folder_path)) {paste0(getwd(),"/comparerobj_",gsub(" ","_",gsub(":","-",Sys.time())))} else {folder_path}
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
        } else {
          self$parallel_cores <- parallel_cores
        }
        # self$parallel_cluster <- parallel::makeCluster(spec = self$parallel_cores, type = "SOCK")
      }
    },
    run_all = function(redo = FALSE, noplot=FALSE, run_order, parallel_temp_save=FALSE) {
      if (missing(run_order)) { # random for parallel for load balancing
        if (self$parallel) {run_order <- "random"}
        else {run_order <- "inorder"}
      }
      if (!redo) { # Only run ones that haven't been run yet
        to_run <- which(self$completed_runs == FALSE)
      } else {
        to_run <- 1:self$number_runs
      }
      if (run_order == "inorder") {} # Leave it in order
      else if (run_order == "reverse") {to_run <- rev(to_run)}
      else if (run_order == "random") {to_run <- sample(to_run)}
      else {stop("run_order not recognized #567128")}

      if (self$parallel) {#browser()
        # pc <- parallel::detectCores()
        # cl1 <- parallel::makeCluster(spec=pc, type="SOCK")
        # parallel::parSapply(cl=cl1, to_run,function(ii){self$run_one(ii, noplot=noplot)})
        if (is.null(self$parallel_cluster)) {
          self$parallel_cluster <- parallel::makeCluster(spec = self$parallel_cores, type = "SOCK")
        }
        if (parallel_temp_save) {self$create_save_folder_if_nonexistant()}
        parout <- parallel::clusterApplyLB(
          cl=self$parallel_cluster,
          to_run,
          function(ii){
            tout <- self$run_one(ii, noplot=noplot, is_parallel=TRUE)
            if (parallel_temp_save) {
              saveRDS(object=tout, file=paste0(self$folder_path,"/parallel_temp_output_",ii,".rds"))
            }
            tout
          })
        lapply(parout, function(oneout) {do.call(self$add_result_of_one, oneout)})
        parallel::stopCluster(self$parallel_cluster)
        self$parallel_cluster <- NULL
        if (parallel_temp_save) {
          sapply(to_run,
                 function(ii) {
                   unlink(paste0(self$folder_path,"/parallel_temp_output_",ii,".rds"))
                 })
          self$delete_save_folder_if_empty()
        }
      } else {
        sapply(to_run,function(ii){self$run_one(ii, noplot=noplot)})
      }
      # self$postprocess_outdf()
      invisible(self)
    },
    run_one = function(irow=NULL, save_output=self$save_output, noplot=FALSE, is_parallel=FALSE) {#browser()
      if (is.null(irow)) { # If irow not given, set to next not run
        if (any(self$completed_runs == FALSE)) {
          irow <- which(self$completed_runs == 0)[1]
        } else {
          stop("irow not given and all runs completed")
        }
      } else if (length(irow) > 1) { # If more than one, run each separately
        sapply(irow, function(ii){self$run_one(irow=ii, save_output=save_output)})
        return(invisible(self))
      } else if (self$completed_runs[irow] == TRUE) {
        warning("irow already run, will run again anyways")
      }
      # browser()
      cat("Running ", irow, ", completed ", sum(self$completed_runs),"/",
          length(self$completed_runs), " ",
          format(Sys.time(), "%a %b %d %X %Y"), "\n", sep="")
      row_grid <- self$rungrid[irow, , drop=FALSE] #rungrid row for current run
      # if (!is.na(row_grid$seed)) {set.seed(row_grid$seed)}
      # Can't just set row_list <- lapply since a function can't be named
      #  so to get names there we need to handle functions separately.
      row_list <- list();lapply(1:ncol(self$nvars),
                         function(i) {
                           ar <- self$arglist[[i]]
                           if (is.data.frame(ar)) {
                             tr <- as.list(ar[row_grid[1,i],])
                           } else if (is.list(ar)) {
                             tr <- ar[[row_grid[1,i]]]
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
                               # row_list <- c(row_list, names(self$arglist)[i]=tr)
                               row_list <<- c(row_list, tmpnameforfunc=tr)
                               names(row_list)[names(row_list)=="tmpnameforfunc"] <<- names(self$arglist)[i]
                               return()
                             } else {
                               names(tr) <- names(self$arglist)[i]
                             }
                           }
                           row_list <<- c(row_list, tr)
                           # tr
                         })
      # row_list <- as.list(unlist(row_list, recursive = FALSE)) # Need to get list of lists out into single list
      print(row_list)
      # return()

      # Get df for output row, must be number of string, no functions
      row_df <- lapply(1:ncol(self$nvars),
                                function(i) {
                                  ar <- self$arglist[[i]]
                                  if (is.data.frame(ar)) {
                                    tr <- as.list(ar[row_grid[1,i],])
                                  } else if (is.list(ar)) {
                                    tr <- ar[[row_grid[1,i]]]
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
      row_df <- as.list(unlist(row_df, recursive = FALSE)) # Need to get list of lists out into single list

      # Run and time it
      start_time <- Sys.time()
      systime <- system.time(output <- do.call(self$eval_func, row_list))
      end_time <- Sys.time()

      # If parallel need to return everything to be added to original object
      if (is_parallel) {
        return(list(output=output, systime=systime, irow=irow, row_grid=row_grid, row_df=row_df, start_time=start_time, end_time=end_time, save_output=save_output))
      }
      # If not parallel
      # Add results using function
      self$add_result_of_one(output=output, systime=systime, irow=irow, row_grid=row_grid, row_df=row_df, start_time=start_time, end_time=end_time, save_output=save_output)
      # Return invisible self
      invisible(self)
    },
    add_result_of_one = function(output, systime, irow, row_grid, row_df, start_time, end_time, save_output) {
      # systime <- system.time(u$run(row_grid$batches,noplot=noplot))
      # browser()
      # newdf0 <- data.frame(batch=u$stats$iteration, mse=u$stats$mse,
      #                      pvar=u$stats$pvar, pamv=u$stats$pamv,
      #                      pred_intwerror=u$stats$intwerror,
      #                      actual_intwerror=u$stats$actual_intwerror,
      #                      #obj=row_grid$obj,
      #                      num=paste0(row_grid$obj,row_grid$repl),
      #                      time = systime[3], #repl=row_grid$repl,
      #                      #force_old=row_grid$force_old, force_pvar=row_grid$force_pvar,
      #                      force2=paste0(row_grid$force_old, '_', row_grid$force_pvar),
      #
      #                      row.names=NULL,
      #                      stringsAsFactors = FALSE
      # )
      self$outlist[[irow]] <- output
      if (is.data.frame(output)) {
        output$runtime <- systime[3]
        newdf0 <- output
        # newdf0$start_time <- start_time
        # newdf0$end_time <- end_time
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
      newdf1 <- cbind(row_grid, newdf0, row.names=NULL)
      newdf_clean <- cbind(row_df, newdf0, row.names=NULL, stringsAsFactors=FALSE)
      nr <- nrow(newdf1)
      if (nrow(self$outrawdf) == 0) { # If outrawdf not yet created, created blank df with correct names and size
        self$outrawdf <- as.data.frame(matrix(data=NA, nrow=nrow(self$rungrid) * nrow(newdf1), ncol=ncol(newdf1)))
        colnames(self$outrawdf) <- colnames(newdf1)
        # Create outcleandf too
        self$outcleandf <- as.data.frame(matrix(data=NA, nrow=nrow(self$rungrid) * nrow(newdf_clean), ncol=ncol(newdf_clean)))
        colnames(self$outcleandf) <- colnames(newdf_clean)
        # browser()
        for (i in 1:ncol(self$outcleandf)) {class(self$outcleandf[,i]) <- class(newdf_clean[1,i])}
      }
      self$outrawdf[((irow-1)*nr+1):(irow*nr), ] <- newdf1
      self$outcleandf[((irow-1)*nr+1):(irow*nr), ] <- newdf_clean


      if (save_output) {
        if (file.exists(paste0(self$folder_path,"/data_cat.csv"))) { # append new row
          write.table(x=newdf1, file=paste0(self$folder_path,"/data_cat.csv"),append=T, sep=",", col.names=F)
        } else { #create file
          write.table(x=newdf1, file=paste0(self$folder_path,"/data_cat.csv"),append=F, sep=",", col.names=T)
        }
      }
      self$completed_runs[irow] <- TRUE
    },
    plot_run_times = function() {
      ggplot2::ggplot(self$outcleandf) +
        ggplot2::geom_segment(
          ggplot2::aes(x=start_time, xend=end_time,
                       y=run_number, yend=run_number)) +
        ggplot2::xlab("Start and end time") +
        ggplot2::ylab("Run number")
    },
    save_self = function() {
      file_path <- paste0(self$folder_path,"/object.rds")
      cat("Saving to ", file_path, "\n")
      self$create_save_folder_if_nonexistant()
      saveRDS(object = self, file = file_path)
    },
    create_save_folder_if_nonexistant = function() {
      if (!dir.exists(self$folder_path)) {
        dir.create(self$folder_path)
      }
    },
    delete_save_folder_if_empty = function() {
      if (length(list.files(path=self$folder_path, all.files = TRUE, no.. = TRUE)) == 0) {
        unlink(self$folder_path, recursive = TRUE)
      } else {
        stop("Folder is not empty")
      }
    },
    recover_parallel_temp_save = function() {
      # Read in and save
      for (ii in 1:nrow(self$rungrid)) {
        # Check for file
        file_ii <- paste0(self$folder_path,"/parallel_temp_output_",ii,".rds")
        if (file.exists(file_ii)) {
          # Read in
          oneout <- readRDS(file=file_ii)
          do.call(self$add_result_of_one, oneout)
          # Delete it
          unlink(file_ii)
        }
      }
      self$delete_save_folder_if_empty()
    },
    delete = function() {
      cat("Deleting...\n")
      if (!is.null(self$parallel_cluster)) {parallel::stopCluster(self$parallel_cluster)}
    }
  ),
  private = list(

  )
)
if (F) {
  cc <- comparer$new(a=1:3,b=2, cd=data.frame(c=3:4,d=5:6), eval_func=function(...) {list(...)})
  cc <- comparer$new(a=1:3,b=2, cd=data.frame(c=3:4,d=5:6), eval_func=function(...,a,b) {data.frame(apb=a+b)})
  cc$arglist
  cc$run_one()
  cc$run_all()
  # Try parallel
  cc <- comparer$new(a=1:3,b=2, cd=data.frame(c=3:4,d=5:6), eval_func=function(...,a,b) {Sys.sleep(rexp(1, 10));data.frame(apb=a+b)}, parallel=T)
  cc <- comparer$new(a=1:10, b=list(sin), eval_func=function(...,a) {Sys.sleep(rexp(1, 5));data.frame(apb=a^2)}, parallel=F)
  system.time(cc$run_all())
  cc$outrawdf
  cd <- comparer$new(a=5:2, bc=data.frame(b=LETTERS[14:17], c=6:9, stringsAsFactors=F), eval_func=function(a,b,c){data.frame(ac=a+c)}); cd$run_all()
  cd <- comparer$new(a=5:2, bc=data.frame(b=LETTERS[14:17], c=6:9, stringsAsFactors=F), d=cos, eval_func=function(a,b,c,...){data.frame(ac=a+c)}); cd$run_all()
  cd <- comparer$new(a=5:2, bc=data.frame(b=LETTERS[14:17], c=6:9, stringsAsFactors=F), g='a', eval_func=function(a,b,c,...){data.frame(ac=a)}); cd$run_all(); cd$outcleandf
}
