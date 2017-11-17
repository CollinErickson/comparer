comparer <- R6::R6Class(
  classname = "comparer",
  public = list(
    outrawdf = data.frame(),
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
    initialize = function(..., eval_func, save_output=FALSE, parallel=FALSE, parallel_cores="detect") {#browser()
      self$eval_func <- eval_func
      self$save_output <- save_output
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
      self$rungrid <- do.call(reshape::expand.grid.df, lapply(1:ncol(self$nvars), function(i){x <- self$nvars[2,i];td <- data.frame(tt=1:x);names(td) <- names(x);td}))

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
        self$parallel_cluster <- parallel::makeCluster(spec = self$parallel_cores, type = "SOCK")
      }
    },
    run_all = function(redo = FALSE, noplot=FALSE) {
      if (!redo) { # Only run ones that haven't been run yet
        to_run <- which(self$completed_runs == FALSE)
      } else {
        to_run <- 1:self$number_runs
      }
      if (self$parallel) {#browser()
        pc <- parallel::detectCores()
        cl1 <- parallel::makeCluster(spec=pc, type="SOCK")
        parallel::parSapply(cl=cl1, to_run,function(ii){self$run_one(ii, noplot=noplot)})
      } else {
        sapply(to_run,function(ii){self$run_one(ii, noplot=noplot)})
      }
      # self$postprocess_outdf()
      invisible(self)
    },
    run_one = function(irow=NULL, save_output=self$save_output, noplot=FALSE) {#browser()
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
      cat("Running ", irow, ", completed ", sum(self$completed_runs),"/",length(self$completed_runs), " ", format(Sys.time(), "%a %b %d %X %Y"), "\n", sep="")
      row_grid <- self$rungrid[irow, , drop=FALSE] #rungrid row for current run
      # if (!is.na(row_grid$seed)) {set.seed(row_grid$seed)}
      row_list <- lapply(1:ncol(self$nvars),
                         function(i) {#browser()
                           ar <- self$arglist[[i]]
                           if (is.data.frame(ar)) {
                             tr <- as.list(ar[row_grid[1,i],])
                           } else if (is.list(ar)) {
                             tr <- ar[[row_grid[1,i]]]
                           } else {
                             tr <- ar[row_grid[1,i]]
                           }
                           if (is.null(names(tr))) {
                             names(tr) <- names(self$arglist)[i]
                           }
                           tr
                         })
      row_list <- as.list(unlist(row_list, recursive = FALSE)) # Need to get list of lists out into single list
      print(row_list)
      # return()
      systime <- system.time(output <- do.call(self$eval_func, row_list))
      # systime <- system.time(u$run(row_grid$batches,noplot=noplot))
      #browser()
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
        newdf0 <- output
      } else {
        newdf0 <- data.frame(runtime=systime[1])
      }
      newdf1 <- cbind(row_grid, newdf0, row.names=NULL)
      nr <- nrow(newdf1)
      #if (browsernow) {browser()}
      #self$outdf <- rbind(self$outdf, newdf1)
      if (nrow(self$outrawdf) == 0) { # If outrawdf not yet created, created blank df with correct names and size
        self$outrawdf <- as.data.frame(matrix(data=NA, nrow=nrow(self$rungrid) * nrow(newdf1), ncol=ncol(newdf1)))
        colnames(self$outrawdf) <- colnames(newdf1)
      }
      self$outrawdf[((irow-1)*nr+1):(irow*nr), ] <- newdf1
      #stop("Here it is adding some columns wrong, force2 should be 0_0 and I think it is as newdf0, but it shows up as 1 in final df")
      if (save_output) {
        if (file.exists(paste0(self$folder_path,"/data_cat.csv"))) { # append new row
          write.table(x=newdf1, file=paste0(self$folder_path,"/data_cat.csv"),append=T, sep=",", col.names=F)
        } else { #create file
          write.table(x=newdf1, file=paste0(self$folder_path,"/data_cat.csv"),append=F, sep=",", col.names=T)
        }
      }
      self$completed_runs[irow] <- TRUE
      invisible(self)
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
}
