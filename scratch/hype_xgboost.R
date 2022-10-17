# Find example data set
f1 <- function(x) {x[1] + x[2]*x[3] + .2*x[4]^2*sqrt(1+x[5]) + cos(x[6]) +
    x[7]*(.5-x[8])*cos(x[9])}
d <- 20
ntr <- 2e3
nte <- 1e3
xtr <- matrix(runif(d*ntr), ncol=d)
xte <- matrix(runif(d*nte), ncol=d)
ytr <- apply(xtr, 1, f1) + rnorm(ntr, 0, .1)
yte <- apply(xte, 1, f1) + rnorm(nte, 0, .1)
Dtr <- xgboost::xgb.DMatrix(xtr, label=ytr)
Dte <- xgboost::xgb.DMatrix(xte, label=yte)

xgb_rmse <- function(eta, nrounds, gamma, max_depth, min_child_weight,
                     subsample,
                     colsample_bytree, lambda) {
  nrounds <- as.integer(nrounds)
  max_depth <- as.integer(max_depth)
  # Fit to training
  mod <- xgboost::xgboost(#data=xgboost::xgb.DMatrix(xtr), label = ytr,
                          data=Dtr,
                          params=list(
                            eta=eta,
                            gamma=gamma,
                            max_depth=max_depth,
                            min_child_weight=min_child_weight,
                            subsample=subsample,
                            colsample_bytree=colsample_bytree,
                            lambda=lambda
                          ),
                          print_every_n = 20,
                          nrounds=nrounds)
  # Get preds on testing
  predte <- predict(mod, Dte) #yte)
  # Calculate RMSE
  sqrt(mean((predte - yte)^2))
}

h1 <- hype(
  eval_func=xgb_rmse,
  n_lhs=20,
  eta=par_log10('eta', 1e-4, 1e-1),
  nrounds=par_unif('nrounds', 5, 100),
  gamma=par_unif('gamma', 0, 1),
  max_depth=par_unif('max_depth', 2, 12),
  min_child_weight=par_unif('min_child_weight', 1, 10),
  subsample=par_unif('subsample', .4, 1),
  colsample_bytree=par_unif('colsample_bytree', .2, 1),
  lambda=par_unif('lambda', 0, 3)
)
h1
h1$run_all()
h1$plotX()
h1$plotinteractions()
h1$run_EI_for_time(sec=20, 1)
h1$plotorder()
h1$plotXorder()
h1$plotX()
h1$change_par_bounds('eta', upper=.5)
h1$plotX()
h1$add_EI(1, just_return = T, model='dk')
h1$add_EI(1, just_return = T, model='gaupro')
