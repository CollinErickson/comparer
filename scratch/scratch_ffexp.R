parallel::detectCores()

cl <- parallel::makeCluster(
  spec=1, type = "SOCK")

parallel::clusterApplyLB(
  cl,
  1:4,
  function(x) {x}
)
cl

f1 <- ffexp$new(eval_func=function(x){x^2}, x=1:3)
f1$save_self()
# debugonce(f1$change_save_folder)
f1$rename_save_folder("abc")

# f1$run_all(save_output = T)

f1 <- ffexp$new(eval_func=function(x, b){x^2 + b}, x=1:3, b=3:6)
f1$run_all()
f1$calculate_effects()
f1$calculate_effects2()

f1$rungrid
plot(f1)


f1 <- ffexp$new(eval_func=function(x, b){c(x^2, x^3)}, x=1:3, b=letters[1:2])
f1$run_all()
f1$outcleandf



f1 <- ffexp$new(eval_func=function(x, b){list(x^2, x^3)}, x=1:3, b=letters[1:2])
f1$run_all()
f1$outcleandf

f1 <- ffexp$new(eval_func=function(x, b){list(x^2, x^3)}, x=1:3, b=letters[1:2],
                extract_output_to_df=function(lst) {lst[[1]]})
f1$extract_output_to_df
f1$run_all()
f1$outcleandf

f1 <- ffexp$new(eval_func=function(x, b){list(x^2, x^3)}, x=1:3, b=letters[1:2],
                extract_output_to_df=function(lst) {data.frame(o1=lst[[1]], o2=lst[[2]])})
f1$extract_output_to_df
f1$run_all()
f1$outcleandf
