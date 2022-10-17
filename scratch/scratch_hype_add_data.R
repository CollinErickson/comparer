# Test adding in data using add_data

f1 <- function(a, b, c) {-a^2*b^2}

n0 <- 10
x0 <- data.frame(a=runif(n0, -1,1),
                 b=runif(n0, -3,4),
                 c=runif(n0,  1,2))
y0 <- numeric(n0)
for (i in 1:n0) {
  y0[i] <- f1(x0$a[i], x0$b[i], x0$c[i])
}
cbind(x0, y0)

# 3 inputs, 2 matter, interaction
h1 <- hype$new(eval_func = f1,
               par_unif("a", -1, 1),
               par_unif("b", -3, 4),
               par_unif("c", 1,2),
               n_lhs=6)
h1$run_all()
h1$plotX()
# debugonce(h1$add_data)
h1$add_data(X=x0, Z=y0)
h1
# h1$plotX2()
h1$add_EI(1)
h1$run_all()
h1$plotorder()
h1$plotX()
h1$plotinteractions()



# Test adding data when creating object

# 3 inputs, 2 matter, interaction
h1 <- hype$new(eval_func = f1,
               par_unif("a", -1, 1),
               par_unif("b", -3, 4),
               par_unif("c", 1,2),
               X0=x0)
h1

h1 <- hype$new(eval_func = f1,
               par_unif("a", -1, 1),
               par_unif("b", -3, 4),
               par_unif("c", 1,2),
               X0=x0, Z0=y0)
h1
h1$plotX()

# Test changing parameter bounds

h1 <- hype$new(eval_func = f1,
               par_unif("a", -1, 1),
               par_unif("b", -3, 4),
               par_log10$new("c", 1,100),
               n_lhs=6)
h1$run_all()
h1$plotX()
h1$parlist
h1$parlowerraw
h1$parlowertrans
h1$parupperraw
h1$paruppertrans
h1$change_par_bounds('a', lower=0)
h1$change_par_bounds('b', upper=12)
h1$change_par_bounds('c', lower=.1, upper=1e3)
h1$parlowerraw
h1$parlowertrans
h1$parupperraw
h1$paruppertrans
h1$plotX()





# 3 inputs, 2 matter, interaction
h1 <- hype$new(eval_func = f1,
               par_unif("a", -1, 1),
               par_unif("b", -3, 4),
               par_log10$new("c", 1,1e8),
               n_lhs=6,
               X0=data.frame(a=runif(10, -1,1),
                        b=runif(10,-3,4),
                        c=10^runif(10,0,8)))

