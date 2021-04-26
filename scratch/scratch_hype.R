# Hype scratch

# 1 input
p1 <- par_unif$new("a", -1, 1)
# Create hype
h1 <- hype$new(eval_func = function(a) {a^2}, p1, n_lhs=3)
h1
h1$run_all()
# h1$plotX2()
h1$add_LHS(3)
h1$run_all()
h1$plotX()
h1$plotinteractions()
# h1$plotX2()

# 2 inputs, second is null
h1 <- hype$new(eval_func = function(a, b) {a^2},
               par_unif$new("a", -1, 1),
               par_unif$new("b", -3, 4), n_lhs=6)
h1$run_all()
# h1$plotX2()
h1$add_LHS(13)
h1$run_all()
h1$plotX()
h1$plotinteractions()

# 2 inputs, both matter
h1 <- hype$new(eval_func = function(a, b) {a^2 + cos(b)},
               par_unif$new("a", -1, 1),
               par_unif$new("b", -3, 4), n_lhs=6)
h1$run_all()
# h1$plotX2()
h1$add_LHS(13)
h1$run_all()
h1$plotX()
h1$plotinteractions()

# 5 inputs
h1 <- hype$new(eval_func = function(a, b,d,e,f) {3*a^2 + cos(b) + .3*d},
               par_unif$new("a", -1, 1),
               par_unif$new("b", -3, 4),
               par_unif$new("d", -1, 1),
               par_unif$new("e", -1, 1),
               par_unif$new("f", 0, 10),
               n_lhs=6)
h1$run_all()
h1$plotX2()
h1$add_LHS(13)
h1$run_all()
# h1$plotX2()
h1$plotX()
h1$plotX(F)


# 5 inputs with noise
h1 <- hype$new(eval_func = function(a, b,d,e,f) {3*a^2 + cos(b) + .3*d + rnorm(length(a),0,.3)},
               par_unif$new("a", -1, 1),
               par_unif$new("b", -3, 4),
               par_unif$new("d", -1, 1),
               par_unif$new("e", -1, 1),
               par_unif$new("f", 0, 10),
               n_lhs=6)
h1$run_all()
h1$plotX()
h1$add_LHS(13)
# debugonce(h1$ffexp$run_all)
h1$run_all(parallel=F)
h1$plotX()



# 2 inputs, both matter, interaction
h1 <- hype$new(eval_func = function(a, b) {-a^2*b^2},
               par_unif$new("a", -1, 1),
               par_unif$new("b", -3, 4), n_lhs=6)
h1$run_all()
# h1$plotX2()
h1$add_LHS(13)
h1$run_all()
h1$plotX()
h1$plotinteractions()


# 3 inputs, 2 matter, interaction
h1 <- hype$new(eval_func = function(a, b, c) {-a^2*b^2},
               par_unif$new("a", -1, 1),
               par_unif$new("b", -3, 4),
               par_unif$new("c", 1,2),
               n_lhs=6)
h1$run_all()
# h1$plotX2()
h1$add_LHS(40)
h1$run_all()
h1$plotX()
h1$plotinteractions()


# 5 inputs, 2 matter with interaction
h1 <- hype$new(eval_func = function(a, b, d,e,f) {-a^2*b^2},
               par_unif$new("a", -1, 1),
               par_unif$new("b", -3, 4),
               par_unif$new("d", -1, 1),
               par_unif$new("e", -1, 1),
               par_unif$new("f", 0, 10),
               n_lhs=6)
h1$plotX()
h1$run_all()
h1$add_LHS(50)
h1$plotX()
h1$run_all()
h1$plotX()
h1$plotinteractions()
