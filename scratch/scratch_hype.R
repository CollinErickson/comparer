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
# Compare EI
h1$add_EI(1, model='dk', just_return = T)
h1$add_EI(1, model='gaupro', just_return = T)
h1$add_EI(1, model='dk', calculate_at = c(0, 4))
h1$add_EI(1, model='dk', calculate_at = c(0, -3))
h1$add_EI(1, model='gaupro', calculate_at = c(0, 4))
h1$add_EI(1, model='gaupro', calculate_at = c(0, -3))
xmat <- cbind(runif(1000,-1,1), runif(100,-3,4))
ei_gp <- h1$add_EI(1, model='gaupro', calculate_at = xmat)
ei_dk <- h1$add_EI(1, model='dk', calculate_at = xmat)
plot(ei_gp, ei_dk); abline(a=0,b=1, col=2)

# 5 inputs
h1 <- hype$new(eval_func = function(a, b,d,e,f) {3*a^2 + cos(b) + .3*d},
               par_unif$new("a", -1, 1),
               par_unif$new("b", -3, 4),
               par_unif$new("d", -1, 1),
               par_unif$new("e", -1, 1),
               par_unif$new("f", 0, 10),
               n_lhs=6)
h1$run_all()
h1$plotX()
h1$add_LHS(13)
h1$run_all()
# h1$plotX2()
h1$plotX()
h1$plotX(F)
h1$add_EI(1, model='dk', just_return = T)
h1$add_EI(1, model='gaupro', just_return = T)

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
# h1$add_EI(n=6)


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

# 2 params, 2nd is Log parameter

# 2 inputs, second is null
h1 <- hype$new(eval_func = function(a, b) {a^2 + .2*log(b) + .3*a*log(b)},
               par_unif$new("a", -1, 1),
               par_log10$new("b", 1e-8, 1e-2), n_lhs=6)
h1$run_all()
# h1$plotX2()
h1$add_LHS(13)
h1$run_all()
h1$plotX()
h1$plotinteractions()
h1$pairs()

# 5 inputs, 2 are log
h1 <- hype$new(eval_func = function(a, b, d,e,f) {-a^2*b^2*log(d)*sqrt(f)},
               par_unif$new("a", -1, 1),
               par_unif$new("b", -3, 4),
               par_log10$new("d", 1e-8, 1e-2),
               par_unif$new("e", -1, 1),
               par_log10$new("f", 1, 1000),
               n_lhs=6)
# h1$plotX()
h1$run_all()
h1$add_LHS(50)
h1$plotX()
h1$run_all()
h1$plotX()
h1$plotinteractions()


# Test discrete par
h1 <- hype$new(eval_func = function(a, b, c) {-a^2*b^2*ifelse(c=='a', 1, 2)},
               par_unif$new("a", 6, 8),
               par_log10$new("b", 1e-8, 1e-2),
               par_discrete$new("c", c('a', 'b')),
               n_lhs=6)



# 7 inputs, 2 are log
u4 <- hype$new(eval_func = function(a, b, d,e,f,g,h) {-a^2*b^2*log(d)*sqrt(f)},
               par_unif$new("a", -1, 1),
               par_unif$new("b", -3, 4),
               par_log10$new("d", 1e-8, 1e-2),
               par_unif$new("e", -1, 1),
               par_log10$new("f", 1, 1000),
               par_unif$new("g", 13, 23),
               par_log10$new("h", 1e4, 1e8),
               n_lhs=10)
u4$run_all()
u4$plotX()
u4$add_LHS(50)
u4$run_all()
u4$plotX()
u4$add_LHS(50)
u4$run_all()
u4$plotX()
# 6.9 sec, 6.0
system.time({u4$add_EI(1, just_return = T, model="dk")})
# 65.85 sec, mostly in C_dC. Way too slow.
# 11.4 after changing restarts to 0.
system.time({u4$add_EI(1, just_return = T, model="gaupro")})
# u4$run_all()
# u4$plotX()
# u4$plotinteractions()
