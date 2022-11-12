# Hype scratch ----

# 1 input ----
p1 <- par_unif("a", -1, 1)
# Create hype
h1 <- hype(eval_func = function(a) {a^2}, p1, n_lhs=3)
h1
h1$run_all()
# h1$plotX2()
h1$add_LHS(3)
h1$run_all()
h1$plotX()
h1$plotinteractions()
# h1$plotX2()

# 2 inputs, second is null ----
h1 <- hype(eval_func = function(a, b) {a^2},
           par_unif("a", -1, 1),
           par_unif("b", -3, 4), n_lhs=6)
h1$run_all()
# h1$plotX2()
h1$add_LHS(13)
h1$run_all()
h1$add_EI(1)
h1$run_all()
h1$plotX()
h1$plotinteractions()

# 2 inputs, both matter ----
h1 <- hype(eval_func = function(a, b) {a^2 + cos(b)},
           par_unif("a", -1, 1),
           par_unif("b", -3, 4), n_lhs=6)
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

# 5 inputs ----
h1 <- hype(eval_func = function(a, b,d,e,f) {3*a^2 + cos(b) + .3*d},
           par_unif("a", -1, 1),
           par_unif("b", -3, 4),
           par_unif("d", -1, 1),
           par_unif("e", -1, 1),
           par_unif("f", 0, 10),
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

# 5 inputs with noise ----
h1 <- hype(eval_func = function(a, b,d,e,f) {3*a^2 + cos(b) + .3*d + rnorm(length(a),0,.3)},
           par_unif("a", -1, 1),
           par_unif("b", -3, 4),
           par_unif("d", -1, 1),
           par_unif("e", -1, 1),
           par_unif("f", 0, 10),
           n_lhs=6)
h1$run_all()
h1$plotX()
h1$add_LHS(13)
# debugonce(h1$ffexp$run_all)
h1$run_all(parallel=F)
h1$plotX()
# h1$add_EI(n=6)


# 2 inputs, both matter, interaction ----
h1 <- hype(eval_func = function(a, b) {-a^2*b^2},
           par_unif("a", -1, 1),
           par_unif("b", -3, 4), n_lhs=6)
h1$run_all()
# h1$plotX2()
h1$add_LHS(13)
h1$run_all()
h1$plotX()
h1$plotinteractions()


# 3 inputs, 2 matter, interaction ----
h1 <- hype(eval_func = function(a, b, c) {-a^2*b^2},
           par_unif("a", -1, 1),
           par_unif("b", -3, 4),
           par_unif("c", 1,2),
           n_lhs=6)
h1$run_all()
# h1$plotX2()
h1$add_LHS(40)
h1$run_all()
h1$plotX()
h1$plotinteractions()


# 5 inputs, 2 matter with interaction ----
h1 <- hype(eval_func = function(a, b, d,e,f) {-a^2*b^2},
           par_unif("a", -1, 1),
           par_unif("b", -3, 4),
           par_unif("d", -1, 1),
           par_unif("e", -1, 1),
           par_unif("f", 0, 10),
           n_lhs=6)
h1$plotX()
h1$run_all()
h1$add_LHS(50)
h1$plotX()
h1$run_all()
h1$plotX()
h1$plotinteractions()

# 2 params, 2nd is Log parameter

# 2 inputs, second is null ----
h1 <- hype(eval_func = function(a, b) {a^2 + .2*log(b) + .3*a*log(b)},
           par_unif("a", -1, 1),
           par_log10("b", 1e-8, 1e-2), n_lhs=6)
h1$run_all()
# h1$plotX2()
h1$add_LHS(13)
h1$run_all()
h1$plotX()
h1$plotinteractions()
h1$pairs()

# 5 inputs, 2 are log ----
h1 <- hype(eval_func = function(a, b, d,e,f) {-a^2*b^2*log(d)*sqrt(f)},
           par_unif("a", -1, 1),
           par_unif("b", -3, 4),
           par_log10("d", 1e-8, 1e-2),
           par_unif("e", -1, 1),
           par_log10("f", 1, 1000),
           n_lhs=6)
# h1$plotX()
h1$run_all()
h1$add_LHS(50)
h1$plotX()
h1$run_all()
h1$plotX()
h1$plotinteractions()


# Test unordered par ----
h1 <- hype(eval_func = function(a, b, c) {-a^2*b^2*ifelse(c=='a', 1, 2)},
           par_unif("a", 6, 8),
           par_log10("b", 1e-8, 1e-2),
           par_unordered("c", c('a', 'b')),
           n_lhs=6)
h1$run_all()
h1$add_EI(1)
h1$run_all()
h1$add_LHS(n=10)
h1$run_all()
h1$X

# Test unordered and ordered par ----
h2 <- hype(eval_func = function(a, b, c, d) {-a^2*b^2*ifelse(c=='a', 1, 2)*ifelse(d=='l',.6,1)},
           par_unif("a", 6, 8),
           par_log10("b", 1e-8, 1e-2),
           par_unordered("c", c('a', 'b')),
           par_ordered("d", c('j', 'k', 'l')),
           n_lhs=6)
h2$run_all()
h2$add_EI(1)



# 7 inputs, 2 are log ----
u4 <- hype(eval_func = function(a, b, d,e,f,g,h) {-a^2*b^2*log(d)*sqrt(f)},
           par_unif("a", -1, 1),
           par_unif("b", -3, 4),
           par_log10("d", 1e-8, 1e-2),
           par_unif("e", -1, 1),
           par_log10("f", 1, 1000),
           par_unif("g", 13, 23),
           par_log10("h", 1e4, 1e8),
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
