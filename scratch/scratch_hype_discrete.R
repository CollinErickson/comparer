# Test discrete par
hp <- hype$new(eval_func = function(a, b, c) {-1e-3*a^2*log(b,10)^2*ifelse(c=='a', 1, 2) + rnorm(length(a),0,1e-1)},
               par_unif$new("a", 6, 8),
               par_log10$new("b", 1e-8, 1e-2),
               par_discrete$new("c", c('a', 'b')),
               n_lhs=28)
hp$par_all_cts
hp
hp$run_all()
hp
hp$plotX(addEIlines = F)
hp$plotX(addEIlines = T)
hp$plotX(addlines = F, addEIlines = F)
hp$plotorder()
hp$plotXorder()
hp$add_EI(1, model='gaupro')
hp$run_all()
hp$plotX(addlines=F, addEIlines = F)
hp$plotorder()
hp$plotXorder()

# Two discrete par, two cts
dp2 <- hype$new(eval_func =
                 function(a, b, c, d) {
                   -1e-3*a^2*log(b,10)^2*ifelse(c=='a', 1, 2) + ifelse(d=='a',1,2) + rnorm(length(a),0,1e-1)},
               par_unif$new("a", 6, 8),
               par_log10$new("b", 1e-8, 1e-2),
               par_discrete$new("c", c('a', 'b')),
               par_discrete$new("d", c('a', 'b', 'c', 'd')),
               n_lhs=28)
dp2$par_all_cts
dp2
dp2$run_all()
dp2
dp2$plotX(addEIlines = F)
dp2$plotorder()
dp2$plotXorder()
dp2$add_EI(1, model='gaupro')
dp2$run_all()
dp2$plotX(addlines=T, addEIlines = F)
dp2$plotorder()
dp2$plotXorder()


# Two discrete par, two cts, two null
dp3 <- hype$new(eval_func =
                  function(a, b, c, d, e, f) {
                    -1e-3*a^2*log(b,10)^2*ifelse(c=='a', 1, 2) + ifelse(d=='a',1,2) + rnorm(length(a),0,1e-1)},
                par_unif$new("a", 6, 8),
                par_log10$new("b", 1e-8, 1e-2),
                par_discrete$new("c", c('a', 'b')),
                par_discrete$new("d", c('a', 'b', 'c', 'd')),
                par_unif$new('e', -4,5),
                par_log10$new('f', 1e-1, 1e7),
                n_lhs=28,model = "GauPro")
dp3$run_all()
dp3$add_LHS(77)
dp3$run_all()
system.time(dp3_mod <- dp3$mod)
