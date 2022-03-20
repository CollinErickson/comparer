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
