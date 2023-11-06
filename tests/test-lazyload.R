library(qwraps2)

# use the example and check the results
e <- new.env()
example("lazyload_cache_dir", local = e)

stopifnot(isTRUE(all.equal(e$kenv$mpg_by_wt_hp, e$env1$mpg_by_wt_hp)))
stopifnot(isTRUE(all.equal(e$env1$mpg_by_wt_hp, e$env3$mpg_by_wt_hp)))

stopifnot(isTRUE(all.equal(e$kenv$mpg_by_wt_hp_am, e$env2$mpg_by_wt_hp_am)))
stopifnot(isTRUE(all.equal(e$env2$mpg_by_wt_hp_am, e$env3$mpg_by_wt_hp_am)))

stopifnot(isTRUE(all.equal(e$kenv$x, exp(1))))
stopifnot(isTRUE(all.equal(e$env1$x, pi)))
stopifnot(isTRUE(all.equal(e$env2$x, exp(1))))
stopifnot(isTRUE(all.equal(e$env3$x, exp(1))))

################################################################################
##                              Extended testing                              ##


################################################################################
#                                 End of File                                  #
################################################################################
