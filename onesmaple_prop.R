library(EnvStats)
power <- propTestPower(n.or.n1 = 200, p.or.p1 = 0.6, p0.or.p2 = 0.5, alternative = "greater", sample.type = "one.sample")


## via simulation

# exact single smple power via simulation
sim_onesample_p <- function(n = 3000, p1 = 0.3, p0 = 0.5) {
  samp <- rbinom(n = n, p = p1, size = 1)
  test <- binom.test(sum(samp), n, p0, alternative = "less")
  test$p.value
}


pvals <- replicate(10000, sim_onesample_p())

mean(pvals < .025)
