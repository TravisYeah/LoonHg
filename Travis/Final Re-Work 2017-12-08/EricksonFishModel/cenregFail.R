
m2
## Simulate data
d <- data.frame(x = rep(1:10, times = 3))
d$conc <- d$x * 2 + rnorm(30, 0, sd = 0.25)
d$group <- rep(1:3, each = 10)
d$conc <- d$conc + d$group
d$group <- as.factor(d$group)
d$conc[ d$conc < 8] <- 0
d$conc <- d$conc + .01
d$cen <- FALSE
d$cen[ d$conc < 8] <- TRUE

log(d$conc)
library(NADA)
with(d, Cen(conc, cen))
m1 <- with(d, cenreg(Cen(conc, cen) ~ x + group - 1,
                     dist = 'lognormal'))
m1
m2 <- with(d, cenreg(Cen(conc, cen) ~ x + group - 1,
                     dist = 'gaussian'))


summary(with(d, glm(log(conc + 1) ~ x + group - 1,
            family = 'gaussian')))

m3 <- with(d, glm(conc + 1~ x + group - 1,
                  family = 'Gamma'))
summary(m3, dispersion = 1)

