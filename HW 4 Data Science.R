set.seed(123)
theta <- runif(1, -10, 10)
x <- rcauchy(100)
mean(x, trim = 0.05)

do1rep(mean(x, trim = 0.05))