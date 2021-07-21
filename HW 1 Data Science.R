#problem 1
numbers <- seq(1,100,by=1)
for (x in numbers)
if (x %% 7)

#problem 2
sevens <- 7 * (1:100)
cat(sevens,"\n")

#problem 3
for (x in 1:9) {
  binomialSample <- rbinom(10000, 100, x/10)
  par(mfrow=c(1,2))
  hist(binomialSample, main=paste("p=", x/10), xlim=c(0,100))
  boxplot(binomialSample, ylim=c(0,100))
}