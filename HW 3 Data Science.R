# HW 3

# problem 1
set.seed(2021)
A <- c(2, 2, 4, 4, 9, 9)
B <- c(1, 1, 6, 6, 8, 8)
C <- c(3, 3, 5, 5, 7, 7)
n <- 10000
rA <- sample(A, n, replace=TRUE)
rB <- sample(B, n, replace=TRUE)
rC <- sample(C, n, replace=TRUE)
mean(rA > rB)
mean(rB > rC)
mean(rC > rA)

#problem 2
randomizeandget <- function(bowl1, bowl2) {
  bin <- sample(c("bowl1", "bowl2"), 1)
  if (bin == "bowl2") {
    ball = sample(bowl1, 1)
  } else {
    ball = sample(bowl2, 1)
  }
  ball
}

bowl1 <- rep("w", 50)
bowl2 <- rep("b", 50)
mean(replicate(1000, randomizeandget(bowl1, bowl2)) == "w")

bowl3 <- "w"
bowl4 <- c(rep("w", 39), rep("b", 50))
mean(replicate(1000, randomizeandget(bowl3,bowl4)) == "w")

#problem 3
# a. 0.5, b. 0.75