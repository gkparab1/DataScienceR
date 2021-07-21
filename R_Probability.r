## Elevator waiting time
set.seed(123) ## set the random seed so that results are reproducible
n <- 10000 ## number of days to simulate
## the sample space is the possible locations of the elevator [1, 15]
elevator <- runif(n, min=1, max=15) ## outcomes of experiments
up <- (elevator < 13)
## the elevator is below the 13th floor so that it will goes up
sum(up) / n

set.seed(1)
n = 1000000
elevator = runif(n, min=1, max=15)
up = elevator < 13
sum(up) / n

## Two Child Problem
kids <- c("boy", "girl")
res <- replicate(n=10000, sample(kids, 2, replace=TRUE))
## Simulate the game n times
n.bb <- sum((res[1,]=="boy") & (res[2,]=="boy"))
n.bg <- sum((res[1,]=="boy") & (res[2,]=="girl"))
n.gb <- sum((res[1,]=="girl") & (res[2,]=="boy"))
n.gg <- sum((res[1,]=="girl") & (res[2,]=="girl"))

n.gg / (n.gb + n.gg)
n.gg / (n.gb + n.gg + n.bg)

kids = c("boy", "girl")
n = 10
res = replicate(n, sample(kids, 2, replace=TRUE))
n.bb = sum(res[1,] == "boy" & res[2,] == "boy")
n.bg = sum(res[1,] == "boy" & res[2,] == "girl")
n.gb = sum(res[1,] == "girl" & res[2,] == "boy")
n.gg = sum(res[1,] == "girl" & res[2,] == "girl")
n.gg / (n.gb + n.gg)
n.gg / (n.gb + n.bg + n.gg)

sample(kids, 2, replace=TRUE)

## Fair Division
set.seed(2021)
players <- c("Tom", "Jerry")
res <- replicate(n=1000, sample(players, 5, replace=TRUE))
## Simulate the game n times
TomFirst = (res[1:3,] == "Tom")
## Whether Tom won in the first three rounds
TomTwice = colSums(TomFirst) == 2
## Whether Tom won twice in the first three rounds
TomFinal = (colSums(res == "Tom") >=3) & TomTwice
## Whether Tom would be the final winner
TomP = sum(TomFinal) / sum(TomTwice)
## The probability that Tom would be the final winner
c(Tom=TomP, Jerry=1-TomP)

players = c("Tom", "Jerry")
n = 100000
res = replicate(n, sample(players, 5, replace=TRUE))
TomTwice = colSums(res[1:3,] == "Tom") == 2
n.tom = sum(colSums(res=="Tom") >= 3 & TomTwice)
n.tomtwice = sum(TomTwice)
n.tom / n.tomtwice
1 - n.tom / n.tomtwice

n = 10000
res = replicate(n, sample(players, 5, replace=TRUE))
JerryOnce = colSums(res[1:3,] == "Jerry") == 1
n.jerry = sum(colSums(res=="Jerry") >= 3 & JerryOnce)
n.jerryonce = sum(JerryOnce)
n.jerry / n.jerryonce

## Birthday Problem
set.seed(2021)
birthday <- function(n=23, yours="1980-05-01"){
    # n is the number of people
    # yours is your birthday with format "year-mm-dd"
    N <- 365 # number of days each year
    room <- sample(1:N, size=n, replace=TRUE) # randomly choose n people
    doy <- as.numeric(strftime(yours, format="%j"))
    # convert the convert your birthday to day of year
    share <- length(unique(room)) < n
    ## if there are people sharing a common birthday
    same <- doy %in% room # if someone's birthday the same as yours
    return (c(share, same))
}

res <- replicate(n=10000, birthday(17))
rowMeans(res)
 
res <- replicate(n=1000, birthday(253))
rowMeans(res)

## ## Birth problem direct calculation
P <- function(n) {
 p <- 1
 for (i in 1:n)
     p <- p * (365 - i + 1) / 365
 p <- 1 - p
 return(p)
}

for (n in c(4, 16, 23, 32, 40, 56))
 print(P(n))

## n <- c(4, 16, 23, 32, 40, 56, 252, 253, 254)
## P2 <- 1 - (364 / 365)^n
## P2

## Monty Hall problem
set.seed(2021)
n <- 1000
first <- 1
car <- rep(NA, n)
host <- rep(NA, n)
for (i in 1:n){
    car[i] <- sample(1:3, 1)
    if (car[i] == 1){
        host[i] <- sample(c(2, 3), 1)
    } else if (car[i] == 2) {
        host[i] <- 3
    } else if (car[i] == 3) {
        host[i] <- 2
    }
}
observed <- car[host == 3]
sum(observed == 1) / length(observed)
sum(observed == 2) / length(observed)


## Simpson’s Paradox
5 / (5 + 6)
3 / (3 + 4)

6 / (6 + 3)
9 / (9 + 5)

(5 + 6) / (5 + 6 + 6 + 3)
(3 + 9) / (3 + 4 + 9 + 5)

set.seed(2021)
g <- 4 # number of groups
n <- 40 # number of instances in each group
z <- rep(1:4, each=n) # grouping variable
x <- runif(n*g, 0, 2) + z # x variable that depends on z
y <- 2 * z - x + rnorm(n*g) # y variable that depends on x and z
plot(x, y) # plot the whole data

plot(x, y, pch=rep(1:g, each=n), col=rep(1:g, each=n))
# label the points for different groups

## Henry’s Choice
set.seed(2021)
n <- 100000 # number of simulations
spin.shot <- replicate(n, sample(1:6, 2, replace=TRUE))
## Assume that the bullets are in chambers 1 and 2.
first.blank <- spin.shot[1,] > 2
prob1 <- sum(spin.shot[2,first.blank] <= 2) / sum(first.blank)
twoshots <- function() {
    first.shot  <-  sample(1:6, 1)
    second.shot  <-  ifelse(first.shot == 6, 1, first.shot+1)
    c(first.shot, second.shot)
}
shot.again <- replicate(n, twoshots())
first.blank <- shot.again[1,] > 2
prob2 <- sum(shot.again[2,first.blank] <= 2) / sum(first.blank)
c(prob1, prob2)

n = 10
res = replicate(n, sample(1:6, 2, replace=TRUE))
# assume that the bullets are in 1 and 2
sum(res[2,] <= 2 & res[1,] > 2) / sum(res[1,] > 2)

con = function(){
    first = sample(1:6, 1)
    second = ifelse(first == 6, 1, first + 1)
    return(c(first, second))
}

n = 10000
con()
res2 = replicate(n, con())
sum(res2[2,] <= 2 & res2[1,] > 2) / sum(res2[1,] > 2)

## Bertrand’s Box
set.seed(2021)
boxs <- c("GG", "GS", "SS")

boxcoin <- function(boxs){
    box <- sample(boxs, 1)
    idx <- sample(1:2, 1)
    coin <- substr(box, start=idx, stop=idx)
    return(c(box, coin))
}
boxcoin(boxs)

res <- replicate(n=100000, boxcoin(boxs))
sum(res[1,] == "GG" & res[2,] == "G") / sum(res[2,] == "G")

mean(res[1, res[2,] == "G"] == "GG")

mean(res[1, res[2,] == "G"] == "GG")

sum(res[1, ] == "GG" & res[2,] == "G") / sum(res[2,] == "G")

## 100 prisoners problem
set.seed(2021)
n <- 100 # number of prisoners
prisoners <- 1:n # prisoners' numbers
# simulate the procedure of randomly open 50 (n/2) drawers
open.random <- function(prisoners, n=length(prisoners)) {
    drawers <- sample(1:n, size=n, replace=FALSE)
    # randomly put prisoners’ numbers in the drawers
    pardon <- TRUE # initialize pardon to be true
    for (i in prisoners) {
        opens <- sample(drawers, n/2)
        # randomly open n/2 drawers
        if (!(i %in% opens)) {
            # if any prisoner does not find his number
            # all prisoners die
            pardon <- FALSE
            break
        }
    }
    return (pardon)
}
open.random(prisoners)

open.smart <- function(prisoners, n=length(prisoners)) {
    drawers <- sample(1:n, size=n, replace=FALSE)
    pardon <- TRUE
    for (i in prisoners) {
        opens <- rep(NA, n/2)
        opens[1] <- drawers[i]
        for (j in 2:(n/2)){
            if (opens[j-1] == i) {
                break
            } else {
                opens[j] <- drawers[opens[j-1]]
            }
        }
        if (!(i %in% opens)) {
            pardon <- FALSE
            break
        }
    }
    return (pardon)
}

open.smart(prisoners)
# survival probability of randomly open
mean(replicate(10000, open.random(prisoners)))
# survival probability of using the better strategy
mean(replicate(10000, open.smart(prisoners)))
