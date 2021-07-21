#problem 1
longjump <- read.csv("longjump1.txt",sep="\t",header=TRUE)
table(longjump[5])
print(min(longjump[2]))
print(max(longjump[2]))
print(mean(longjump[2]))
plot((longjump[1]),(max(longjump[2])),"year","best result")

#problem 2
gender <- rep(c("female","male"),c(1835,2691))
admitted <- rep(c("yes","no","yes","no"),c(557,1278,1198,1493))
dept <- rep(c("A","B","C","D","E","F","A","B","C","D","E","F"),
            c(89,17,202,131,94,24,19,8,391,244,299,317))
dept2 <- rep(c("A","B","C","D","E","F","A","B","C","D","E","F"),
             c(512,353,120,138,53,22,313,207,205,279,138,351))
department = c(dept,dept2)
ucb <- data.frame(gender,admitted,department)
rm(gender,admitted,dept,dept2,department)
ls()
summary(ls)
GenderDept <- table(gender)
spineplot(GenderDept)
# there does seem to be a relationship between department and gender. They seem to be changing together.

#problem 3
print(summary(airquality[,1]))
print(summary(airquality[,2]))
print(summary(airquality[,3]))
print(summary(airquality[,4]))
print(summary(airquality[,5]))
print(summary(airquality[,6]))
boxplot(airquality)
density(airquality[,3])
# from the plots and tables, you can tell that the variables are changing at different rates