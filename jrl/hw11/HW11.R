
# LECTURE/HW #11 - BOOTSTRAPPING & PERMUTATION TESTING IN R



# 1 Basics ------------------------
# a) Change your working directory to wherever you want it to be. Everything you write out (save) will go into this folder
setwd('~/Desktop/psyc5541/jrl/hw11/')
# b) load the car & MASS packages. Note: require() & library() are ~ same thing. The difference is trivial, but read ?require if you're curious
require(car)
require(MASS)

#c) #Read in a script I wrote that contains handy functions like info(x), LS(), and look(x), The function source() simply reads in another R script and runs every line of code

# d) list out all the datasets and functions that exist in the car package

# 2 Very Simple Simulation ------------------------
# a) First, simulate a population of size 100,000 of random normal data with mean = 100 and sd = 15. Call this "pop"
pop <- rnorm(100000,200,15)
# b) Now, take a single random sample of size 35 from the population above. Call it "samp"
samp <- sample(pop,35)
# c) What is the mean of your sample? What is it's standard deviation?
mean(samp)
sd(samp)
# d) If we only had this sample from which to make inference, what would the best estimate of the population mean be? What about the population SD? (i.e., pretend we didn't have knowledge of how the data was simulated, above)
#it would just be the above


# e) Based on your understanding of statistics, what is the standard error of a sampling distribution of means when n=35, sd = your best estimate of the sd, and mean = your best estimate of the mean? What will the shape of this distribution be? Just write your answers

#normal 
sem <- sd(samp)/sqrt(35)



# 3 Bootstrapping the mean ------------------------
# a) How well does the bootstrap approximate what the sampling dist of means is expected to look like theoretically? (i.e., such as that figured out in (2e) above? Take B=10,000 bootstraps of your sample (sample with replacement). Each time, figure out the mean of each resample and record it in a vector. After you're done, plot a histogram and a qq.plot of the bootstrapped sampling distribution of means.
B <- 10000
means <- vector(length = B)
for(i in 1:B){
  means[i] <- mean(sample(pop,35))
}
means
hist(means)
qqlot(means)
# b) what is the mean of your sampling dist of means? What is the SD of your sampling dist of means? What is its shape? How does this compare to the mean, sd, and shape that we would have expected of the sampling dist of means had we only had information on the sample?
mean(means)
sd(means)
#normal

# c) what is your conclusion about how bootstrapping compares to the traditional method of finding the sampling distribution properties in this simple case?
#bootstrapping works




# 4 Bootstrapping regression ------------------------
# Let's say we're interested in a question that is difficult to assess using classical statistics: are two regression slopes equal to one another? In this case, we'll use income and education (both are on a similar scale - percentages) in the Duncan dataset. 

#a) use R and ? to get some basic info on the Duncan dataset

?Duncan

#b) perform a regression predicting prestige from income and education
model.1 <- lm(prestige ~ income + education, data = Duncan)

#c) drop the datapoint that has the largest Cook's distance (that pulls the regression line the most). Call this new dataset "Duncan2". You will use Duncan2 from now on. This is not necessarily a recommendation that high Cook's D observations should be dropped! Base dropping on theory or knowledge of the design. Recall from lecture 3 that rule of thumb for Cook's D > 4/(n-p) is high; here our n = 45 and # parameters = 3

Duncan$cooks <- cooks.distance(model.1)
Duncan2 <- na.omit(Duncan[!Duncan == max(Duncan$cooks),])
Duncan2

#d) Let's say we're interested in whether the slope for education = slope income in Duncan2. Classical statistics is not going to help you here. Use bootstrapping to create a 95% confidence interval for the difference between the two slopes. Make B = 1000 to save time. Use set.seed(1) so we all get the same answer. 

set.seed(1)
B = 1000
diffs <- vector(length = B)
size <- nrow(Duncan2)
for(i in 1:B){
  new.dat <- Duncan2[sample(1:size,size,replace = T),]
  boot.model <- lm(prestige ~ income + education, data = new.dat)
  diffs[i] <- boot.model$coefficients[3]-boot.model$coefficients[2]
}
diffs


# e) What is the bootstrapped CI of slope differences? Does the confidence interval above include 0?
diffs.sorted <- sort(diffs)
CI <- c(diffs.sorted[25],diffs.sorted[975])
hist(diffs.sorted)

#yup

# f) What can you conclude inferentially about the null that they two slopes are the same?
#do not reject the null

# g) what is the shape of the sampling distribution of slope differences? Use hist and qq.plot

hist(diffs.sorted)
qqPlot(diffs.sorted)


X <- mvrnorm(1e3,c(0,0),matrix(c(1,.6,.6,1),nrow=2))

X1 <- X[,1]

X2 <- X[sample(1:nrow(X),1e3,F),2]

cor(X1,X2)

# 5 Permutation regression ------------------------
# a) Let's say we want a p-value for the null hypothesis that the two slopes above are equal in Duncan2 (where we've dropped the biggest Cook's D residual). Use permutation to derive a p-value. This is tricky because you want to accurately simulate data under the null hypothesis. How would you do that? This problem demonstrates that permutation is often far trickier than bootstrapping. Use Duncan2. Hint: I would standardize both education and income.

Duncan3 <- Duncan2
Duncan3$education <- scale(Duncan2$education)
Duncan3$income <- scale(Duncan2$income)
model.2 <- lm(prestige ~ income + education, data = Duncan3)
model.2

# b) Now we should consider carefully the null. In this case, if both slopes are the same, then it should be fine to switch (permute) whether a given subject's score is education or income. By doing this, we're creating a null distribution where the two slopes are guaranteed to be the same.

x <- cbind(Duncan3$income, Duncan3$education)
B <- 1000
diffs.2 <- vector(length=B)

for (i in 1:B){
  col1 <- sample(c(1,0),nrow(x),replace=TRUE)
  col2 <- (col1*-1)+1
  newcol1 <- col1*x[,1] + col2*x[,2]  #stole from key
  newcol2 <- col1*x[,2] + col2*x[,1]
  newx <- cbind(newcol1,newcol2) 
  coefs <- coefficients(lm(Duncan3$prestige~newx))
  diffs.2[i] <- diff(coefs[2:3])}

hist(diffs.2)
orig.dif <- diff(coefficients(lm(prestige ~ income + education,data=Duncan3))[2:3])
sum(abs(diffs.2) > abs(orig.dif))/length(difvec)  #this is the p-value associated with our null
# definitely not significant




# 6 Bootstrapping repeated measures regression (or t-test) ------------------------
# Sometimes there are dependencies in data (e.g., 2 scores from same person), and we want to make sure to keep those dependencies are kept intact when we bootstrap/permute. Here, we simulate data from n=100 people's depression scores before and after 6 weeks of cognitive behavioral therapy. Depression is scored from 0 (no depression) to 36 (highest depression level). People also are currently taking antidepressants (=1) or not (=0). Run the following:
set.seed(123)
n <- 100
CBT.effect <- rnorm(n,-1.5,8)
ANTIDEP.effect <- rnorm(n,-.75,6)
antidep.t1 <- rbinom(n,1,.2)
antidep.t2 <- rbinom(n,1,.2)
person.id <- 1:n
dep.score.t1 <- floor(rnorm(n,20,5) + ANTIDEP.effect*antidep.t1)
dep.score.t1[dep.score.t1<0] <- 0
dep.score.t1[dep.score.t1>36] <- 36
dep.score.t2 <- floor(dep.score.t1 + CBT.effect + ANTIDEP.effect*antidep.t2)
dep.score.t2[dep.score.t2<0] <- 0
dep.score.t2[dep.score.t2>36] <- 36
Dat.Time1 <- data.frame(id=person.id,antidep=antidep.t1,dep.score=dep.score.t1)
Dat.Time2 <- data.frame(id=person.id,antidep=antidep.t2,dep.score=dep.score.t2)

# a) You receive two data files, Dat.Time1 from people before CBT therapy and Dat.Time2 after. Your job is to derive bootstrapped CIs of the effect of CBT. You might consider putting the datasets together and into wide format so each row corresponds to one person. Make sure to control for antidepressant usage at both times (I'd merge the datasets and then residualize on this). From the bootstrap CI, can you infer that CBT is useful for decreasing depression?

dat.1 <- rbind(Dat.Time1,Dat.Time2)
dat.1$res.dep <- residuals(lm(dep.score~antidep,data=dat.1))
dat.1$time <- c(rep('t1',n),rep('t2',n))
dat.1.wide <- reshape(dat.1,idvar="id",direction="wide",timevar="time")

B <- 1000
cbt.effect <- vector(length=B)

for (i in 1:B){
  newdat <- dat.1.wide[sample(1:nrow(dat.1.wide),nrow(dat.1.wide),replace=TRUE),]
  cbt.effect[i] <- mean(newdat$res.dep.t2 - newdat$res.dep.t1)}

cbt.effect <- sort(cbt.effect)
(CI.CBT <- c(cbt.effect[25],cbt.effect[975]))
hist(cbt.effect)
abline(v=0,col='red')
#No


# 7 Confidence intervals for eigenvalues ------------------------
# Run the following to set up a var/covar matrix for simulation:
Sigma <- matrix(c( 1, .5, .5, .4, .8,
                  .5,  1, .4, .6, .5,
                  .5, .4,  1, .6, .6,
                  .4, .6, .6,  1, .7,
                  .8, .5, .6, .7,  1),byrow=TRUE,nrow=5)



# a) Use mvrnorm from the MASS package to simulate multivariate data (5 variables) that comes from the Sigma covariance matrix below. Make your simulated dataset have 1023 cases. the means of each variables should be 0. See ?mvrnorm

dat <- mvrnorm(1023,rep(0,5),Sigma)

# b) conduct a principle components analysis on the simulated data above. We haven't covered how, so you might google it to figure out how to do this

princomp(dat)

# c) use bootstrapping to find the 95% confidence intervals for *each* of the eigenvalues. In other words, you resample the dataset (with replacement), then do PCA and get the 5 eigenvalues. Do thta 1000 times and find the empirical distributions for each of the 5 eigenvalues.

R <- 1000
results <- matrix(NA,nrow=R,ncol=5)

for (i in 1:R){
  rand.dat <- dat[sample(1:nrow(dat),size=nrow(dat),replace=TRUE),]
  results[i,] <- princomp(rand.dat)$sd
}

eig1 <- sort(results[,1])
eig2 <- sort(results[,2])
eig3 <- sort(results[,3])
eig4 <- sort(results[,4])
eig5 <- sort(results[,5])

CI <- rbind(eig1[c(round(.025*length(eig1),0),round(.975*length(eig1),0))],
            eig2[c(round(.025*length(eig2),0),round(.975*length(eig2),0))],
            eig3[c(round(.025*length(eig3),0),round(.975*length(eig3),0))],
            eig4[c(round(.025*length(eig4),0),round(.975*length(eig4),0))],
            eig5[c(round(.025*length(eig5),0),round(.975*length(eig5),0))])





