
# LECTURE/HW #6 - PROGRAMMING & LOOPING IN R


#1 WORKING DIRECTORY ---------------------
#a) Change your working directory to wherever you want it to be, e.g.
setwd("~/Desktop/psyc5541/jrl/hw6")

# b) What version of R are you using? Here, we use the function "help.search" to find the appropriate function.
help.search("version")
R.Version()

# c) What time is it? We use Sys.time() to find out, and save it as a character string called "class.begin". The parentheses around the assignment tell R to display the assigned object
(class.begin <- Sys.time())

# d) Read in a script I wrote that contains handy functions like info(x), LS(), and look(x). source() simply reads in another R script and runs every line of code
source("http://www.matthewckeller.com/R.Class/KellerScript2.R") 




#2 PASTE COMMAND ---------------------
# a) What packages are currently loaded in your R session?
search()

# b) use the paste() function to paste together s1, s2, s3, and s4. Call this object "myluckynum"
s1 <- "my name is"
s2 <-   "Matt"          # enter your name here
s3 <- "and my lucky number is"
s4 <- sample(1:10,1)
(myluckynum <- paste(s1,s2,s3,s4))

#the default sep argument is sep=" ". We can change that, e.g.:
(myunluckynum <- paste(s1,s2,s3,sample(1:10,1),sep="***"))

# c) set.seed() is a function that sets the random number generator seed. Seeds are typically automatically set by R to be some number between 1 and some huge number, depending on the system time. But you can manually set it too. This makes every call that requires a random number to be reproducible. Try it:
set.seed(s4)
rnorm(2)
rnorm(3)
set.seed(s4)
rnorm(2)
rnorm(3)
#or you can reproduce the same 5 normal variables as above all at once:
set.seed(s4)
rnorm(5)
#but this does NOT produce the same 5 normal variables because we called a new function in between
set.seed(s4)
rnorm(2)
runif(1)
rnorm(3)

# Set the seed to be 1234. Now, unlike previous classes, everyone will have the same exponential random numbers:
set.seed(1234)
rexp(10) #your's will start with 2.501758605. Am I right?

# d) Remember R's recycyling rule? Here are two examples:
2*1:20
c(2,4)*1:20

# if a function is called on two vectors of different lengths, R will "recycle" the elements of the shorter vector and apply them to the longer. If the shorter is *not* divisible by the longer, it still does so, but returns a warning (since most such cases represent a user error).
c(2,4,6)*1:20

#To see what's happening above:
rbind(c(2,4,6),1:20,c(2,4,6)*1:20)

#If the longer vector happened to be divisible by the length of the shorter vector, there would still be recycling but we'd get no warning, so BE CAREFUL with recycling!:
c(2,4,6)*1:21
rbind(c(2,4,6),1:21,c(2,4,6)*1:21)


# The same recycling principle applies to paste(). Use "paste" to make a vector that is "Y2","Y4",..."Y60". Call this vector of text values "ynames". See seq() function and the "sep" argument in paste to help you out. 
(ynames <- paste("Y",seq(2,60,2),sep=""))

#You can accomplish the same (with a default sep="" argument) using paste0() function:
paste0("Y",seq(2,60,2))

# e) Say that we want to rename the columns of the 80x4000 data.frame below with the names X1, X2, X3... X4000. We'll use the name() and paste() function to create exponentially distributed data, and we'll make 5% of the data be missing - can you understand how this was done??
set.seed(52)
dat <- matrix(rexp(80*4000),nrow=80)
na.locs <- runif(80*4000)<.05 #Even though these are "random" numbers, everyone should have a TRUE for the 7th element (e.g.). This is why R's random numbers are called "pseudo-random numbers"- they're not truly random (no computer actually does produce truly random #s - they all use seeds and follow an algorithm to determine the next set of #s following that seed. However, we can treat them as "random" because humans can't predict what they are going to be)
#We can look at the first 1000 na.locs like so (hopefully your options(max.print=1000))
na.locs # notice this is a logical vector
which(na.locs==TRUE)[1:20] #The elements of the first 20 TRUE's. You should have 7, 33, 48, 67, etc. I'm not sure you've seen the which() function, but it returns the index of every element that evaluates to TRUE. This can be a very useful function for converting a logical index to a numeric index

#we can ensure that about ~5% of data is missing. By default, if you enter a logical vector into a function wanting numbers, TRUE=1 and FALSE=0
sum(na.locs) #summing a logical produces the number of TRUE's (1's)
sum(na.locs)/(nrow(dat)*ncol(dat)) #.0497 are missing - pretty close to .05!

#Note that we can index a matrix (not a data.frame) with a vector (as done here) in addition to the usual X[row,column] indexing. By default, R does this BY COLUMNS, just like the matrix() function, which has default byrow=FALSE.
dat[na.locs] <- NA
dat[,1:3] #note the elements of the 7th, 33rd, 48th, & 67th row of the first column are missing.
mydata <- as.data.frame(dat)
names(mydata) <- paste("X",1:ncol(dat),sep="")

#if we want to change the NAs to -999's, we can't do that like this because you can't index a data.frame with a vector. It would be easiest to convert to a matrix then back to a data.frame. This is an advantage of matrices.
mydata[na.locs] <- -999 #(returns error)




# 3 FOR LOOPS ---------------------
#Loops are very useful tools to have in your toolbox. However, unlike C++ and other "compiled" languages, loops tend to be quite inefficient time-wise in R (and similarly for many "interpreted" languages). Thus, it's often wise not to use a for loop if there is a better (faster) alternative (such as vectorizing); we'll cover these below.

#a) Simple example of a for loop:
for (index in 1:5){
  print(index)
}

#b) Let's loop through the columns of dat to get the means of each column:
mymeans <- vector("numeric",length=ncol(dat))
for (IT in 1:ncol(dat)){
mymeans[IT] <- mean(dat[,IT],na.rm=TRUE)
} #end for loop
mymeans #note that it's too long to print them all; we could change this in options(max.print=XXX)

#c) We could also have looped through to get the means of the rows:
mymeans.rows <- vector("numeric",length=nrow(dat))
for (IT in 1:nrow(dat)){
  mymeans.rows[IT] <- mean(dat[IT,],na.rm=TRUE)
} #end for loop
mymeans.rows #because it is < than options()$max.print, we see the whole vector

#d) We can also make two different iterator. Say that we want to print out both the iterator that we're looping across and a different iterator
Main.It <- 1:5
(Second.It <- seq(2,10,2))
for (i in Main.It){
  i2 <- Second.It[i]
  print(paste("Main iterator is ",i," and second iterator is ",i2))
} #end for loop

#e) Or we can accomplish the above via algebra on the main iterator
Main.It <- 1:5
for (i in Main.It){
  i2 <- i*2
  print(paste("Main iterator is ",i," and second iterator is ",i2))
} #end for loop

#f) Here's a slightly more complicated example. Using the above, let's say we want the 1st, 3rd, and 8th column means put into one vector, and the 2nd, 4th, and 10th columns means put into another vector. THIS WON'T GIVE US WHAT WE WANT! Can you see the problem?
Main.It <- c(1,3,8)
Second.It <- c(2,4,10)
meansMain <- meansSecond <- vector("numeric",length=length(Main.It))
for (IT in Main.It){
  meansMain[IT] <- mean(dat[,IT],na.rm=TRUE)
  IT2 <- Second.It[IT]
  meansSecond[IT2] <- mean(dat[,IT2],na.rm=TRUE)
} #end for loop
meansMain
meansSecond

#g) Here's a simple solution to the above:
Main.cols <- c(1,3,8)
Second.cols <- c(2,4,10)
meansMain <- meansSecond <- vector("numeric",length=length(Main.cols))
for (IT in 1:length(Main.cols)){
  colmain <- Main.cols[IT]
  meansMain[IT] <- mean(dat[,colmain],na.rm=TRUE)
  colsec <- Second.cols[IT]
  meansSecond[IT] <- mean(dat[,colsec],na.rm=TRUE)
} #end for loop
meansMain
meansSecond




#HW Problem 1  ---------------------
# (a) Create a 2 row by 1 column plot (using par()) with a histogram of all the scores in dat above (which are exponentially distributed), and a histogram of mymeans below (means of n=80 of the exponentially distributed data). Title them with informative titles (what are we looking at?)
par(mfrow=c(2,1))
hist(dat)
hist(mymeans)
par(op)
#(b) What is the mean & sd of the original data (dat) and what are they for mymeans?
(mean(dat, na.rm = T))
sd(dat,na.rm = T)
(mean(mymeans, na.rm = T))
sd(mymeans, na.rm = T)
#(c) The original data in dat (top hist) was exponentially distributed with sd=1. The distribution of means was normally distributed with a smaller sd. Why? It's because the central limit theorem dictates that the sampling distribution of means (or sums) approaches normal as n grows, and that its SD will be sd/sqrt(n). Given this, what is the theoretical SD of means of n=80 from a distribution where SD=1, and how closely does your empirical SD of mymeans agree with this?

#let's check empirical SD 
mymeans_sd <- sd(mymeans, na.rm = T)
# now lets compute SD for n=80 and SD = 1 from above

new_sd <- 1/sqrt(80)

(diff <- mymeans_sd - new_sd)

#that is not all that large of a difference, so they agree quite well!
# (d) Use a for() loop to get the mean of every 50th column in "mydata". Save these means in a vector "myd.means"
mydata2 <- mydata
myd.means <- vector("numeric", length = ncol(mydata)/50)
i2 <- seq(1, ncol(mydata2), by = 50)
for(i in 1:length(i2)){
  col <- i2[i]
  myd.means[i] <- mean(mydata[,col], na.rm = T)
}
myd.means
#(e) Using a for loop, rename mydata to have the column names include information on missingness of each column. For example, if the first column has 2 missing, rename it "X1.2". X503.0 would mean no missings in the 503rd column. There are multiple ways to do this. 
for(i in 1:ncol(mydata2)){
  na_count <- sum(is.na(mydata2[,i]))
  colnames(mydata2)[i] <- paste("X",i,".",na_count, sep='')
}
names(mydata2)

# 4 ERROR HANDLING ---------------------
#Sometimes (this arises especially in functions, in loops, or in situations where you're running entire scripts in an automated way), you don't want an error to stop R running. Rather, you want to note that an error occurred but for R to continue to run. We can use the function tryCatch() to tell R to keep running even after an error. The basic syntax for tryCatch is:
tryCatch(expr=, #This is the code that might give us an error; often, it's multiple lines of code, so we can use curly brackets {} across multiple lines if so. If there is no error, tryCatch() returns the evaluated expr
         warning=#this must be a function that tells what to return from tryCatch() if R hits a warning in the expr above
         error=) #this must be a function that tells what to return from tryCatch() if R hits an error in the expr above

#Example of tryCatch:
(ans <- vector(mode="list",length=4)) #create an empty list
iterator <- list(10,-10,"a",exp(1))
for (i in 1:length(iterator)){
  input <- iterator[[i]] #note the use of double brackets to grab the element in a list. Do you understand why we do that? Try with single brackets if not
  ans[[i]] <- tryCatch(expr = {log(input)},
              warning = function(w) {paste("negative argument", input)},
              error = function(e) {paste("non-numeric argument", input)})
} #end for loop
ans #the for loop completed

#Example of the above for loop without handling the errors:
ans2 <- vector(mode="list",length=4) #create an empty list
iterator <- list(10,-10,"a",exp(1))
for (i in 1:4){
  input <- iterator[[i]]
  ans2[[i]] <- log(input)
} #end for loop
ans2 #Note that the for loop exited on the 3rd iteration upon hitting an error




# 5 WHILE LOOPS ---------------------
#a) Sometimes we don't know how long a loop will take - we just want to continue looping until some condition is met, or "while" some condition holds. For example, we might want to keep removing (an indeterminant) number of the most related individuals from a sample until all people are unrelated. All for loops can be rewritten as while loops. For example, we can rewrite the for loop in 3b above as a while loop. Note that we need to explicitly create an iterator and increase it within the loop here
mymeans.rows.while <- vector("numeric",length=nrow(dat))
IT <- 0
while (IT < nrow(dat)){
  IT <- IT + 1
  mymeans.rows.while[IT] <- mean(dat[IT,],na.rm=TRUE)
} #end while loop
rbind(mymeans.rows.while,mymeans.rows) #same as the for loop in 3
sum(mymeans.rows.while==mymeans.rows) #note that sum() on a logical returns the number of TRUE's

#b) While loops are useful for situations where we're running some algorithm and need to continue running until some condition is met, which is an unknown number of future iterations. Say we want 100 principal component models to run but some % of covariance matrices are not positive definite. We could use a while loop to continue until we get 100 PC models to run. This one is tough, so maybe we should go through it together in class:
r <- 5
SDs <- list()
IT <- 0
tot.valid <- 0
while (tot.valid < 100){
  set.seed(IT)
  z <- tryCatch(
  expr = {X <- matrix(NA,nrow=r,ncol=r)
  X[lower.tri(X)] <- runif(r*(r-1)/2,0,.8)
  X[upper.tri(X)] <- t(X)[upper.tri(t(X))] #these two lines are useful for creating symmetric matrices
  diag(X) <- 1 #and 1's down the diagonal, so X looks like a correlation matrix now
  v <- princomp(covmat=X,cor=TRUE)$sdev}, #end expr
  warning=function(w) {"PC warning"}, #end warning
  error=function(e)  {rep(NA,r)}  ) #end error and end tryCatch
  IT <- IT + 1
  valid <- (! is.na(z[1]))*1
  tot.valid <- tot.valid+valid
  SDs[[IT]] <- z
} #end while loop

#c) It is usually a good idea to place in a manual "break" into your while loops so that they don't accidentally go forever. To do this, use the if() function and break statement. The code above could be rewritten like thus (although the # of iterations here is too low, but that's for demonstration purposes)
r <- 5
SDs <- list()
IT <- 0
tot.valid <- 0
while (tot.valid < 100){
  set.seed(IT)
  z <- tryCatch(
    expr = {X <- matrix(NA,nrow=r,ncol=r)
    X[lower.tri(X)] <- runif(r*(r-1)/2,0,.8)
    X[upper.tri(X)] <- t(X)[upper.tri(t(X))] #these two lines are useful for creating symmetric matrices
    diag(X) <- 1 #and 1's down the diagonal, so X looks like a correlation matrix now
    v <- princomp(covmat=X,cor=TRUE)$sdev}, #end expr
    warning=function(w) {"PC warning"}, #end warning
    error=function(e)  {rep(NA,r)}  ) #end error and end tryCatch
  IT <- IT + 1
  valid <- (! is.na(z[1]))*1
  tot.valid <- tot.valid+valid
  SDs[[IT]] <- z
  if (IT > 10000) break #THIS IS THE NEW PART OF THE CODE
} #end while loop




#HW Problem 2  ---------------------
# (a) We'd like to know how long it takes in a truly random mating population of beetles for all males to end up paired with all females. A pair meets and if they are the same sex, they mate. Otherwise, the search again. How many times does this search have to go before each female is paired with a male? We can begin the problem like this (assume that number males= number females exactly)
N <- 100 #this is a "Wildcard" - we might decide to change this later, or to vary it to study how it influences our answer. It must be an even #. Keep it 100 here
POP <- c(rep(0,N/2),rep(1,N/2)) #0 is male and 1 is female (or vice-versa, doesn't matter)
index <- sample(x=1:N,size=N,replace=FALSE)
index1 <- index[1:(N/2)]
index2 <- index[((N/2)+1):N]
POP[index1]==POP[index2] #so in the first search, about half (~25 pairs) have found an opposite sex partner. How many searches does it take before all individuals are paired up with opposite sex partners? Inspired by the code above, try to figure this out for one particular random run. Use a while() loop to accomplish this and include a break statement so that you don't iterate more than 1000 times. Don't over-complicate this. All we really need to figure out after each loop is, "how many females (=males) remain unmated after the ith search?", then have the while loop redo the above using that number of females (males). So start with unmated.pairs <- N/2 outside the while loop, and then redefine unmated.pairs within the while loop.
i <- 0
N <- 100
pairs <- N/2
while (pairs > 0){
  i <- i+1   
  POP <- c(rep(0,N/2),rep(1,N/2)) 
  index <- sample(x=1:N,size=N,replace=FALSE)
  index1 <- index[1:(N/2)]
  index2 <- index[((N/2)+1):N]
  pairs <- sum(POP[index1]==POP[index2])
  N <- pairs*2
  if (i > 1000) break
}
# (b) Instead of just doing the above once, say we'd like to do it 1000 times in order to get the distribution of search times. Wrap your while() code above into a for() loop and provide a histogram of the search time. Notice that this is a nested loop - a while loop nested inside a for loop, where the for loop goes from 1:1000. Obviously, make sure to save the search time (IT) each time you redo this.
time <- rep(NA, 1000)
for(j in 1:1000){
  i <- 0
  N <- 100
  pairs <- N/2
  while (pairs > 0){
    i <- i+1   
    POP <- c(rep(0,N/2),rep(1,N/2)) 
    index <- sample(x=1:N,size=N,replace=FALSE)
    index1 <- index[1:(N/2)]
    index2 <- index[((N/2)+1):N]
    pairs <- sum(POP[index1]==POP[index2])
    N <- pairs*2
    if (i > 1000) break
  }
time[j] <- i}

hist(time)

# 6 USING APPLY AND SPECIALIZED COL/ROW FUNCTIONS ---------------------
#  The apply function of families (apply, tapply, sapply, lapply, rapply) are alternatives to for loops. They can be faster than for loops, but not always (e.g., when the datasets are very large, they can be slower). The main reason for using apply-family functions is that they're convenient and simplify your code. 

#a) apply() takes a matrix or dataframe and appies a function to all its rows or all its columns. E.g.:
(mydata.rowmeans <- apply(mydata,MARGIN=1,FUN=mean,na.rm=TRUE))
# you can also make functions (called "anonymous functions") on the fly - this is quite a powerful ability. E.g., here we get the mean again per column of mydata
(mydata.rowmeans2 <- apply(mydata,MARGIN=1,FUN=function(x) sum(x,na.rm=TRUE)/length(x)))
# the above isn't quite the same as using FUN=mean. Do you see why?
(mydata.rowmeans3 <- apply(mydata,MARGIN=1,FUN=function(x) sum(x,na.rm=TRUE)/length(na.exclude(x))))
cbind(mydata.rowmeans,mydata.rowmeans2,mydata.rowmeans3)
# OPTIONAL: if you want to get into some nitty gritty R arcania, run these:
#mydata.rowmeans2==mydata.rowmeans #yes, we know this
#mydata.rowmeans3==mydata.rowmeans #wtf?? Why aren't the all TRUE??
#all.equal(mydata.rowmeans3,mydata.rowmeans)  #see CRAN FAQ 7.31 to understand these otherwise puzzling results

# b) In HW1e, we used a for loop to rename the columns depending on the number of missing per column. We could easily have done this using apply (and much more quickly). Here, we demonstrate how to use apply to name the rows in a similar way:
rownames(mydata) #what are they now?
# Now we use the apply function to get the # missing per row and call this r.#.X where # is the row number and X is the NA number
(numnas <- apply(mydata,MARGIN=1,function(x) sum(is.na(x))))
rownames(mydata) <- paste("r.",1:nrow(mydata),".",numnas,sep="")
rownames(mydata)

# c) if you are interested in getting means or sums of rows or columns, there are even faster options that apply(). These are substantially faster because they've been optimized (precompiled in C++) to do one particular type of job very quickly. :
rowSums(mydata,na.rm=TRUE)
rowMeans(mydata,na.rm=TRUE)
colMeans(mydata,na.rm=TRUE)

#d) Sometimes the index across which we'd like to aggregate data isn't just per column or per row - sometimes we want to aggregate across a variable that changes. For example, we might want the mean of airquality across months, where month is a variable in a column of a dataset. We can do this using tapply:
search() #what is loaded in R now? See package:datasets
LS("package:datasets")
airquality #A dataset autoloaded in R
head(airquality)
(ozone.means <- tapply(X=airquality$Ozone,INDEX=airquality$Month,FUN=mean,na.rm=TRUE))

#e) Say that we have a continuous measure, like Temp, and we'd like to cut it into X number of levels (i.e., make it a discrete factor). We can do that using the cut() function:
?cut #look at the "Value" section - it tells us cut returns a factor by default unless the option labels=FALSE
(Temp.factor <- cut(airquality$Temp,breaks=5))
info(Temp.factor) #it's now a factor
head(airquality)
summary(Temp.factor) #since it's a factor, summary() tells us the N per level

#f) We could now use tapply to get ozoner levels across 5 different levels of Temp:
(ozone.means.temp <- tapply(X=airquality$Ozone,INDEX=Temp.factor,FUN=mean,na.rm=TRUE))

#g) We can also use the lapply() or sapply() functions. These are similar to the apply() function, but they apply the FUN across each element of a list (instead of each row/col of a matrix/data.frame). The difference between them is in their output. lapply() returns a list, whereas sapply() returns a vector, matrix, or array if possible.
(My.list <- list(a=rnorm(100),b=rexp(200),c=-20:50,d=seq(2,50,2)))
lapply(X=My.list,FUN=sd) #returns a list
sapply(X=My.list,FUN=sd) #returns a vector
lapply(X=My.list,FUN=quantile) #returns a list
sapply(X=My.list,FUN=quantile) #returns a matrix

#h) recall that a data.frame is just a type of list, with each column being its separate element. We can use lapply or sapply therefore on a data.frame to get functions on all columns (we could also have done this using apply()):
lapply(X=airquality,FUN=sd)
sapply(X=airquality,FUN=sd)
#Note we get some NA's. As above, we can supply additional arguments to sd() after the FUN argument
lapply(X=airquality,FUN=sd,na.rm=TRUE)
sapply(X=airquality,FUN=sd,na.rm=TRUE)

#i) we can also make up functions on the fly here. Say we want to know the number of values beween 0 & 2 for each element of the list:
lapply(X=My.list,FUN=function(x) sum(x>0 & x < 2))
sapply(X=My.list,FUN=function(x) sum(x>0 & x < 2))
sapply(X=My.list,FUN=function(bb) sum(bb>0 & bb < 2)) #same - "x" or "bb" are just arbitrary arguments to the function

#j) we could also get all the actual values (not just how many there are) between 0 & 2 using a function on the fly:
lapply(X=My.list,FUN=function(x) x[x>0 & x < 2])
sapply(X=My.list,FUN=function(x) x[x>0 & x < 2])
#Can you figure out why sapply() returned a list rather than an array in this instance?




#7 TIMING OF CODE ---------------------
# system.time() is a function useful for seeing how long a bit of code takes. Optimizing a script for speed is critically important when you're doing simulations and/or working with large datasets - it can mean the difference between the same job taking weeks versus minutes. Literally! To see how it works, run this:
system.time({
  n <- 1000
  mat <- matrix(rnorm(n*n),n,n)
  system.time(foo <- svd(mat))})
#"user" gives the CPU time spent by the current process (i.e., the current R session) and "system" gives the CPU time spent by the kernel (the operating system) on behalf of the current process. The operating system is used for things like opening files, doing input or output, starting other processes, and looking at the system clock: operations that involve resources that many processes must share. For most purposes, differentiating user vs. system is not important - we should just be considered with the total ("elapsed") time.



#HW Problem 3 ---------------------
#a) The dataset ChickWeight (see ?ChickWeight) has data from an experiment on the effect of diet on early growth of chicks. The column "Time" records when each measurement was taken in days since birth. Use summary() to figure out how many measurements there are for each time. Note that if you just do summary(ChickWeight$Time), you'll get summary statistics relevant to Time being a numeric variable. See if you can change the mode of ChickWeight$Time within summary in order to get the number of observations per Time.
head(ChickWeight)
summary(as.factor(ChickWeight$Time))

#b) What is the mean weight for each Time period? What is the mean weight of each Chick? What is the mean weight across the four levels of Diet?
tapply(ChickWeight$weight, INDEX = ChickWeight$Time, FUN = mean)
tapply(ChickWeight$weight, INDEX = ChickWeight$Chick, FUN = mean)
tapply(ChickWeight$weight, INDEX = ChickWeight$Diet, FUN = mean)
#c) Use apply() to count how many values are > 2 for each person (each row) in mydata (this will necessitate you creating a function on the fly within apply(). You'll also have to concern yourself with missing values). Call this vector "outliers". Then use names(outliser) and paste() to name each element of the vector "outliers" as "person.r.X" where r is the original row number (from 1:80) and X is the number of outliers. This one is  challenging and might take some monkeying around until you get it.
(outliers <- apply(mydata,1,function(x) sum(x>2, na.rm=TRUE)))
names(outliers) <- paste("person",1:80,outliers,sep=".")

#d) beaver1 and beaver2 datasets are small parts of a study of the long-term bodily temperature dynamics of beaver Castor canadensis in north-central Wisconsin. Body temperature was measured by telemetry every 10 minutes for four females, but data from one period of less than a day for each of two animals is used there. Here, we combine the two datasets (one per beaver) into a list:
beav <- list(beaver1,beaver2)
#In a single line of code, use lapply() and then sapply() (i.e., do it twice, once for each function) to find the number of rows for each of the two datasets in the "beav" list
lapply(beav,nrow)
sapply(beav,nrow)

#e) Using lapply() and then sapply(), find the means of each of the columns in the two datasets in the list "beav"
lapply(beav,colMeans)
sapply(beav,colMeans)
#f) Using lapply() and then sapply(), find the mean "temp" when "activ" is 0 vs. when "activ" is 1 for each of the two datasets in "beav". This one is a bit more challenging because you'll need to create a function on the fly (probably using tapply nested within lapply/sapply)
lapply(beav,function(x) tapply(x$temp,x$activ,mean))
sapply(beav,function(x) tapply(x$temp,x$activ,mean))

#g) Here is a simulated very large dataset:
C <- 1e2
N <- C*1e6
BIG <- matrix(rnorm(N),ncol=C) #may take a bit of time
info(BIG) #100 columns of 1 million rows each
#There are 3 ways we've covered for getting the means of columns of a data.frame/matrix. Do each one: using a for loop, using apply() and then using colMeans(). Call the resulting vectors bigmean1, bigmean2, and bigmean3. 
mean1 <- vector("numeric",length=ncol(BIG))
for (i in 1:ncol(BIG)){
  mymeans[i] <- mean(BIG[,i],na.rm=TRUE)
}
mean2 <- apply(BIG,2,mean,na.rm=TRUE)
mean3 <- colMeans(BIG,na.rm=TRUE)

#h) Compare the timing of the three lines of code above in g. Call the first time "time.loop", the second "time.apply" and third "time.colmean". Which is fastest?
(time.loop <- system.time({bigmean1 <- vector("numeric",length=ncol(BIG))
for (IT in 1:ncol(BIG)){
  mymeans[IT] <- mean(BIG[,IT],na.rm=TRUE)
} }))

(time.apply <- system.time({bigmean2 <- apply(BIG,MARGIN=2,mean,na.rm=TRUE)}))

(time.colmean <- system.time({mymeans3 <- colMeans(dat,na.rm=TRUE)}))
#colMeans is DEF the fastest



