
# LECTURE/HW #5 - REVIEW OF MATERIAL FROM FIRST FOUR LECTURES
#In this lecture/HW, I won't introduce anything new. Use your knowledge (and scripts) from the first four lectures to help you answer all the problems below. Good luck!


#1 R'S WORKING DIRECTORY ---------------------
#Read in a script I wrote that contains handy functions like info(x), LS(), and look(x), The function source() simply reads in another R script and runs every line of code
source("http://www.matthewckeller.com/R.Class/KellerScript2.R") 

#a) Create a new folder somewhere on your computer and make this folder your working directory for this session
setwd("~/Desktop/psyc5541/jrl/hw5")

#2 VECTORS ---------------------
# a) Create a vector, length=500, of random, uniformly distributed numbers between -1 and 1. Assign this to an object called "rand.unif"
rand.unif <- runif(500, max = 1, min = -1)
hist(rand.unif)
# b) Create another vector of length=2000 of random, normally distributed numbers (mean=100, sd=15). Assign this to an object called "rand.norm"
rand.norm <- rnorm(2000, mean = 100, sd = 15)
hist(rand.norm)
# c) Create a long vector of length 2500 whose first 500 elements are from rand.unif and last 2000 are from rand.norm.  Assign this to an object called "long.vector"
long.vector <- c(rand.unif,rand.norm)
#d) What are the dimensions of "long.vector"? (Hint: dim() doesn't give you what you want for vectors)
str(long.vector)
#e) Check that you've created the vector properly; run summary statistics on the first 500 elements. Then do the same for the last 2000.
summary(long.vector[1:500]);summary(long.vector[105:2000])
# checks out


#3 MATRICES & WRITING FILES ---------------------
#a) Use the object "long.vector" to create a matrix of 500 rows and 5 columns. The first column is rand.unif. The next four are rand.norm.  Assign this object to be called "sim.matrix"
sim.matrix <- matrix(long.vector, nrow = 500, ncol = 5)
#b) Use the object "long.vector" to create another matrix of 500 rows and 5 columns. The first 100 rows should be rand.unif. The next 400 rows are rand.norm.  Assign this object to be called "sim.matrix.2"
sim.matrix.2 <- matrix(long.vector, nrow = 500, ncol = 5, byrow = 1)
#c) Write the matrix "sim.matrix" out to your working directory as a comma delimitted file. Do NOT include column or row names. Call this file "My.Simulated.R.Matrix.txt"
write.table(sim.matrix, row.names = F, col.names = F, file = "My.Simulated.R.Matrix.txt")
#d) Look at "My.Simulated.R.Matrix.txt" using your Terminal (e.g., the second tab next to Console here in Rstudio). Does it look as expected?
#yes
#e) Let's say we want to round all the elements to 3 decimal places. Write out "sim.matrix" again as a comma delimitted file, calling it "My.Sim2.txt", but with all elements rounded to 3 decimal places
write.table(round(sim.matrix, digits = 3), row.names = F, col.names = F, file = "My.Sim2.txt")
#f) Look at "My.Sim2.txt" using your Terminal (e.g., the second tab next to Console). Does it look as expected?
#Yes


#4 WORKING DIRECTORY, LISTING FILES, & READING FILES  ---------------------
#a) List the current files currently in your working directory
list.files()
# b) Create a character vector called "myfiles" that contains the names of all files in your working directory
myfiles <- list.files()
# c) Using the == operator, create a logical (TRUE/FALSE) vector that is TRUE when myfiles== "My.Simulated.R.Matrix.txt". Assign this to an object called "file.index"
file.index <- myfiles== "My.Simulated.R.Matrix.txt"
#d) Using "file.index" as an index, extract the element of "myfiles" that is equal to "My.Simulated.R.Matrix.txt".  Assign this element to an object called "file.name"
file.name <- myfiles[file.index]
#e) Here is an idea that we'll use more as the course progresses. Try to use the object "file.name"  within the read.table function, in order to read the "My.Simulated.R.Matrix.txt" file into R. Assign this to a new object called "Read.Mat"
Read.Mat <- read.table(file.name, header = F)
#f) What is the class of Read.Mat? What are its dimensions?
str(Read.Mat)
#g) Use the head function to look at the first 10 rows of Read.Mat
head(Read.Mat, n =10)


#5 RECODING   ---------------------
#a) Use the "colnames" function to rename the columns of Read.Mat. 1st column: "Level", 2nd column: "Y", 3rd column "Sex", 4th: "X1", 5th: "X2"
colnames(Read.Mat) <- c("Level","Y","Sex","X1","X2")
#b) Recode "Levels" to be an ordered factor, which is -1 if < -.333, 0 if -.3329 < "Level" < .3329  and 1 if > .333. Hint: recode to an ordered factor *after* changing the values.
Read.Mat$Level2 <- (Read.Mat$Level< -.333)*-1 +  (Read.Mat$Level> .333)*1 
Read.Mat[Read.Mat$Level < -.333,"Level"] <- -1;
Read.Mat[Read.Mat$Level > -.3329 & Read.Mat$Level < .3329, "Level"] <- 0
Read.Mat[Read.Mat$Level > .333,"Level"] <- 1
Read.Mat$Level
Read.Mat$Level <- factor(x = Read.Mat$Level, labels = c(-1,0,1))
str(Read.Mat)
#c) Create a new variable in Read.Mat, "Female", that is 0 if below the mean of Sex and 1 if above the mean of Sex
Read.Mat$Female <- (Read.Mat$Sex > mean(Read.Mat$Sex))*1
Read.Mat$Female

#6 BASIC DESCRIPTIVES & SUBSETTING  ---------------------
#a) Find the mean Y score for females and the mean Y score for males
mean(Read.Mat[Read.Mat$Female == 0,"Y"])
mean(Read.Mat[Read.Mat$Female == 1,"Y"])


tapply(Read.Mat$Y,Read.Mat$Female,mean,na.rm=TRUE)

#b) Using the function tapply(), find the standard deviations of Y for the three levels of the "Level" factor. Place this in a 3-element vector entitled "SD.Levels"
(SD.Levels <- tapply(Read.Mat$Y, Read.Mat$Level, sd))
#c) Using the apply() function, create another vector called "ind.means" that is the mean for each individual of Y, X1, and X2
ind.means <- apply(Read.Mat[c(2,4:5)], 1, mean)
#d) Cre                    ate a subset of Read.Mat this only includes rows 1, 3, 5, 7, 8, 9, & 10, and that only includes columns 1, 3, 4. Assign this to a new object called Read.Mat.subset1
Read.Mat.subset.1 <- Read.Mat[c(1,3,5,7:10),c(1,3:4)]
#e) Create a subset of Read.Mat this only includes females. Assign this to a new object called Read.Mat.female
Read.Mat.female <- Read.Mat[Read.Mat$Female == 1,]
#f) Create a subset of Read.Mat that only includes columns Y, X1, and X2 and only includes rows where the mean of Y, X1, and X2 is greater than 105. Assign this to a new object called "Read.Mat.105"
cols <- c("Y","X1","X2")
rows <- apply(Read.Mat[c("Y","X1","X2")], 1, mean) > 105
Read.Mat.105 <- Read.Mat[rows,cols]

#7 PLOTTING AND FOR LOOPS  ---------------------
#Create a 3x3 plot using a for loop. Each quadrant of the plot is the scatterplot between X1 & Y from a random subset of rows:
#   The top row (3 plots) plots X & Y for three different randomly selected rows of size = 10
#   The 2nd row (3 plots) plots X & Y for three different randomly selected rows of size = 30
#   The 3rd row (3 plots) plots X & Y for three different randomly selected rows of size = 150
#   Make sure to create best fit lines for each plot. Finally, after you've plotted these, change R's graphing options back to how they were beforehand. This one's tough but you can do it. Good luck!!
par(mfrow = c(3,3))
for (i in c(10,30,150)){
  Y <- sample(Read.Mat$Y,i)
  X <- sample(Read.Mat$X1,i)
  for(i in 1:3){
    plot(Y~X)
    abline(lm(Y~X))
  }
}
#8 REGRESSION   ---------------------
#a) Run a linear regression of X1 & X2 & Levels on Y. Assign this to an object "lin.mod"

#b) Get the summary of lin.mod. Assign this summary to be "lin.mod.summary"

#c) What are the effects being tested by the two Levels contrasts? Can you interpret them?

#d) What is the mode of "lin.mod.summary"?

#e) What are the names of the objects in "lin.mod.summary"?

#f) Add two new columns to Read.Mat. The first column is "fitted" which are the fitted values (y-hats) of lin.mod, and the second new column is "resids" which are the residuals of lin.mod. See ?fitted and ?resid.

#g) create a plot that is residuals (y-axis) plotted against fitted values (x-axis). Is there any concern of heteroscedasticity?

#h) Create a matrix called "reg.info" and place the intercept and slopes for lin.mod in the first column of "reg.info" and place the p-values in the second column of "reg.info". Hint: you can grab objects from lin.mod.summary using "$" - same as in data.frames (indeed, data.frames ARE lists). Alternatively, see ?coef.




# BONUS QUESTION   ---------------------
#Completing this question is OPTIONAL. We'll go over it next class period: Our statistical question for the day is this: Do artifactual univariate outliers in X change the type I or type II error rates in regression? Artifactual outliers are ones that occur due to miscoding, instrument error, etc. They have no relation to the variable in question. To answer this, we'll need to simulate some data. A "Monte Carlo" way to approach this problem is to 1st simulate "population" data and resample from it in a loop. This turns out to be much faster than creating the data anew each iteration of the loop. To check both types of error rates, we'll need to simulate null data as well as data given some effect size. For each iteration, we'll then save the coefficient, the Std. Error, and the p-value. We'll loop through 1000 times. At the end, we'll need to figure out the type I error rates (% of times pval<.05 in the null data) and the type II error rates (% of times p > .05 in the null data). We'll also need to compare this to data without outliers to gauge the true type I and type II error rates.

# a) IF YOUR LAST NAME BEGINS WITH A - H, YOU WILL CHECK THE TYPE I ERRORS. RUN THIS CODE TO CREATE THE POPULATION DATA
x <- rnorm(50000)
out <- (runif(50000)<.10)*rnorm(5000,sd=4)   # 10 % of data is rnorm with sd = 3, rest is 0
x2 <- x+out    # roughly 10% of data has a chance of being an outlier - i.e., this data is heavy tailed
y <- rnorm(50000)  # the y variable; unrelated to the x variables
pop.null <- data.frame(y=y,x.norm=x,x.outlier=x2)  #we've created a population where there is no effect, but outliers exist
pop.null[1:15,]   # look at the data

# b) IF YOUR LAST NAME BEGINS WITH I - Z, YOU WILL CHECK THE TYPE II ERRORS. RUN THIS CODE TO CREATE THE POPULATION DATA 
x <- rnorm(50000)
out <- (runif(50000)<.10)*rnorm(5000,sd=4)   # 10 % of data is rnorm with sd = 3, rest is 0
x2 <- x+out    # roughly 10% of data has a chance of being an outlier - i.e., this data is heavy tailed
y <- sqrt(.15)*x + sqrt(.85)*rnorm(50000)  # x accounts for 15% of the variance in y
pop.alt <- data.frame(y=y,x.norm=x,x.outlier=x2)  #we've created a population where there IS an effect, and outliers exist
pop.alt[1:15,]   # look at the data

# c) Now run this to set up the matrix that we'll store our stats of interest in each round:
results.matrix <- matrix(0,nrow=1000,ncol=6)
dimnames(results.matrix) <- list(paste("run",1:1000),c("beta.no","se.no","p.no","beta.outlier","se.outlier","p.outlier"))

# d) Now it's up to you. You need to do the following things: 1) We're going to do 1000 iterations. For each iteration, randomly sample 50 rows from your population. Run two linear regressions on this subsample. One is Y~x.norm and the other Y~x.outlier. Store the statistics of interest in the appropriate row of results.matrix. At the end, calculate the type I errors for the two alternative realities if in group 1, and the type II errors if in group 2. If you want to get fancy, you might also create histograms of the t-values from the two realities. ALSO, if you want, go ahead and do both type I and type II error rates - once you've done one, the other is easy.
#to get type-I errors, you want the proportion of tests that are significant (p < .05) in the null population (group 1)
#to get type-II errors, you want the proportion of tests that are NOT significant (p > .05) in the population where the alternative hypothesis is true (group 2)


# HOWEVER, before you start, *think* about the simulation. What will its outcome be? Provide your answer before you begin here:





