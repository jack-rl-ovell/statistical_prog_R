
# FINAL EXAM - please work independently. If you get stuck, it's OK to get hints from me or your classmates, but in no situation should you copy code directly or provide code to your classmates. Place all R code in a *.Rmd file and render it into an html or PDF. In the rendered document, include the questions (1a "Write a function..." etc.) in the text with your answers following each question (you should remove the # preceeding the questions though). Make the questions in bold text, and any of your own text in non-bold. Include all R code that YOU write, but no need to include the R code that I write below (although it should obviously be in the *.Rmd file). Include R output (plots, summaries or regression output, head of files, etc.) that is required for me to verify that you have answered the question correctly/well. Feel free to try to further format your final rendered document to be as aesthetically pleasing as you'd like (bonuses for really easy to read and understand documents).


# 1) FUN WITH FUNCTIONS AND DATA MANIPULATION --------------------------

#Here's how the data for question 1 is created. You need to run the following to simulate the data:
set.seed(123456)
num.fams <- 500
Famid <- paste(sample(LETTERS,num.fams,TRUE),sample(100:999,num.fams,FALSE),sep="")
num.per.fam <- sample(1:7,num.fams,TRUE)
Famid <- rep(Famid,num.per.fam)
n <- length(Famid)
Persid <- paste(sample(LETTERS,n,TRUE),sample(10000:99999,n,FALSE),sep="")

ht <- rnorm(n,60,8)
wt <- rnorm(n,140,30)
fam.inf <- rep(rnorm(num.fams),num.per.fam)
cne <- sqrt(.05)*scale(ht) + sqrt(.05)*scale(wt) + sqrt(.2)*fam.inf + sqrt(.7)*scale(rgamma(n,5))

Fam.data <- data.frame(FamID=Famid,PersID=Persid,ht=ht,wt=wt,ex=rpois(n,2))[order(rnorm(n)),]
Fam.data$wt[runif(n)<.05] <- NA
new.dat <- Fam.data[15,]
Fam.data <- Fam.data[-15,]

New.Fam.Data <- data.frame(FamID=Famid,PersID=Persid,cne=cne)[order(rnorm(n)),]
New.Fam.Data$cne[runif(n)<.15] <- NA
New.Fam.Data <- New.Fam.Data[-c(5,10,20,30),]


# a) Write a function called "insert" which inserts a column or row (depending on an argument from the user) into a data.frame or a matrix at some specified position (also an argument from the user). E.g., if I did this:
#newdat <- insert(dataset=x,inserted.vector=z,pos=5,col=TRUE)
#I would want the function to insert the vector z into the 5th column of x, shifting all the other columns over to the right. **For extra credit**, when col=TRUE, ensure that the name of the vector ("z" in this case) is also the name of the column in the returned dataset. E.g., in the above example, the 5th column of newdat would be "z" rather than "inserted.vector". For a hint for doing this extra credit, google, "In R, how to get an object's name after it is sent to a function?"

insert <- function(data,vector,pos,col){
  data2 <- data
  if(col==T){
    data2[,pos] <- vector
    names(data)[5] <- names(vector)
  } else{
    data2[pos,] <- vector 
  }
  return(data2)
}

# b) Write a funcion called "groups" which counts the number of occurences of each unique element of some factor (x) and puts those numbers in a vector of the same length as x. For example:
# x <- c(1,2,2,2,4,5,2,3,1)
# I want groups(x) to return: 2 4 4 4 1 1 4 1 2. Make sure that the function works on numeric, factor, or character classes. Do not use a loop to accomplish this, as that is too slow for large datasets.

groups <- function(x){
  x <- as.vector(x)
  tab <- as.data.frame(table(x), stringsAsFactors = F)
  v2 <- sapply(x,function(x)tab[tab$x==x,"Freq"])
  v2 <- unname(v2)
  return(v2)
}

# c) Use your function insert() to insert new.dat into the 50th row of Fam.data. Call this new dataset "Fam.data1"

Fam.data1 <- insert(data = Fam.data,new.dat,50,F)

#d) merge Fam.data1 and New.Fam.Data. Call this new dataset "Fam.data2". Include all data from both datasets. If an individual is missing in one dataset, make their value "NA" in the merged data. Make sure to output the number of rows of Fam.data2 into the rendered file.

Fam.data2 <- merge(Fam.data1,New.Fam.Data, all = T)
nrow(Fam.data2)

# e) Use your function groups() to create a vector to find out how many people are in each family in Fam.data2 (Famid is the ID for families). Record how long it took your function groups() to accomplish this task
start <- Sys.time()
groups(Fam.data2$FamID)
end <- Sys.time()
time <- end - start
time
# f) Use your function insert() to insert this vector (the number of indivduals per family you just created above) into the third column of Fam.data2. Call this new dataset Fam.data3

Fam.data3 <- Fam.data2
Fam.data3[,3] <- groups(Fam.data2$FamID)

# g) Sort the Fam.data3 so that families are together, and such that the largest families are on top. Call this new dataset Fam.data4

Fam.data3[order(-Fam.data3$ht),]

# h) If a family has 6 members, remove 1 family member and if they have 7 remove 2 (i.e., remove that row). Make sure to selectively remove those who have missing data in the cne variable. Otherwise, remove them at random. Call this new data.frame "Fam.data.reduced". Make sure NOT to hard code this - your code should work if we recreated Fam.data above with a new seed, such that the numbers in each family, missingness, etc. were different. Finally, check your answer by showing output in the rendered file of summary(as97tiy.factor(groups(Fam.data.reduced$FamID)))


Fam.data.reduced <- Fam.data3
Fam.data.reduced[Fam.data.reduced==6,"ht"] <- Fam.data.reduced[Fam.data.reduced$ht==6,"ht"]-1
Fam.data.reduced[Fam.data.reduced$ht==7,"ht"] <- Fam.data.reduced[Fam.data.reduced$ht==7,"ht"]-2
Fam.data.reduced <- Fam.data.reduced[!is.na(Fam.data.reduced$cne),]


#2) MULTI-LEVEL ANALYSES IN R --------------------------
#Here's how the data for question 2 was made. You do NOT need to rerun this; the data is in the zipped folder "FinalData.zip" which you need to download from Canvas and unzip into a folder "FinalData" in your working directory. If you have trouble doing this, however, you may decide to run the code below to recreate the exact same data (note use of set.seed()). Make sure you have a folder called "FinalData" in your working directory if you decide to recreate the data.

library(MASS)

set.seed(321)
beta.a <- mvrnorm(115,c(103,-.035),matrix(c(14,-.1,-.1,.005),nrow=2,byrow=T))
beta.b <- mvrnorm(102,c(103,.005),matrix(c(14,-.1,-.1,.005),nrow=2,byrow=T))
beta.c <- mvrnorm(118,c(103,.035),matrix(c(14,-.1,-.1,.003),nrow=2,byrow=T))
beta <- rbind(beta.a,beta.b,beta.c)
cond <- c(rep("A",115),rep("B",102),rep("C",118))
gender <- sample(c("M","F"),length(cond),replace=TRUE)
age <- sample(65:75,length(cond),replace=TRUE)
friends <- floor(rgamma(length(cond),shape=1.5,scale=2) + (gender=='F')*runif(length(cond),0,2))
id <- paste("SUB",sample(10000:99999,length(cond),replace=FALSE),sep='')
beta[,2] <- beta[,2] + (gender=='M' & cond=='B')*.035 #creating gender*cond int
beta[,2] <- beta[,2] + (gender=='F' & cond=='C')*.015 #creating gender*cond int
dat <- cbind.data.frame(id,cond,gender,age,friends,beta)
names(dat)[6:7] <- c('intcpt1','slope1')

dat$intcpt <- dat$intcpt1 + (gender=='F')*1 - (age-mean(age))*.2 + friends*.45 #expected starting EF
dat$slope <- dat$slope1 - (gender=='M')*.0005 - (age-mean(age))*.004 #expected cognitive decline
summary(lm(intcpt~cond+gender +age+friends,data=dat)) #just checking what the true model is for starting EF, without noise
summary(lm(slope~cond+gender +age+friends+cond*gender,data=dat)) #just checking what the true model is for cognitive decline, without noise
summary(dat)


setwd("FinalData")

for (i in 1:nrow(dat)){
 score <- dat[i,8] + dat[i,9]*1:50 + 8*rnorm(50) #creating the actual 50 observations with noise (8*rnorm(50)). Note that because of this noise, the observed starting EF and cognitive decline will differ from expectation.
 ro <- runif(50) < .02
 score[ro] <- score[ro] + rnorm(1,0,15) #creating outliers
 score[runif(50)<.06] <- NA #creating random missingness
 if (runif(1) > .09){ #this is simulating random dropout of ~9% of subjects
 write.table(score,file=paste('X',substr(dat[i,1],4,8),dat[i,'cond'],sep='.'),quote=FALSE,row.names=FALSE,col.names=FALSE)}
}

write.table(dat[,c(1,3,4,5)],'SubjectInfo.txt',quote=FALSE,row.names=FALSE,col.names=TRUE)




#2) Researchers are interested in whether keeping mentally active and engaging in challenging activities helps stave off cognitive decline among persons of retirement age. The take a sample of 300+ people aged 65-75 and follow them for 50 months (a bit over 4 years). The first (reference) group is not told to do any particular activities (Group A). A second randomly assigned group is assigned to play the computer game "Fortnite" (Group B) for 6 hours per week. The final group is assigned to play card games with friends 6 hours per week (Group C). At the beginning of each month, participants log into the study website and take an Executive Function (EF) test (the dependent variable). The study is finally finished and the researchers send you the raw data (in the form of 300+ flat files in the folder "FinalData". Each file contains 50 EF measures (from week 1 to week 50) for each subject. The files all begin with "X.", followed by the subject number, followed by the group that subject was in.

#Your job as a researcher is to understand the effects of the three conditions on cognitive decline. In particular: i) does either intervention stave off cognitive decline? ii) do these effects depend on on gender (e.g., does playing Fortnite help males more than females?), iii) do people with more friends have higher or lower EF at start? iv) Does number of friends have an effect on cognitive decline?

# You might notice that this is a very typical "hierarchical" model, something we did not covered in this course. I'm going to ask you to analyze this anyway. I'll help you through. The way we'll do it is NOT necessarily the best way to do it, which would involve using HLM (e.g., using the lme4 module). Nevertheless, we'll go through a very intuitive way to think about what these analyses are all about, and I suggest that this intuitive method we use below is the first way you should always analyze HLM data, before doing it 'properly' using an HLM routine, because the two answers should roughly agree, and if not, then you've probably done it the 'proper' way improperly!

# You will probably find this to be a challenging exercize. Hopefully you will use all the R skills you've gained up to this point to be able to solve this problem. BEST OF LUCK!! 

#a) In the "FinalData" folder, there are 300+ files. Pull each one into R and merge the data (by subject ID) with the "SubjectInfo.txt" data file, which is also in the folder (note that there are fewer files than rows in SubjectInfo.txt - some subject withdrew from the study at some point). The variable "friends" in SubjectInfo.txt denotes "number of close friends who you can confide in and who you talk to at least monthly". If subject data is not available in the individual EF files, throw out those subjects

subj.info <- read.table('~/Desktop/psyc5541/jrl/final/FinalData/SubjectInfo.txt',header = T);
files <- list.files('~/Desktop/psyc5541/jrl/final/FinalData/')
files <- files[-1]

mat <- matrix(data = NA, nrow = nrow(subj.info), ncol = 51)

for(i in 1:length(files)){
  dat.file <- files[i];
  num <- substr(dat.file,3,7);
  id <- paste("SUB",num,sep = "");
  tab <- read.table(dat.file);
  mat[i,1] <- id;
  mat[i,2:51] <- t(tab$V1);
}

df.dirty <- as.data.frame(mat)
df.clean <- df.dirty[!is.na(df.dirty$V1),]

#generate names for our prety new df
header <- vector(mode="logical",length = 50)
for (i in 1:50) {
  header[i] <- paste("E",i,sep = "")
}
names(df.clean) <- c("id",header)
#merge the two datasets
dat <- merge(subj.info,df.clean,by="id")


#b) perform a regression for each individual subject, where you regress the 50 EF scores on time (time <- 1:50). Save the intercept and slope for each subject in a matrix called "regdat". These will be our dependent (y) variables later. However, you don't want to save 'bad' regression data. Thus, you want to make any data point that has a high cook's D (cookd > .3) be missing. After that, if any subject is missing 6 or more EF scores in total, that subject's regression data should be set to "NA" and you should then rerun the regression analysis, saving the intercept and slope from *after* the highly influential points have been dropped. You'll need to automate all this in a loop. A few hints:
#i) see the function cooks.distance() in the car library
#ii) you'll probably be doing lm() on each row of a dataframe. If so, you'll need to convert your row to a class of object that R can do lm() on. Also, you can't do a lm(y~1:50) for example. You'll first need to make 1:50 a vector outside of the lm()
#iii) it will be a challenge to figure out how to make the right point(s) missing that has/have a high cookd, given the missingness in the data. There are probably many ways to do this. I did it by making a vector of the cook's d values, and using the names of each element (which is consistent, regardless of missingness) to find the right element to set to NA, rather than the element itself (which is off if there is missingness before that point).
#iv) for me to check your answer on this part, make sure to print out the summary(regdat) (along with your R code) in your rendered file.


time <- 1:50
regdat <- matrix(data =NA, nrow = nrow(dat), ncol = 3)
vec1 <- vector(mode="logical", length=nrow(dat))
for (i in 1:nrow(dat)) {
  y <- as.numeric(dat[i,5:54])
  model <- lm(y~time)
  d.vec <- cooks.distance(model)
  if(sum(d.vec > .3) > 0){
    log.vec <- d.vec > 3
    nums <-  names(log.vec[log.vec==T])
    cols <- paste("E",nums,sep="")
    dat[i,cols] <- NA
  }
  regdat[i,1] <- dat[i,1]
  regdat[i,2] <- model$coefficients[1]
  regdat[i,3] <- model$coefficients[2]
}
summary(regdat)


#c) Before we get to analyzing the slopes (cognitive incline/decline) and intercepts (starting points), we'd like to visualize a scatterplot for each individual. Create a PDF with as many pages as # of subjects. Each page should be a scatterplot of that subjects' time vs. EF (use dev.off() at the end to stop writing to the pdf() device). Also:
# i) make the points blue if male and red if subject is female
# ii) make the title of each graph be the subject's ID number followed by whether that subject is group A, B, or C
# iii) place the best fit line in each graph, in the red or blue color
# iv) place the text "B0 = xx" and "B1 = xx" in each graph, where xx is the correct intercept or slope. Try to place this in the same location (always in the upper left) of each graph. To do this, try using the vector that comes from the following syntax, which you should run after a plot has been produced
lim <- par('usr')
# v) Make sure to plot each subject, even if they're missing in the data above. 
# vi) Include the R syntax for how to do this in the rendered file, and place only the FIRST of these plots in your rendered file, as an example. DO NOT DO NOT DO NOT place all ~300 plots in the rendered file! You fail this class if you do that ;)

for(i in 1:nrow(dat)){
  pdf(file='~/Desktop/psyc5541/jrl/final/regPlts',
      width = 10,
      height = 4)
  x <- time
  y <- as.numeric(dat[i,5:54])
  sub.num <- substr(dat$id[i],4,8)
  info.indx <- grep(sub.num,subj.info)
  group <- substr(files[info.indx],9,9)
  if(dat$gender[i]=="F"){
    plot(y~x, col = "red", main = paste(dat$id[i],group))
    abline(lm(y~x), col ="red")
    legend("topleft", legend = c(paste("B0 =",model$coefficients[1],sep=" "),paste("B1 =",lm(y~time)$ccoefficients[2],sep=" ")), col="red")
  } else{
    plot(y~x, col = "blue", main = paste(dat$id[i],group))
    abline(lm(y~x), col="blue")
    legend("topleft", legend = c(paste("B0 =",model$coefficients[1],sep=" "),paste("B1 =",lm(y~time)$coefficients[2],sep=" ")), col="red")
  }
}
#d) now analyze the main data. Each subject's slopes will be a dependent variable, giving us info on cognitive change. Each subject's intercepts will be another dependent variable in a separate regression model, giving us info on where each person started before the intervention. Answer the questions: i) does either intervention stave off cognitive decline? ii) do these effects depend on on gender (e.g., does playing Fortnite help males more than females?), iii) do people with more friends have higher or lower EF at start? iv) Does number of friends have an effect on initial cognitive functioning or on cognitive decline?. Make sure you use R to graphically check your data and assumptions first, and show me your work. The final stage should be your inferential tests. What are your conclusions?

#e) show the empirical 95% CI for the effects of the interventions on cognitive decline, based on 1000 bootstrapped iterations.

#f) provide the empirical 2-tailed p-values for the two intervention effects using 1000 permutations. Note that this is an example of permuted multiple regressions, so you'll need to get the p-values twice, once for each effect(a total of 2000 permutations), using the residual approach, as we discussed in the bootstrapping/permutation part of the class.




#3) SIMULATION IN R --------------------------

#a) After you publish your findings from the above analysis in #2 in a top journal in your field, you receive criticism from the twitterverse that you did not control for non-random dropout. In particular, a group of colleagues (reasonably) think that people with the most cognitive decline may have been more liable to drop out of the study, and that this may have been especially the case for people in group A (people with decline in groups B & C were doing more fun activities and so may have stayed in despite cognitive decline). In essence, critiques are concerned about a dropout ~ condition*cognitive decline interaction, such that only those in group A dropped out if they had cognitive decline.

#Modify the simulation syntax for the problem provided above in order to conduct a fake data simulation of a worse case scenario, where all those who dropped out of group A were those who had the most severe expected cognitive decline. E.g., if 9 people in group A dropped out, then simulate it again such that the 9 with the most severe expected cognitive decline in group A drop out. Then present the main results of the slopes ~ condition + other variables to see what influence this would have even under the worst case. Note that in the simulation above, we simulated truly random dropout. You just need to change that assumption, create new data, and see what the effect is. This is an example of "sensitivity analysis" - seeing what our results would have looked like under a violation of an assumption.


#b) Do this again under a more realistic scenario where there is some less severe relationship between dropout and cognitive decline among those in group A. If you're inclined, you can use logistic regression to first model the probability of dropout as a function of condition*slope, and then use this probability to randomly select those to drop. If you're unfamiliar with logistic regression, you can simluate this in a simpler way, e.g., have the greatest 1/3 of decline have x% chance of dropping out, the middle 1/3 y%, and smallest 1/3 z%. Try to accomplish this such that the expected number of dropouts in group A = the observed # of dropouts in group A.
  
#HINT for both problems: concern yourself only with the expected cognitive decline (each individual's true slope), not the actual observed cognitive decline (which includes noise).

  
 


































































