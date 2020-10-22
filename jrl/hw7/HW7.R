
# LECTURE/HW #7 - FUNCTIONS, TEXT MANIPULATIO, SYSTEM COMMANDS, AND MORE

#1 PACKAGES ---------------------
#a) Change your working directory to wherever you want it to be, e.g.:
setwd("~/Desktop/psyc5541/jrl/hw7")

#b) Read in a script I wrote that contains handy functions like info(x), LS(), and look(x). source() simply reads in another R script and runs every line of code
source("http://www.matthewckeller.com/R.Class/KellerScript2.R") 

#c) What packages are currently installed?
search()

#d) Note that the above packages that auto-install can be changed in options()
options()$defaultPackages
#options(defaultPackages=c("text","vector","of","packages")) #Example - won't work obviously unless you have installed packages named "text", "vector", etc.

#e) What version of R are you using? 
R.Version()  #notice this is a list
R.Version()$version.string  #This should be R version 4.0.XX. If not, you really need to update R!

#f) In what directory are your libraries installed? 
.libPaths() #Right now, packages should be going into a folder for R version 4.0. When you update to a new version of R (e.g., 4.1.xxx), you'll need to reinstall your packages

#g) Which packages do you have currently installed for your current version of R?
MyLibs <- installed.packages()
nrow(MyLibs) #number of packages you have installed
head(MyLibs)
MyLibs[,c("Version","Depends")] #just looking at 2 relevant columns

#h) What time is it?
(class.begin <- Sys.time())



#2 SIZE OF OBJECTS ---------------------
#a) It is often useful to know how large your objects are in bytes. R stores all objects in RAM. This makes it fast, but limits the size of objects you can work with to the size of your RAM. This command checks the size of your objects:
mydata <- matrix(rnorm(1e5*10),ncol=10)
object.size(mydata) #in bytes (8 bits)
object.size(mydata)/(1024^2)  # size in MB
format(object.size(mydata),units="Mb") #a better alternative

#b) as you begin to move into high performance computing, it's important to know the basics of the machine you're working on, including it's RAM (memory) in GB (1GB =~ 1000 Mb; technically 1024 Mb). See if you can figure this out for your laptop. On a mac, it's apple icon -> About this mac, find "Memory" line



#3 IF/ELSE STATEMENTS ---------------------
#a) if statements evaluate a conditional. If true, R proceeds to do the command. If false, it does not.
(s4 <- sample(1:10,1))
if (s4 > 5) print("I have a big lucky number")
if (s4 <= 5) print("I have a small lucky number")
if (s4 <3 | s4 > 8 ) print("I have an extreme lucky number") #Using OR statement
(s5 <- sample(1:10,1))
if (s4 < 5 & s5 < 5) print("I have two small lucky numbers!") #Using AND statement

#b) note we can also do the same using else, which is a second function that must go with an "if" statement on the same line. 
if (s4 > 5) print("I have a big lucky number") else print("I have a small lucky number")
if (s4 <3 | s4 > 8 ) print("I have an extreme lucky number") else print("I have a moderate lucky number") #Using OR statement

#c) Often in R, we want to perform a set of code that should all be on one line, but for clarity, to do so on multiple lines. To do this, use curley brackets! CURLEY BRACKET RULE = Some functions in R, like if... else and for() loops require that either the entire expression be on a single line OR that multi-line commands are enclosed in curley brackets. This is because R reads line-by-line, and expects its work to be finished at the end of a line. The only exception = it keeps reading if within a curley bracket - i.e., all commands in a curley bracket are treating like they're on a single line separated by ;! They typical syntax is
if (s4 > 5){ 
  print("I have a big lucky number")
  } else { 
    print("I have a small lucky number")
  }

#Alternative (not as good of a coding practice for reasons explained later:
{if (s4 > 5) 
  print("I have a big lucky number") 
  else 
  print("I have a small lucky number")}

# This doesn't work the same. Do you see why?
if (s4 > 5) print("I have a big lucky number")
else print("I have a small lucky number")

#this one is a bit tougher to see why it doesn't work. It's because the "if" statement and the "else" statement aren't on the "same" line; print() and "else" are on the "same" line, but not "if" and "else"
if (s4 > 5) {print("I have a big lucky number")
else print("I have a small lucky number")}

#d) We can also have nested if else statements like so
(rand <- sample(-1:1,1))
{if (rand < 0) 
  print("Negative number")
 else if (rand > 0) 
  print("Positive number")
 else
  print("Zero")}

#The above is the same to R as the below (all 1 line within curly brackets):
if (rand < 0) print("Negative number") else if (rand > 0) print("Positive number") else print("Zero")

#While the above does work, it begins to break with multi-line commands performed after the if/else statements. A better alternative is to get in the habit of using curly brackets like this:
if (rand < 0) {
  print("Negative number")
   } else if (rand > 0) {
     print("Positive number")
      } else {
        print("Zero")}

#Thus, the trick to figuring out curly brackets is to place the line(s) of code you want run after the if() or else statement within curly brackets, and make sure to place any "else" or "else" if() statements immediately after the end of the curly bracket

#e) Nested curly brackets with if/else can get a bit complicated when we have multiple lines of code per if/else statement. This won't work because the one-liner doesn't work (we have two commands on one line), and it exemplifies why simply enclosing entire if/else statements within curly brackets isn't a good practice:
{if (rand < 0) 
  print("Negative number")
  ans <- -50
  else if (rand > 0) 
    print("Positive number")
  ans <- 0
  else
    print("Zero")
  ans <- 50}

#To make the above work,enclose any multi-lines of code following if/else within nested curly brackets, and make sure to start the "else" statement on the same line as the end of the previous curly bracket:
if (rand < 0){ 
  print("Negative number") 
  ans <- -50} else if (rand > 0){
    print("Positive number") 
    ans <- 0} else {
      print("Zero") 
      ans <- 50}
  
#Alternatively:
if (rand < 0){ 
  print("Negative number") 
  ans <- -50
  } else if (rand > 0){
    print("Positive number") 
    ans <- 0
    } else {
      print("Zero") 
      ans <- 50
      }



#4 SIMPLE FUNCTIONS ---------------------
#a) Writing your own functions in R is easy. Everything after the function() function needs to be on the "same" line, like an if()/else or for() statement:
zscore <- function(x) (x-mean(x))/sd(x)
rn <- rnorm(50)
zscore(rn)

#b) often, we want the commands in the function to span multiple lines. How do we accomplish that? Using curly brackets of course!
zscore2 <- function(x){
  num <- x-mean(x,na.rm=TRUE)
  den <- sd(x,na.rm=TRUE)
  num/den #by default, function() returns the output from the final line
}
zscore2(rn)

#c) As noted, functions returns the output from the final command. It's best to be explicit about exactly what we want returned using the return() function. So here, even though den is the final command, we still return num/den:
zscore2 <- function(x){
  num <- x-mean(x,na.rm=TRUE)
  den <- sd(x,na.rm=TRUE)
  return(num/den)
  den #by default, function() returns the output from the final command, but won't here because we explicitly used return() function inside the function(){}
}
zscore2(rn)

#d) notice above that we hard coded how to treat missing values. We can allow the user to have control over that by putting in arguments, here with a default of TRUE, which we then pass to the functions mean() and sd():
zscore3 <- function(x,na.arg=TRUE){
  num <- x-mean(x,na.rm=na.arg)
  den <- sd(x,na.rm=na.arg)
  ans <- num/den
  return(ans)
}

rn2 <- rn
rn2[10] <- NA #let's make the 10th score missing
zscore(rn2) #the default behavior for mean() and sd() is na.rm=FALSE
zscore2(rn2) #we hard coded na.rm=TRUE
zscore3(rn2) #we soft coded this and the user used the default na.arg=TRUE
zscore3(rn2,na.arg=TRUE) #we soft coded this and the user explicitly used na.arg=TRUE
zscore3(rn2,na.arg=FALSE)#we soft coded this and the user explicitly used na.arg=FALSE

#e) Let's make a function that converts fahrenheit to celsius
f2c <- function(F) {        # we're calling the function "f2c", and it has a single argument, "F"
  C <- (F-32)*5/9           # just write code like you normally would
  return(C)        }        # "return" function tells the function what to return.

f2c(85)
f2c(32)
f2c(-40) 

#f) Now we write a function called c2f that converts celsius to Fahrenheit
c2f <- function(C) {
  F <- (C*(9/5))+32
  return(F) }

#g) We can now create a function that calls other functions, and can also give defaults to this function, e.g:
  temp.conv <- function(Tmp,farhenheit=TRUE) {  # here, we're saying the default is to put in Far and get out Cel
    if (farhenheit==TRUE) {
      mytemp <- f2c(Tmp) } else {
        mytemp <- c2f(Tmp) }               # notice the use of curley brackets within curley brackets
    return(mytemp)}

temp.conv(85) #same as f2c(85)
f2c(85)
temp.conv(85,FALSE) #same as c2f(85)
c2f(85)

#h) We might not want to provide a default second argument e.g:
temp.conv2 <- function(Tmp,farhenheit) {  
  if (farhenheit==TRUE) {
    mytemp <- f2c(Tmp) } else {
      mytemp <- c2f(Tmp)}               
  return(mytemp)}

temp.conv2(85) #we get an error bc there is no default to the 2nd argument
temp.conv2(85,TRUE) #same as f2c(85)
f2c(85)
temp.conv2(Tmp=85,farhenheit=TRUE) #here, we are explicit about arguments
temp.conv2(85,FALSE) #same as c2f(85)
c2f(85)

#i) Look in your Global Env using ls(). There is no longer an object called "mytemp"? Objects created within functions are erased immediately.
ls()

#j) here, we use temp.conv to get the celsius of -50 to 100 F in increments of 1 degree and cbind these two columns together. I.e., col 1 is Far starting at -50 ending at 100, col2 is the corresponding Celsius
data.frame(Far=(-50:100),Cel=round(temp.conv2(-50:100,TRUE),3))




#5 MORE ADVANCED FUNCTION WRITING ---------------------
#a) recall our function zscore3:
zscore3 <- function(x,na.arg=TRUE){
  num <- x-mean(x,na.rm=na.arg)
  den <- sd(x,na.rm=na.arg)
  ans <- num/den
  return(ans)
}
#Recall also that the objects created in the function are erased immediately after the function is called. Technically, the function creates it's own temporary environment and then removes this environment. E.g.:
zscore3(rn2)
ls() #num, den, and ans are nowhere to be found in the .GlobalEnv

#Although the function zscore3 is pretty easy, in more complicated functions, we often want to look within this temporary environment created by the function to aid us in debugging it. We can do this using the debug() function:

debug(zscore3) #nothing happens yet - we've just told R to enter the debug mode (i.e., enter the environment created by the function and go line-by-line) whenever the function zscore3 is called hereafter:

zscore3(rn2)  #NOW we're in the debug mode. Hit enter to go through each line of the function. Type in the name of objects created in the function to see what they are. E.g., type in rn2 on the first, line - it's there. However, num hasn't been created yet. After hitting <Enter> on the line that creates num, you should be able to see what num is by typing in "num". Run through this a few times until you get what's happening here.

#Once we're done debugging a function, we need to tell R to NOT enter debug mode hereafter when calling this function. Do this using undebug()
undebug(zscore3)

#b) Say we create yet another function zscore4, but this one has a slight problem that makes the values returned from zscore4() unequal to those from zscore3(). Can you find it using debug()?
zscore4 <- function(x,na.arg=TRUE){
  mnx <- sum(x,na.rm=na.arg)/length(na.omit(x))
  num <- x-mnx
  den <- sd(x,na.rm=na.arg)
  ans <- num/den
  return(ans)
}

rbind(zscore4(rn2),zscore3(rn2))

debug(zscore4)
zscore4(rn2)
undebug(zscore4)

#c) As I've said before, error messages are the users' friends. They help the user understand why something isn't working. Warning messages are also friendly because the alert the user to something that the programmer thinks might be askew. Example:
temp.conv3 <- function(Tmp,farhenheit=TRUE) {
  if (mode(Tmp)=='character' | mode(Tmp)=='factor') stop("Yo! We need numeric or integer temps yo!") # notice the use of constructive error messages can really aid user end experience
  if (farhenheit==TRUE){
    if (Tmp < -100 | Tmp > 200) warning("Are you sure your temps are right? They seem too high/low")
    mytemp <- f2c(Tmp)}
  if (farhenheit==FALSE){
    if (Tmp < -73 | Tmp > 93) warning("Are you sure your temps are right? They seem too high/low")
    mytemp <- c2f(Tmp)}                      
  return(mytemp)}

temp.conv3(70)       # F to C
temp.conv3(21.1,FALSE) # C to F
temp.conv3("21.1")   # error message, does NOT run
temp.conv3(800)  #warning, but still runs  





#HW Problem 1 ---------------------
#a) Using if() and "else" statements, write a function "temp.conv4" just like temp.conv2 (below) but that returns "NA" AND outputs a warning (instead of an error) if the Tmp argument is of class factor or class character. Check that it works by running the code for temp.conv4() below after writing your function:
temp.conv2 <- function(Tmp,farhenheit) {  
  {if (farhenheit==TRUE) mytemp <- f2c(Tmp)
  else mytemp <- c2f(Tmp)}               
  return(mytemp)}


temp.conv4 <- function(tmp,farh){
  if(is.factor(tmp) | is.character(tmp)) {
   mytemp <- NA
   warning("temp is of class factor or char")
  }
  else if(farh==T){
    mytemp <- f2c(tmp)
  } else{
    mytemp <- c2f(tmp)
  }
  return(mytemp)}

temp.conv4("bung", T)
temp.conv4(69,F)
#b) R doesn't have a function for finding data's skewness and kurtosis. So should we write to the R support team and bemoan this fact? Nah, let's write these ourselves. You can find the formulas online. Make sure to include some useful error messages (e.g., if the user supplies data of the wrong class or not enough observations). Include an option in kurtosis which is whether or not you want it to be centered around 0 for a normal distribution (ie, kurtosis normally has a value of 3 for normal dist's. You can give a "stnd.kurt" option that allows users to make kurtosis have a value of 0 for normal dist's). Also, see if you can include a "na.rm" option!

# kurtosis is a measure of how heavily tailed a distribution is relative to the normal dist. It  is defined as the sum of each data point xi subtracted by it's mean ^x to the fourth divided by the number of data points, and all those divided by the SD cubed.

kurtosis <- function(x, na.rm =T){
  if(na.rm==T){
    data <- na.exclude(x)} 
  else{
    data <- x}
  n <- length(data)-1
  numerator <- sum((data-mean(data))^4)
  denom <- n*sd(data)^4
  kurt <- numerator/denom
  return(kurt)
}

skewness <- function(x, na.rm =T){
  if(na.rm==T){
    data <- na.exclude(x)} else{data <- x}
  n <- length(data)-1
  numerator <- sum((data-mean(data))^3)
  denom <- n*sd(data)^3
  skew <- numerator/denom
  return(skew)
}


#c) Write a function "explore" that takes in a rxc data.frame only and returns a 10x? matrix. The function figures out the following for each numeric/integer column and places these in rows. Row1 = Max;  Row2 = Min;  Row3 = Mean;  Row4 = Median;  Row5 = SD;  Row6 = Skewness;  Row7 = Kurtosis; Row8 = % missing. The columns represent the columns of the original data.frame that are numeric/integer. Make sure to include proper names at top of the returned matrix. Hint: to find the mode of each column, use sapply(x,mode). Test out your new function on data.frame that you create. How'd it work? This one is challenging.
explore <- function(x){
  data <- x
  m <- sapply(data, mode)
  type <- "numeric"
  data.2 <- data[,m %in% type]
  max <- apply(data.2, MARGIN = 2, FUN = max)
  min <- apply(data.2, MARGIN = 2, FUN = min)
  mean <- apply(data.2, MARGIN = 2, FUN = mean)
  median <- mean <- apply(data.2, MARGIN = 2, FUN = median)
  SD <- apply(data.2, MARGIN = 2, FUN = sd)
  skewness <- apply(data.2, MARGIN = 2, FUN = skewness)
  kurtosis <- apply(data.2, MARGIN = 2, FUN = kurtosis)
  percent.miss <- apply(data.2, MARGIN = 2, FUN = function(x){sum(is.na(x))/length(x)})
  cbind.data.frame(max,min,mean,median,SD,skewness,kurtosis,percent.miss)
}

#d) You've been using R for some time now. Surely there is something you'd like it to do. Write a simple function that does something that R currently doesn't do "out of the box".
#lets write a function that takes correlations of a datafram then returns the results and a plot
install.packages("corrplot")
library("corrplot")
corr.results <- function(x){
  result <- corr(x)
  corrplot(x, type = "upper", order = "hclust", 
           tl.col = "black", tl.srt = 45)
  return(result)
}




#6 GREP COMMAND ---------------------
#a) grep is a function that searches for text matches in character data. E.g.:
grep("h",c("aa","ab","ac","ad","ae","af","ag","ah","ai","hhhh","h","goodbye","hello")) #it returns a vector of the indices of which elements match the pattern (here, "h")

grep("hh",c("aa","ab","ac","ad","ae","af","ag","ah","ai","hhhh","h","goodbye","hello"))
#this can be very handy when you have text output (e.g., from Mx or GCTA or some output of another program) and want to grab just particular elements of it (e.g., after "-2LL" appears)


#b) the value argument can be handy if you want the match rather than the index
grep("h",c("aa","ab","ac","ad","ae","af","ag","ah","ai","hhhh","h","goodbye","hello"),value=FALSE) #it returns a vector of the indices of which elements match the pattern (here, "h")

grep("h",c("aa","ab","ac","ad","ae","af","ag","ah","ai","hhhh","h","goodbye","hello"),value=TRUE) #it returns a vector of the indices of which elements match the pattern (here, "h")


#c) here, we create a vector "loaded.data" of mode 'text' that contains the name of every dataset in package:datasets
loaded.data <- ls("package:datasets")

#d) We use grep to get the index of every dataset in "loaded.data" that has the letters "US" in it. Assign these indexes to vector "index" 
index <- grep("US",loaded.data)

#e) Use "index" to make a new vector "US.data", that returns the name of every dataset that has "US" in its name
(US.data <- loaded.data[index])

#f) of course, we could have done this more easily using the value argument
(US.data2 <- grep("US",loaded.data,value=TRUE))

#g) Say that we'd like to try to use a for() loop that loops through the four elements of "US.data", extracting the dimensions of each dataset. E.g., if we want the dimensions of the first datasets in US.data and don't want to type it out, this won't work:
dim(US.data[2]) #returns NULL - no dimensions for a character vector
dim(USArrests) #this one works though
#How do we make the character string "USArrests", which is US.data[2], be interpreted in R as if we'd typed USArrests? hmmm... that's a toughie! Keep this in mind; we'll get to it a bit later under eval(parse())



#7 GSUB AND SUBSTR ---------------------
#a) R has lots more text editing capabilities (mostly inherited from UNIX; the folks who invented UNIX were right down the hallway from the folks who invented S - the early version of R). Here are but two more. substr() extracts pieces from a text string at user-specified locations:
substr("WHaTuP?",start=2,stop=4)

#b) it works by recyling along vectors as well:
mm <- c("hello my name is","hello","Slim","Shady")
substr(mm,start=2,stop=4) 
substr(mm,start=7,stop=10) 

#c) demonstration (You must have run 1h above)
(class.end <- Sys.time())
difftime(class.end,class.begin)  #difftime function
#Here, we extract the actual times using substr:
(begin <- substr(class.begin,12,19)) 
(end <- substr(class.end,12,19))

#d) gsub replaces text with other text:
gsub(pattern="hello",replacement="goodbye",x=mm)

#e) Here, instead of something that looks like "2018-02-26 09:49:13 MST", we combine both paste() and substr() to create an object called "mytime" that coverts class.begin to look like: "Year: 2018 Month: 2 Day: 26 Time: 9:49".
(mytime <- paste0("Year: ",substr(class.begin,1,4),"  Month: ",substr(class.begin,6,7),"  Day: ",substr(class.begin,9,10),"  Time: ",substr(class.begin,12,16))) 



#8 PARSE & EVAL COMMANDS ---------------------
#a) parse() takes a text string and acts as if the user typed in that text string into R. eval() acts like the user hit the enter key - it evaluates whatever comes from parse. They are almost always used together in conjunction. E.g.
eval(parse(text="USArrests[1:20,]"))
#of course, in this case we could have just as easily have typed this:
USArrests[1:20,]

#b) This comes in handy when we want to convert a character string into a command. As we've seen, if we want the dimensions of the first datasets in US.data and don't want to type it out, this won't work:
US.data #(reminder of what this is - just a character vector)
dim(US.data[2])

#but here are two options that would work:
dim(eval(parse(text=US.data[2])))
eval(parse(text=paste("dim(",US.data[2],")",sep='')))

#This is actually an incredibly powerful tool. So long as we can create a text string in R, we can use that to run a command in R. When we're automating operations (such as in for loops), this is a great thing to have in our toolkit. (NOTE: the first object in US.data, is of class "time series" or "ts" - it has NULL dimensions)
dim(eval(parse(text=US.data[1])))
x <-  eval(parse(text=US.data[1]))
dim(x)
str(x)
length(x) #but it does have a length

#c) Sometimes commands require quotes (""). e.g., write.table(object, "name.of.file"). How do you use quotes in the middle of a text string surrounded by quotes? E.g.,
eval(parse(text="write.table(mydata,file="mydata.txt")")) # this doesn't work. Do you see why? The way we get around this is to use single quotes (') when we are inside double quotes:
eval(parse(text="write.table(mydata,file='mydata.txt')"))

#d) One thing you sometimes want to do is to automatically create new objects in R. E.g., say that for each data.frame in package:datasets, we want to create two datasets, one of even rows and one of odd rows (e.g., we want independent replications, although in truth you probably wouldn't want to assume even/odd rows are truly random; I'd use sample() instead). To create new objects, you COULD do this using eval(parse()), but I recommend to use the function assign() instead. To do this in a for loop:
loaded.data <- ls("package:datasets")
for (DAT in loaded.data){
  if(is.data.frame(eval(parse(text=DAT)))){
    EVEN.CMD <- paste(DAT,"[seq(2,nrow(",DAT,"),2),]",sep="")
    ODD.CMD <- paste(DAT,"[seq(1,nrow(",DAT,"),2),]",sep="")
    assign(paste(DAT,".even",sep=""),eval(parse(text=EVEN.CMD)))
    assign(paste(DAT,".odd",sep=""),eval(parse(text=ODD.CMD)))} #end if
} #end for loop
#Try to understand the code above - it's a bit advanced, but useful!



#9 SYSTEM COMMAND ---------------------
#a) the system() function is like eval(parse()), except that instead of being evaluated by R, the text string is evaluated by your system, e.g., by your Mac or PC notebook. Anything you can type into a terminal window in your computer, you can type within system() and get the same results.

# a) open a terminal window. In mac, Applications -> Utilities -> open Terminal.app. In pc, start -> run -> type "cmd" into window and hit enter. OR you can just use the handy "Terminal" tab over next to your Console in RStudio!

# b) navigate to your working directory. E.g.,  if mine is "/temp", I'd use "cd /temp" in mac or "cd C:\temp" in PC. List all files in your directory using "ls" in mac or "dir" in PC

# Now do the same thing from within R. Use both list.files() and the system command from with R. Try to assign each to be a character vector.
(files.list <- list.files())
files.sys <- system("ls")

# c) Move a file from /temp to a different folder in your computer using the terminal. Then, using system(), move that file back, issuing the command from within R. 

#NOTE NOTE NOTE: these commands WILL NOT WORK on your computer because the directories I specify don't exist on your computer. If you want them to work, change the paths to point to directories on your computer.

#E.g., in MAC terminal: mv /temp/this.file /Users/matthewkeller/Documents/Teaching/ would place "this.file" that's in the "temp" directory in the /Users/matthewkeller/Documents/Teaching/ directory.
#This would rename the file: mv /temp/this.file /temp/that.file
#This system command does the same:
system("mv ./mydata.txt /../ ")


#E.g., in DOS terminal: move C:\Users\matthewkeller\this.file C:\Users\matthewkeller\Teaching\ would place "this.file" that's in the "matthewkeller" directory in the C:\Users\matthewkeller\Teaching\ directory.
#This would rename the file: move C:\Users\matthewkeller\this.file C:\Users\matthewkeller\that.file
#This system command does the same (although I don't have a windows machine and cannot vouch that it works):
system("move C:\Users\matthewkeller\this.file C:\Users\matthewkeller\that.file ")


# d) The real usefulness of system comes from the ability to run other programs from within R dynamically. Two reasons to do this: 1) using R as a shell script where all your commands can be done within R (otherwise you have to remember the order in which programs were called, how they were called, etc), and 2) being able to run programs dynamically, e.g., if some test is significant, run one script in Mx, otherwise run a different one. Example (NOTE: again, this won't run properly on your system because Mplus and file paths):
for (run in seq(6,run.number*6,by=6)){
  actrun <- run/6
  eval(parse(text=paste("dat",run,"<- read.table('Z:/MattFacsim/Nov2006/Alpha4/Dat",run,"')",sep="")))
  eval(parse(paste("data<- cbind(group,dat",run,"[,1:9])",sep="")))
  write.table(as.matrix(data),file="data",append=FALSE, row.name=FALSE,col.name=FALSE)
  system('Mplus Nineitem.inp output.txt')
  mplus <- read.table("mplus.fac",header=FALSE,na.strings="999.000")
}

# e) Some common DOS & equivalent UNIX commands
#DOS              UNIX
#attrib  	        chmod
#backup 	        tar
#dir              ls
#cls 	            clear
#copy         	  cp
#del 	            rm
#deltree 	        rm -R
#edit 	          vi
#format 	        fdformat
#move / rename 	  mv
#type 	          less <file>
#cd 	            cd
#more <file> 	    more <file>
#md 	            mkdir



#HW Problem 2 ---------------------
# (a) Write out each data.frame in ls("package:datasets") to a subfolder called "data" in your working directory, and provide each name as "My.XXX" where "XXX" is the name of the data.frame. Make sure there are column names, but that there are no row names and no quotes around characters (see ?write.table). Also, create a r*3 data.frame that stores the name (column1), number of rows (column 2), and columns (column 3) of each data.frame that you wrote out. Call this data.frame "data.dimensions". Finally, store each data.frame that you write out in a list called "data.list", where each element of the list is the data.frame that exists in data.dimensions. Note that we can dynamically grow data.dimensions, which often isn't a great idea for RAM reasons, but here it will be trivially small. To do this, instantiate data.dimensions and data.list as shown below:

data.dimensions <- data.frame(name=NA,row=NA,col=NA)
data.list <- vector("list")
loaded.data <- ls("package:datasets")

i <- 0
for (d in loaded.data){
  if(is.data.frame(eval(parse(text=d)))){
    i <- i+1
    REAL <- eval(parse(text=d))
    write.table(REAL,paste("~/Desktop/psyc5541/jrl/hw7/My.",d,".txt",sep=""),quote=FALSE,row.names=FALSE,col.names=TRUE)
    data.dimensions[i,] <- c(d,nrow(REAL),ncol(REAL))
    data.list[[i]] <- REAL
    data.list[[i]] <- d}}

#b) Use a for loop and system() command to copy all the objects that are in the <working.directory>/data folder to your working.directory, renaming these files to be "My.copy.XXX" where XXX is the name of the data.frame. Check that the files are there from within R using list.files():

for (i in 1:nrow(data.dimensions)){
  (CMD <- paste("cp data/My.",data.dimensions$name[i]," ./",sep=""))
  system(CMD)
}
list.files()


# c) loop through all your objects in your global environment. If the object is of class data.frame, change it to a matrix. Otherwise, leave it as it is. (Challenging).

myobs <- ls()
for (i in 1:length(myobs)){
  cls <- eval(parse(text=paste("class(",myobs[i],")",sep="")))
  if (cls=="data.frame") eval(parse(text=paste(myobs[i]," <- as.matrix(",myobs[i],")",sep="")))}

# d) sometimes it is useful to suppress R's warnings (e.g., in a function). suppressWarnings() does this. Do the same as above, but this time suppress the warnings

myobs <- ls()
for (i in 1:length(myobs)){
  cls <- eval(parse(text=paste("class(",myobs[i],")",sep="")))
  suppressWarnings({
    if (cls=="data.frame") eval(parse(text=paste(myobs[i]," <- as.matrix(",myobs[i],")",sep="")))})}










