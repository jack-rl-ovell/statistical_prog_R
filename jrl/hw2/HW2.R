
###############Lecture/HW SCRIPT FOR R CLASS 2

#1 R'S WORKING DIRECTORY & SOURCING ------------------------
# Get working dirctory; asking R, "where is the default folder?"
getwd()  		
# if you want your working directory to be somewhere else, please change it using setwd()

#Let's go ahead and read in a script I wrote that contains handy functions dimx() info(), LS(), and look(). source() simply reads in another R script and runs every line of code
source("http://www.matthewckeller.com/R.Class/KellerScript2.R") 

#Note that we've pulled in additional functions that I wrote; we'll go over these as we go
ls()



#2 KEYBOARD ENTRY OF DATA ------------------------
# it is somewhat unusual to enter data by hand; usually you read in an existing dataset. If you are going to need to enter lots by hand, I suggest using a spreadsheet program. However, can enter data manually in R using "c" function
scan <- c(41,55,23,53,22,39,83,28,30,87,77,51,23,11,36,NA,53,18) 
country <- rep(c("Netherlands","Germany","Finland"),times=c(4,5,9))

#two ways we might create a dataframe - there is a subtle but important difference between these that we'll get to later
dat2a <- data.frame(scan=scan,country=country)

#dat2b is a matrix [cbind() by default makes things matrices]
dat2b <- cbind(scan,country)

#now we change this to a data.frame
dat2c <- as.data.frame(dat2b)          

#note that we could have done the last two lines of code in a single line of code:
dat2c <- as.data.frame(cbind(scan,country))   
#this type of nested coding (functions within functions) happens a lot in R).

#Note that dat2a, dat2b, and dat2c are different. You can see this using the str() function, which provides the structure of the object as well as the info() function I wrote:
info(dat2a) #data.frame
str(dat2a) #first vector is numeric, second is character
info(dat2b) #this is a matrix
str(dat2b) #note the str() output for a matrix is different: everything is a character (first line), and then str prints the "dimnames" or row and column names of the matrix. Because there are no row names, it prints "NULL" for rownames 
info(dat2c) #data.frame
str(dat2c) #everything is a character 

#The reason this happens is that the inner cbind() creates a matrix (dat2b), and everything in a matrix must be of the same mode. When the modes going in are mixed, R coerces them all to be the lowest mode (in order, character, then factor, then numeric). E.g.:
str(cbind(scan,country)) #everything is a character
#Then, when we wrap this around as.data.frame, all the columns are characters
str(as.data.frame(cbind(scan,country))) #everything is a character


#Recapitulating from HW1 how to subset data only gives us the first five rows of dat1
dat2a[1:5,]   
# only gives 2nd column of dat2
dat2a[,2]     
# first five rows of 2nd column
dat2a[1:5,2]  


#3 ANOTHER IMPORTANT MODE: LISTS ------------------------
# we've gone over 3 modes so far: numeric, character, and logical. The other most common modes are list and factor. A LIST is a mode for objects that can hold two or more different modes; data.frames are *types* of lists
mode(dat2a)	
# notice that the 1st vector in the data.frame is numeric and 2nd is character
str(dat2a)	# str() is a useful function
info(dat2a) #info() is a function I wrote that outputs the useful information of an object

#Let's create a list. Lists are the most general types of objects in R; they can be used to store objects that have multiple modes
my.list3 <- list(dat=dat2a,description="this is a dataset that I created for the lecture",y=rnorm(100),dir=getwd())
my.list3 # this output is super verbose
str(my.list3) # this output is more compact and easier to understand

# you can access list components using the "$" operator as with data.frames. Indeed, data.frames are just types of lists that appear to us as n-by-m rectangular datasets
my.list3$dat	
info(my.list3$dat)

# you can also access list components using the "[[" operator
my.list3[[1]]	

# Compare this to my.list3[1]. See a difference? Using the "[" operator on a list returns another list, whereas using the "[[" operator returns whatever is INSIDE that element of the list. 
my.list3[1]      #notice the "$dat" at the top that's not there for my.list3[[1]] - this is just a part of a list;
info(my.list3[1])
info(my.list3[[1]]) #(note that the mode of any data.frame is a list)

# you can add on to lists using the "$" or the "[[" operators
my.list3$newthing <- 1:50  
str(my.list3)
my.list3[[6]] <- "hello"
str(my.list3)


#4 A FINAL IMPORTANT MODE: FACTORS ------------------------
#R was designed for statistical analyses. Usually when we analyze something that looks like a character (e.g., "yes" "no"; "none",moderate","severe"), we want to treat those as categorical variables (and corresponding underlying contrast codes) so that we can analyze them. R used to automatically convert character vectors to factors in data.frames, when reading data, and other situations where it "guessed" you would want factors instead of characters. I HATED this behavior - I want to control my data, not have a program do automatic things for me. Luckily, it has been changed as of the newest R version!

# length 3 vector of mode "character"
people4a <- c("Mark","Jane","Lisa")  
# now we've reassigned 'people'
people4a <- rep(people4a,each=5)       
people4a

# people is a vector of mode "character"
info(people4a)	

# let's change it to a vector of "factor" class (the mode will be numeric because factors map labels to integer vectors - i.e., contrast codes)
people4b <- as.factor(people4a) 
info(people4b)

# differences between characters and factors
summary(people4b)
levels(people4b)
summary(as.character(people4b)) #Note that we changed the mode of people4b on the fly here

# this is how R codes this factor in a linear model
contrasts(people4b)	# notice that these are dummy codes

# ordered factors store ordinal categorical data
ord.people4b <- ordered(people4b,levels=c("Lisa","Mark","Jane")) 
contrasts(ord.people4b)
#Now the contrast assumes Lisa is lowest, Mark middle, and Jane highest. If we want to do the linear contrast, it would be -1, 0, 1 and the quadratic would be 1, -2, 1. R does something a bit more complicated than that, creating "orthogonal polynomial" contrasts. It won't matter a bit for ANOVA, but it would change the interpretation of regression coefficients over what you might expect. Honestly, if you're going to use factors in regression, I believe you're better off coding the contrasts yourself, which we'll get to later in the course.

#I'm not sure how often this will happen now (since R just changed its auto-conversion "feature"), but you will still probably encounter situations where R will  treat a vector that you think is numeric as though it is a factor. It's important to know how to change factors to numeric. For example, say you think of x4 as numeric but for whatever reason, it is a factor in R.
x4 <- as.factor(c(1,5,10,10))
str(x4)
sum(x4) #OK, good, R knows not to sum factors

#The obvious way to deal with this is to change the factor to a numeric, but this doesn't work:
sum(c(1,5,10,10)) #we all agree we should get 26
sum(as.numeric(x4)) #but R returns 9! Why??
as.numeric(x4) #as.numeric on a factor starts with 1 and goes up to nth level. Summing this gives 9. 

#THUS, to change a numeric vector that was incorrectly coded as a factor, change it to a character vector *first*, then change it to a numeric:
x4a <- as.character(x4)
x4b <- as.numeric(x4a)
sum(x4b)
#Or, in a single line
sum(as.numeric(as.character(x4)))



#5 MORE ON DATA SUBSETTING & LOGICALS ------------------------
#We'll create a data.frame called "x5"
ind.scores <- c(1,2,5,3,2,2,4,3,2,2,2,NA,5,3,3,4,5,4,5)
group <- rep(c("group1","group2","group3"),times=c(4,7,8))
(x5 <- data.frame(ind.scores=ind.scores,group=group))
str(x5)
#Note that group is a character (yay!). By default, older versions of R change characters to factors by default in data.frames or when reading datasets

#There are two ways we could change this so that character strings are converted to factors.
#First, do it when you create the data.frame (or when you read data in using read.table)
(x5a <- data.frame(ind.scores=ind.scores,group=group,stringsAsFactors=TRUE))
str(x5a)
#Second, if you want to change R's default behavior of changing character vectors to factors when, e.g., creating data.frames or when reading in datasets, you can set the default behavior using: 
#options(stringsAsFactors=TRUE)
#Don't do this now because I want you to get used to R's default behavior, but it's something to keep in mind going forward. 

# the function set.seed() assures that any call to a random number generator will generate the exact same set of random numbers. Here, we use it to assure that everyone in the class will have the same numbers come out of rnorm() below
set.seed(1234) #1234 is just a number I chose; could have been 1, or 2, or 1839308293850
rnorm(1)
rnorm(1) #check with your groupmates - everyone should get the same 2 numbers. Thus, set.seed creates the same random numbers for all functions that call a random number generator (RNG) going forward, through the whole script. But if you rerun parts of the script that call the RNG and your groupmates don't, then you'll have different numbers thereafter.
#set.seed() is very useful for creating reproducible simulation scripts
set.seed(11)
rnorm(1)
set.seed(22)
rnorm(1)
set.seed(11)
rnorm(1) #same as above
set.seed(1234)

# Now we want to create 3 new columns on x5. There are lots of nested functions here. Everyone seeing what's going on? If not, start with the inner fuctions and see what they're returning
extra.columns <- 3
x5[,(ncol(x5)+1):(ncol(x5)+extra.columns)] <- rnorm(nrow(x5)*extra.columns)
x5
str(x5)
# don't move on until you understand the above... break it up into pieces and run the smaller pieces to see it. It could be done more easily in this instance, but using variables like "extra.columns" allows you to automate & program later

#We can look at the column names of x5 in two ways:
names(x5)
colnames(x5)

# here's how you reassign data.frame names
names(x5) <- c("maindata","where","Time1","Time2","Time3")  
# Note that the above function, names(), works on lists (and therefore on data.frames). An equivalent function, "colnames", works on both matrices and data.frames
colnames(x5)[3:5] <- c("time1","time2","time3")  
# I know these trivialities are a pain in the ass. Better now than wondering why "names" doesn't work on your matrix later
x5

# We use logical (TRUE/FALSE) vectors to manipulate data all the time. It really is a fundamental R skill to get good at, which is why we cover it again here. Recall that TRUE/FALSE vectors can be used to subset data (we can save space by typing T, not TRUE; F, not FALSE)
rows.to.get <- c(T,F,F,F,F,F,T,T,T,T,T,F,F,F,T,T,F,T,T) 
x5[rows.to.get,]
#just by using a ! in front of a logical, we get the opposite logical
rows.to.get2 <- !rows.to.get  
rows.to.get;rows.to.get2
x5[rows.to.get2,]
x5[!rows.to.get,] #identical

# To illustrate, I'll use a logical to make a new dataset that only has data from group3 in it. Step 1 is to make the logical vector that I need
(group.I.want <- x5$where=="group3"   )
# step 2 is to choose only those rows where group.I.want is TRUE
(germ.dat5 <- x5[group.I.want,]) #it worked
# we could have done the above in 1 line of code
(germ.dat5 <- x5[x5$where=="group3",]    )

# Second illustration: choose only those row where maindata > 3 & time1 >0
(rows.i.want <- x5$maindata > 3 & x5$time1 > 0)
(new.dat5a <- x5[rows.i.want,])

# Third illustration: a more complicated conditional. Choose rows for which "where" is NOT group3 (using !=) unless time3 is > 1
(new.dat5b <- x5[x5$where != "group3" | (x5$where=="group3" & x5$time3>1),]  )

# Fourth illustration, we can accomplish the above more simply but somewhat more opaquely:
(new.dat5c <- x5[!(x5$where=="group3" & x5$time3<1),]  ) #do you understand why?

# Fifth illustration, as above, but only columns 1 & 3. Multiple options:
(new.dat5d <- x5[!(x5$where=="group3" & x5$time3<1),c(1,3)]  ) #do you understand why?
(new.dat5d <- x5[!(x5$where=="group3" & x5$time3<1),c(T,F,T,F,F)]  ) #do you understand why?

#Sixth illustration, as the second illustration above, but time1 < 0. Note how R deals with the "NA" here in maindata. This is because rows.i.want2 has an "NA" in it, and indexing with an NA returns NA for the entire row:
(rows.i.want2 <- x5$maindata > 3 & x5$time1 < 0)
(new.dat5e <- x5[rows.i.want2,])
#If this behavior is undesirable, you can explicitly tell R to not include NAs by using is.na() function like so:
(rows.i.want3 <- x5$maindata > 3 & x5$time1 < 0 & !is.na(x5$maindata)) 
(new.dat5f <- x5[rows.i.want3,])
#Compare rows.i.want2 to rows.i.want3:
rbind(rows.i.want2,rows.i.want3) #the "NA" in the 12th element of rows.i.want2 is FALSE in rows.i.want3

#Seventh illustration, you can also get around the NA rows by using the function subset()
(new.dat5g <- subset(x5,subset=rows.i.want2))
#this worked even though rows.i.want2 had an "NA" in it. subset() skips these rows, which is usually desirable
rows.i.want2

#Subset can also be used to grab specific columns
(new.dat5g <- subset(x5,subset=rows.i.want2,select=c(1,3)))
#Or equivalently you can name the columns
(new.dat5h <- subset(x5,subset=rows.i.want2,select=c("maindata","time1")))





#HW Problem 1  - inoculating you from some common R frustrations ------------------------
# (a) Enter in by hand the following 15 scores into a vector called "score": 14,22,23,19,50,7,20,38,20,14,18,26,NA,19,33
score <- c(14,22,23,19,50,7,20,38,20,14,18,26,NA,19,33)

# (b) Let's say that the first 7 scores are from the US, the next 5 from Mexico, the last 3 from Canada. You could type the 15 scores manually into a vector called "country", but do it more cleverly by using the rep() function, like I did above. see ?rep
country <- rep(c("US","Mexico","Canada"), times = c(7,5,3))
# (c) Create two new data frames, "hw1ca" using data.frame() function and create "hw1cb" using as.data.frame(cbind()). Both have column 1 as being score and column 2 being country. THEN CREATE AN OBJECT "ans1c" that is just a text that briefly explains how and why the two dataframes you just created are NOT identical. Understanding why these are not identical can save you a lot of R pain later. Hint: matrices all must have the same mode! If the modes of the columns are mixed, matrices force them to be the same. Does this help you understand why the above are different?
hw1ca <- data.frame(score = score, country = country)
hw1cb <- as.data.frame(cbind(score,country))
ans1c <- "The reason that these two are different is because when using cbind(), the function converts any vector it recieves to characters. This happens because when cbind() writes data to a matrix it will convert the data to the lowest mode (i.e. characters)"
# (d) Note that the 2nd variable in hw1ca (country) is now of class "character". Change it to a factor (i.e., reassign hw1ca$country) using one of the "as.xxx" series of functions. After you've changed it, use a function on hw1ca to check that you've done it correctly.
hw1ca$country <- as.factor(hw1ca$country)
str(hw1ca)
# (e) Now *in a single line using data.frame() function*, create hw1e, ensuring that the variable "country" is actually of mode "factor" rather than mode "character". See ?data.frame for an option in data.frame() that will allow you to do this.
hw1e <- data.frame(score = score, country = country, stringsAsFactors = 1)
# (f) Check that the modes of the variables of hw1e are what you want
str(hw1e)
# (g) Create a new object called "ans1g" that is the sum of the first column (score) of hw1ca. You'll need to figure out how to deal with missing values (NA) to get a valid answer. Hint: *read* those help pages!! Try ?sum
ans1g <- sum(hw1ca[,1],  na.rm = 1)
# (h) Create a new object called "ans1h" that is the sum of the first column of hw1cb (from c above, where you created hw1cb using as.data.frame(cbind())). It shouldn't work if you just use sum(). Why? You can't sum factors/characters. Try to nevertheless figure out a way to get the sum of hw1cb[,1]. Look at the end of section 4 for a hint of how to do this. Ensure that ans1g==ans1h is TRUE.
#tried sum(hw1cb[1]) in console
ans1h <- sum(as.numeric(as.character(hw1cb[,1])), na.rm = T)
ans1h == ans1g
# (i) Use the logical operator == to create a logical vector named "hw1j.tf" that is TRUE if hw1e$country is "Canada" and FALSE otherwise.
hw1j.tf <- hw1e$country == "Canada"
# (j) In a single line of code, create an object called "hw1j" that uses the logical vector hw1j.tf to find the mean score for Canada in one of the datasets above (hw1ca,hw1cg,or hw1e). See ?mean to figure out how to deal with NAs. 
hw1j <- mean(hw1ca[hw1j.tf,1], na.rm = T)

#6 BRIEF INTRO TO R's "SEARCH PATH" ------------------------
# This might be a little complicated at 1st... try to understand what's going on, then try asking your classmates/instructor. The function search() tells us the packages and data.frames that R searches through to find particular objects
search()
#the first element is ".GlobalEnv" - this is the default location R searches and is where all the objects you've created exist. By R's reasoning, the .GlobalEnv is position 1, or pos=1. The next place R looks is in the rstudio package ("tool") or pos=2, then in the stats package (pos=3), etc. So R search path goes 1, 2, 3, 4, ... For reasons we won't go into, pos=-1 is the *default* environment, which in all cases in this class is the same as pos=1.

#We can see what objects are in these environments using ls()
ls(1) #in .GlobalEnv that we've created
ls(3) #all the objects (mostly functions) in the stats package

#We can type in the name of an object in any position to see what it is, including functions. E.g., the last place R looks is in package:base in 10th position:
ls(10)
#Or you can explicitly name the search location:
ls("package:base")
#The last function there is zapsmall:
zapsmall
#R tells you how functions are written by typing in the function names. R is mostly written in R!

#When we type a name into console, R sees if it can find this object in any of these locations. No object "maindata" exists!
maindata	
# it is a part of the data.frame "x5"
x5$maindata		

# We can add a data.frame to the search path. This function 'attaches' the variables of x5 to the search path, which means that it makes "maindata" available to us, so we don't have to type "x5$maindata":
attach(x5)		
maindata		
# this function shows R's "search path" - R looks in .GlobalEnv first, then in "x5", etc.
search()		

# We've masked "maindata" that exists in pos=2 with a new object "maindata" that exists in .GlobalEnv
maindata <- 598	        
maindata
#This is confusing. Now we have TWO "maindata" objects, one in the .GlobalEnv (the default) and another in the second position ("x5"). We can see this using the ls() function:
ls(1);ls();ls(-1) #all identical, showing the .GlobalEnv
ls(2)

# removes the first "maindata" R finds in .GlobalEnv
remove(maindata)	
maindata

# can't find it... but it exists!! What's going on here???
remove(maindata)

# look at the defaul behavior of remove()... it looks in the pos=-1 default (.GlobalEnv) only
?remove			
# *now* we've removed it
remove(maindata,pos=2)

# R has searched through all pos's & can't find maindata anymore
maindata                
# still here of course
x5$maindata		
# removes x5 from the search path
detach(x5)	#detach(2) works the same by removing whatever is attached in pos=2	
# back to normal
search()		
x5$maindata
maindata

#Finally, when we attach an object with names that are already in .GlobalEnv, R will give us a warning:
attach(dat2a)
#country and score were already in .GlobalEnv and R told us so. Let's detach this before we get into trouble:
detach(dat2a)

# Some people like to use attach() when working on data.frames so that they can just type in the variable when they want it. I recommend AGAINST using attach() in general. Having multiple variables of the same name in different environments can lead to confusion. E.g.: you change maindata in pos=1 but not in x5... 



#7 WRITING DATAFILES OUT ------------------------
# this writes out the datafile as an R object to be read back into R later
save(x5,file="country.RData")

#Note where these files are written by default:
getwd()
#You can chane this default by being explicit about where the file should be placed like so:
save(x5,file="~/Documents/MattFiles/myRstuff/country.RData") #(won't work on you machine)
#Note that "~" on mac/linux is short for "/Users/<your.user.name>". 

# writing data out as a flat file that it can be opened by R or by any other  program
write.table(x=x5,file="country.data")	
# see what the defaults and options are for "write.table"
?write.table			       	
# an alternative
write.table(x=x5,file="country.data.norownames",col.names=TRUE,row.names=FALSE,quote=FALSE)   
# another alternative 
write.table(x=x5,file="country.data.diffna",na="-99",col.names=TRUE,row.names=FALSE,quote=FALSE)   
# tab delimitted
write.table(x=x5,file="country.data.tabbed",sep="\t",col.names=TRUE,row.names=FALSE,quote=FALSE)   
# * delimitted
write.table(x=x5,file="country.data.starred",sep="*",col.names=TRUE,row.names=FALSE,quote=FALSE)   

# you can do functions on data before its written out
write.table(x=cbind(x5[,1:2],round(x5[,3:5],2)),file="country.data.rounded")

# write is a "primitive" function - mostly used internally rather than by users, but fast. Here, it will return an error. *Read* what the error message says! write() doesn't like writing out lists
write(x=x5,file="country.data.write") 
# this works... 
write(x=as.matrix(x5),file="country.data.write")  
write(x="Hi all, this is something I'm writing to help out in the R class. 
      Lots of fun is R. Strong is the power. Like the wookies you do.",file="mytext")



#HW Problem 2 ------------------------
# (a) Open up each of the "country.data.xxx" files created in #7 above in a text editor (like Notepad or TextEdit) and in your favorite spreadsheet program (like Excel). If you can, also look at them using the "less" command in a unix terminal (preferred). Compare each one with the syntax above to see what each is actually doing. Note that you cannot look at the .RData file (it is an R proprietary file) and also note how ugly "country.data.write" is... "write" is still a useful function but its user-end 'ease of use' is low


# (b) In a single line of code, write out a subset of x5 file named "group.subset" that excludes "group1" rows UNLESS time1 > .05. Write out column names but not row names.
write.table(x5[!(x5$where=="group1" & x5$time1<=.05),],"group.subset",col.names=TRUE,row.names=FALSE)

# (c) Create a character vector names "ans2c" whose elements are all the example datasets that are supplied to you by default in R. The package with datasets in it is (aptly) called "datasets". Use search() to find which environment they're in, then use ls() to list them.
(ans2c <- ls("package:datasets"))

# (d) Create a new dataset called "hw2d" that is the first 3 columns of x5 and all the even rows (see ?seq for creating a vector of even numbers). 
(hw2d <- x5[seq(from=2,to=nrow(x5),by=2),1:3])

# (e) Now attach "hw2d" using attach(). By default, it will go into your second environment position (pos=2). Then type "maindata" into your R console. 
attach(hw2d)
maindata

# (f) Now use the matrix() and rnorm() functions to create a matrix "maindata" of random normal variables, 3 rows x 5 columns. Before seeing what "maindata" is, make sure you understand what will happen when you type "maindata" into your R console. Will it be a vector or a matrix?
(maindata <- matrix(rnorm(3*5),nrow=3,byrow=TRUE))


# (g) Perform a log (base 10) transformation of the 3rd column in hw2d ("time1"). This transformed variable should replace the 3rd column of hw2d. (Usually, I don't recommend replacing things - keep them all - but I ask you to do this for didactic reasons here) See ?log
(hw2d$time1 <- log(hw2d$time,base=10))

# notice the "NaN's" in hw2d$time1 - the log of a negative number is "Not A Number" (NaN)

# (h) Type in time1 to console. Create an object called "ans2h" that is a character that explains why time1, the variable, was not log transformed whereas hw2d$time1 was log transformed. Please include the positions of the search path in your answer.
ans2h <- "time1 is in pos=2 and this was not transformed. hw2d$time1 is in pos=1 and was transformed"



#8 CLEANUP ------------------------
# we have lots of objects that we don't need
ls()					
         # if we wanted to remove all objects, we'd do the following (please leave commented)
#remove(list=ls())			
ls() #There would be no more objects in pos=1. NOTE that we would have also removed the functions like info() that I created and you read in.
# If you accidentally ran the remove(list=ls()), no worries - just rerun the script to this point! Takes seconds. One reason I never create objects in terminal is so that I can recreate all objects I had just by rerunning the script. For this reason - reproducability - I recommend that you do NOT enter data in by hand... either type it into the script or have it on a file that you read in.




#9 READING DATAFILES IN ------------------------
# this reads in the objects in "country.RData" - in this case, a single data.frame
load("country.RData")
#save(x5,file="country.RData") #recall how we saved this file
# Note: "load" *only* reads in .RData files that have been saved by R. load() does NOT read in regular flat (e.g., tab delimitted) files
ls()

# read space delimited data into R
new.country <- read.table(file="country.data")
# write.table(x=x5,file="country.data")	 #recall how we wrote this
new.country #looks good

# the below reads in tab delimited data - this is b/c sep=" ", the default argument for sep, includes tabs as well
new.country.tab <- read.table(file="country.data.tabbed")    
#write.table(x=x5,file="country.data.norownames",col.names=TRUE,row.names=FALSE,quote=FALSE) #recall how we wrote this
new.country.tab
#The above doesn't look right. We did not tell R that the dataset had column names. See the default behavior for "header" in ?read.table. When we wrote out row.names above (the default behavior for write.table), it made a data file with the first row having 5 columns and all other rows having 6. So it defaulted to header=TRUE. When we did NOT write out row.names above, all rows have the same number of columns. Thus, to read this in, do this:
new.country.tab2 <- read.table(file="country.data.tabbed",header=T)    
new.country.tab2 #Better
#I usually write out files WITHOUT row names and use header=T when reading them in. This makes them more interoperable with other programs.

# reads in star delimited data 
new.country.star <- read.table(file="country.data.starred",header=T)
new.country.star #Again, not correct. Let's make the sep explicit
(new.country.star2 <- read.table(file="country.data.starred",header=T,sep="*"))

# scan is the "input" analogue to the "write" function - low on user-end 'ease of use' but useful sometimes for speed... 
#This won't work (we'd need to tell it to treat everything as a character)		
a <- scan("country.data.tabbed")	
#So do this instead
(a <- scan("country.data.tabbed",what="character"))
#But we should probably skip the header line:
(a <- scan("country.data.tabbed",what="character",skip=1))
#We could even wrap this into a matix if we wanted, but we'd need to know the number of rows to do so:
(a <- matrix(scan("country.data.tabbed",what="character",skip=1),nrow=nrow(new.country.tab2),byrow=T)) #Note all the function nesting in here!

# now we have our text entered into R - very useful sometimes!
(b <- scan("mytext",what="character"))	
# "b" is a character vector of length 28
str(b);mode(b)

# R has all the cool UNIX text manipulation abilities. We'll use this later for cool programming!
wookie.index <- grep("wookies",b)			
wookie.index
b[wookie.index]




#10 R PACKAGES ------------------------
# As I said during the first day of class, one of the great things about R is its expandability... let's explore that! From the R CRAN website, let's download and install the package MASS package. Please spend a few minutes exploring the "Packages" section of the CRAN website!
# The "MASS" package allows, among other things, the ability to create multivariate normal distributions with user-defined covariance matrices. Installing this can also be done through the RStudio GUI (Tools -> Install Packages)
install.packages("MASS",dependencies=TRUE)  #When it asks, say "n" to the question about installing from source.

# this command loads the "MASS" package
library("MASS")                             
# notice that MASS is now in the 2nd position. If it had any functions with the same name as functions that exist in other positions, we'd get a warning about masked objects
search()				     
# what functions are available from MASS?
ls(2)					     
# same thing 
ls("package:MASS")			     
# Note: if you always use a particular package, think about having it loaded automatically at startup by placing the above syntax in your RProfile.site or in your .RProfile files. See here: https://rstats.wtf/r-startup.html

#We can look at the function "mvrnorm" which creates multivariate random normal distributions
?mvrnorm



#11 MISSING DATA ------------------------
# We've already seen that "NA" is how R stores missing data. Let's explore some more functionality.
my.rows <- 25;my.columns <- 5
simulated.data <- matrix(rnorm(my.rows*my.columns),nrow=my.rows,ncol=my.columns)
# quick way to look at data - here just the first 6 rows
head(simulated.data) 
(simulated.missing <- matrix(rbinom(my.rows*my.columns,size=1,prob=.15),nrow=my.rows,ncol=my.columns))
# Note the "smart" (element-wise) way that R indexes when the index is a matrix of the same dimension as the main matrix! You haven't seen this before.
(tf.matrix <- simulated.missing==1)
simulated.data[tf.matrix] <- NA #This is the key step - understand what it's doing
simulated.data

# look at simulated missing
head(simulated.data)
mean(simulated.data[,1]) # R doesn't let you forget about missing values
mean(simulated.data[,1],na.rm=TRUE)  

# the function "na.omit" omits all data (and all rows) with NAs in it
na.omit(simulated.data) #omits rows with any missingness
na.omit(simulated.data[,1]) #omits elements with missingness

# this does NOT work - you can't create a logical in this way.
simulated.data[,1]==NA	      	
# NA's are not numbers and can't equal anything. This is how you create a logical vector if NA
is.na(simulated.data[,1])		
# this is how you create a logical vector if is NOT NA
!is.na(simulated.data[,1])		




#12 LOOPING ------------------------
# This is a very very short intro into an important topic we'll cover more as the course goes.
for (i in 1:5) {
  print(i)
}				# this is the basic syntax for building a loop

#we can loop through whatever index we want
myindex <- c(5,3,2203930,1)
for (i in myindex) {
  print(i)
}	

# Now think of all the ways you could use the "i" variable here. Oodles of ways! Here's just a few examples... more to come!:

# the below syntax says to loop through as many times as there are rows in simulated.data and for each iteration, find the mean of row i and put that in the ith element of my.means
my.means <- vector(length=nrow(simulated.data))
for (i in 1:nrow(simulated.data)) {    
  my.means[i] <- mean(simulated.data[i,],na.rm=TRUE)  
}	# try to understand this syntax before moving on
my.means

# we could also find the means of each variable using a for loop
my.col.means <- vector(length=ncol(simulated.data))
for (i in 1:ncol(simulated.data)) {    
  my.col.means[i] <- mean(simulated.data[,i],na.rm=TRUE)  
} # try to understand the above code before moving on

#Or we could ask how many NAs there are for each row (or column)
subj.nas <- vector(length=nrow(simulated.data))
for (i in 1:nrow(simulated.data)) {    
  subj.nas[i] <- sum(is.na(simulated.data[i,])) #summing a logical tells how many trues
}	# try to understand this syntax before moving on
subj.nas

# a faster alternative to looping in R is the apply family of functions. sapply() will do the looping internally but will only work properly on lists remember, a dataframe is a type of list
my.col.means2 <- sapply(as.data.frame(simulated.data), mean, na.rm=TRUE) # equivalent to above loop
rbind(my.col.means,my.col.means2) # they're the same :)




#HW Problem 3  ------------------------
# (a) MyData.csv (from the file on Canvas under Lecture 2) is a comma delimited file. Download it to your working directory and read it into your R session. Call it "My.CSV.data"
My.CSV.data <- read.csv("./jrl/MyData.csv")

# (b) elements that equal -99 are missing for the first 5 columns; those that equal 999 are missing for the last 4. Create a new object, DAT2, that is My.CSV.data but that is changed to reflect that these missing data are actually NAs
DAT2 <- My.CSV.data
DAT2[,1:5][DAT2[,1:5]==-99] <- NA
DAT2[,6:9][DAT2[,6:9]==999] <- NA
DAT2
# (c) the third column is actually an ordered factor with three levels. Change it in DAT2 accordingly.
DAT2[,3] <- as.factor(DAT2[,3])
str(DAT2)
# (d) find the trimmed mean for columns each column of MyData.csv (trim 5% from each tail; see ?mean). Do this using a loop, saving the mean results each time in a vector called "mymeans" of length 9. (Note: since the third column is an ordered factor, taking its mean is nonsensical so its mean will be "NA", which is as it should be (thanks R). Note also that you'll get a warning here, but not an error). Call you answer "ans3d"
ans3d <- vector(length=ncol(DAT2))
for (i in 1:ncol(DAT2)){
  ans3d[i] <- mean(DAT2[,i],trim=.05,na.rm=TRUE)
}

# (e) use sapply() instead of a for loop to answer question (d). Call your answer "ans3e"
ans3e <- sapply(DAT2,mean,trim=.05,na.rm=TRUE)







#THE END ---------------------------------



#@@Keller check hw1
str(hw1ca);str(hw1cb)   #num chr vs. factor factor
ans2c #Why we have diff modes above

str(hw1ca) #num chr

str(hw1e) #num chr

(ans1g <- sum(hw1ca$score,na.rm=TRUE)) #323

ans1h==ans1g #TRUE

hw1j #4.1428


#@@Keller check hw2
ans2c #datasets

hw2d #9x3

hw2d$time1 #NAs and -s

ans2h # time1 is in pos=2 hw2d$time1 is in pos=1 


#@@Keller check hw3
str(DAT2) #numx2, fac, intx3,num,intx2

ans3d #95.9922277 -0.1067078         NA 99.6097561 25.4390244 74.7750000  4.5756342 54.4736842  0.6097561

ans3e ==ans3d #all T but one NA




