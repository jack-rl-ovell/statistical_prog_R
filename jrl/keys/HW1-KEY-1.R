
###############Lecture/HW SCRIPT FOR R CLASS 1 - KEY



#1 R'S WORKING DIRECTORY  ---------------------------------
# Get working dirctory; asking R, "where is the default folder?"
getwd()					   
# This is where R expects to find files, and where it saves files, but this can be changed

#A word followed by parentheses tells R to look for a function named that word. Typically, we place "arguments" within the parentheses, but some functions, like getwd() do not require any arguments.
			
# R language is case sensitive
GETWD() 	#doesn't work

#You can set the default working directory using the function "setwd" like so on a PC:
setwd("C:/temp")	#doesn't work on MAC, and won't work on PC unless you already have a folder, "temp" in root	

#Or like so on a MAC:
setwd("/temp") #doesn't work on PC, and won't work on MAC unless you already have a folder, "temp" in root	

# The "temp" folder on your computer is the new default folder for saving and importing data for this session. This is where R looks by default to read and write files. Obviously, you can change the working directory whenever you'd like in a session. 

#You can also supply the full file path (e.g., "/Users/mmkeller/Documents/Teaching/R/my.R.script.R") to read or write files (we'll cover this later), and I suggest you typically be explicit about where you read/write files by spelling out the full paths of file locations within functions. 

#Nevertheless, knowing where your working directory is is useful, and because everyone will have different folders on their computers, it is not possible to be explicit about file locations in this class. Thus, we'll use working directory default a lot.

#Finally, to see the help file associated with a function, simply type "?" followed by the function name. E.g.:
?getwd

			       

#HW Problem 1 ----------------------------------------------------------
# (a) Change the working directory to a folder of your choice on your computer. Choose a folder that has some files in it
setwd("/Users/matthewkeller/Documents/RESEARCH/Cascade")
# (b) Look at the help page of the list.files function
?list.files
# (c) Using the function list.files(), make a list of the files in this folder
list.files()
# (d) Create a folder on your computer that will be your actual working directory for the rest of this session. This script expects this folder to be "C:/temp" for windows users or /temp for mac users but you can change this to any folder on your computer. Change your working directory to this folder.
setwd("/temp")



#2 R AS A CALCULATOR  ---------------------------------
# R can be used as a calculator
9+12*3			      
(9+12)*3
(sqrt(9)-6^2)/4

#Note that R follows typical order of operations from 8th grade math, so if it's often wise to use parentheses in your math to be explicity about the order you want.

(sqrt(9)-6)^2/4 #this answer is different because different parentheses

#The function ls() tells us what "objects" we've created in this session. Objects are things we've named and assigned data to

# Still nothing in our environment; we haven't created any objects yet
ls()					


#3 CREATING OBJECTS   ---------------------------------
# "<-" Assigns data ((sqrt(9)-6^2)/4) to new objects (x1)
x3 <- (sqrt(9)-6^2)/4		
x3
ls() #now there's something in our environment

#Note that you can also use "=" to do the same thing:
x3 = (sqrt(9)-6^2)/4		

#However, we will reserve "=" for specifying arguments within functions, and use "<-" for assignment. Another nice feature of the "<-" style of assigning objects is that you can specify direction:
(sqrt(9)-6^2)/4 -> z3

#Another useful trick sometimes is to wrap assignments within parentheses. This causes R to print out the value of the assigned object. Thur, rather than this:
x3 <- (sqrt(9)-6^2)/4		
x3
#We can simply to this:
(x3 <- (sqrt(9)-6^2)/4)	
#Obviously, we shouldn't do this when we're creating a large object
(x3.too.large <- rnorm(10000))

#Object x1 and z1 are identical:
x3
z3

#We can ask R to check that this is true using the "==" operator, which is asking R, "is the right object equal to the left object?"
x3 == z3



#HW Problem 2 ----------------------------------------------------------
# (a) Create an object called "hw2a" that is the square root of (19 dividied by 3)
(hw2a <- sqrt(19/3))
# (b) Create an object called hw2b that is the (square root of 19) divided by 3.
(hw2b <- sqrt(19)/3)
# (c) Use R to check whether these two quantities are the same.
hw2a==hw2b





#4 MODES, CLASSES, AND DIMENSION  ---------------------------------
#Every R object has a mode attribute and a class attribute. Mode is the type of thing (numbers, characters, logical, mixed, etc) stored in the object. Class is what the object is (a data set? a vector? a matrix? a list?) that tells R functions how to treat the object by default.

# "mode" function tells you what type of stuff is stored in object x1
mode(x3)

# Although the same for vectors, mode and class are different in general (we'll cover more on this later)
class(x3)				
#I really wish R would return "vector" here, but alas...

#There are many many more classes (thousands) than modes (maybe a dozen). Function often need the class of an object in order to know how to deal with the object. I know that the distinction between mode & class isn't obvious yet. It'll get more clear once we get into different types of objects below.
		              
# Vectors have no dimensions in R
dim(x3)				
# Only lengths
length(x3)

# Overrides old assignment - no warning given!
(x3 <- 4)				



#5 CREATING VECTORS OF NUMERIC MODE ---------------------------------
#We can assign a new object from an old object; both are still there
x5 <- x3
x5
x3

# We can try to overwrite x5 into a vector. R doesn't know what to do with commas outside of a function
x5 <- 1,2,3,4,5,6		
#Don't worry about doing things wrong in R - errors are your friends - read them! They're often helpful.

# Works for MATLAB, not R
x5 <- 1 2 3 4 5 6			
# x5 is still 4
x5					   

# c() is a function to combine; you'll use it ALL THE TIME!
c(1,2,3,4,5,6) #here we created a vector but did not assign it to anything.

#Let's override x5 and make it a vector of length 6 instead of length 1
x5 <- c(1,2,3,4,5,6)     
x5

# The ":" operator does the same
1:6					        
mode(x5)
class(x5)
dim(x5)				    
length(x5) # x5 is now a numeric vector of length 6
			    


#6 VECTORS OF CHARACTER MODE & INDEXING ---------------------------------
# Elements don't have to be numbers
x6 <- c("red","blue","green")	
x6

#However, all elements within a vector have to be of the same mode. Let's reassign x6 and add the number 3 at the end of it. What happens to the 3? 
x6 <- c(x6,3)
x6
#The 3 is no longer a number. R automatically "coerced" the mode to be the same for each element. By default, for a mix of characters and numerics are converted to character mode.

#We can "index" a vector using the [] operator. If we want the 2nd element of x6:
x6[2]

#If we want the second and fourth element:
x6[c(2,4)]
#Note that we used the c() function INSIDE the [] operator to index the second and fourth element. 

#Or we can index a range:
x6[2:4]

#We can also rearrange elements like so:
x6[c(3,4,2,1)]

#Or repeat and rearrange, etc:
x6[c(3,4,2,2,2,2,4,1,3,2)]

#Again, note that "3" is a character, not a number, and R doesn't know how to do math operations on characters:
x6[4]*3 #returns an error
x5[3]*3     #works fine



#7 VECTORS OF LOGICAL MODE & LOGICAL OPERATORS ---------------------------------
# The values "T","F","TRUE", "FALSE", and "NA" are reserved in R and have
# special meanings
x7 <- c(T,T,F,T,T,F)		
x7
#TRUE is the same as T, and FALSE the same as F. These are "logical" elements.

#Interestingly, R is able to do math operations on logicals. It simply converts TRUE's to 1's, and FALSE's to 0's. E.g.:
x7*1
x7*3
x7+1
#This trick will come in handy later.

#R also uses the "NA" element to mean "missing". We can reassign the third element of x7 to be missing:
x7[3] <- NA
x7
x7*3 #Any math operation on NA returns NA

# Logicals are used very often in R as a way of subsetting data. Logical operators are ==, <, >, <=, >=, &, |
(x7b <- c(3,4,5,6,5,4))
x5

#Less than
x7b<x5 #with a vector (note this returns a logical vector of length 6)
x7b < 4 #with a scalar (returns a logical vector of length 6)
4 < x7b #with a scalar (returns a logical vector of length 6)

#Less than or equal to?
x7b <= x5 #with a vector 
x7b <= 4
x7b > 4 #the compliment of above

#We can also use the ! operator to get the compliment 
x7b <= 4
!(x7b <= 4)

# == function means "are they equal?" do ALL the elements of x7b = x5?
x7b==x5 #vector
x7b==4 #scalar

# != function means "are they unequal?" do ANY of the elements x7b not equal x5?
x7b!=x5				   
x7b<= x5

#The | means "or". The & means "and"
x5<3 | x5>6
x5>2 & x5<50
(x5 >5 | x5 < 2) & x5 < 3 #can make it more complicated
x5 >5 | (x5 < 2 & x5 < 3) #parentheses matter
(x5 >5 | x5 < 2) | x5 == 3 #make sure you understand how these all work. Play around with these logical operators, and ask questions!!


#We can assign these logicals - they're just vectors after all
(tf7 <- x7b==x5)
mode(tf7) # tf7 is a vector of mode and class "logical"
class(tf7)

# Arithmetic on a "logical" vector changes the TRUE elements=1 and FALSE=0
(tf7 <- tf7*1)	     
# tf7 is now a "numeric" rather than "logical" vector
mode(tf7)	
class(tf7)



#8 USING LOGICALS TO SUBSET & RECODE DATA ---------------------------------
#We can index using logicals
x5[c(T,T,F,F,T,T)]
#The above created the logical vector on the fly. Alternatively:
tf7b <- c(T,T,F,F,T,T)
x5[tf7b]

#Recoding with logicals
new.vector  <- c(33,85,28,12,6,97,25,55,45,78)
#Say we want to make numbers less than 30 be 30, and those more than 60 be 60. And then everything else stay the same. We want to call this "new.vector2". How to do this? Here is a laborious way to do this using logicals:
new.vector2 <- new.vector #create a new object
new.vector2[new.vector < 30] <- 30
new.vector2[new.vector >60] <- 60

# There is another way to recode with logicals that is often more efficient; use this trick for HW problem below
(x7b==x5)*5  #everything is zero except where x7b==x5, then it's 5
(x7b != x5)*8 #everything zero except where x7b != x5, then it's 8
(x7c <- (x7b==x5)*5 + (x7b != x5)*8) #by adding these, we've recoded x7b to 5's and 8's

# Let's use this trick to recode new.vector3 like above: 
new.vector3 <- (new.vector < 30)*30 + (new.vector > 60)*60 + (new.vector>=30 & new.vector<=60)*new.vector 
#Break it up bit by bit to see what's happening
(new.vector < 30)
(new.vector < 30)*30 
(new.vector > 60)
(new.vector > 60)*60 
(new.vector>=30 & new.vector<=60)
(new.vector>=30 & new.vector<=60)*new.vector

#Check:
new.vector3==new.vector2
sum(new.vector3==new.vector2)==length(new.vector3) #are they all TRUE?



#9 VECTOR MANIPULATION & MORE ON INDEXING ---------------------------------
# R performs element by element arithmetic (matrix operators too; see below)
x5*x7b				

# If the dimensions are nonequal, R "recycles". Here, we multiply a vector of length 6 by a vector of length 1 (what we'd call a scalar, but R thinks of scalars as vectors of length 1)
x5*c(2,2,2,2,2,2)
x5*2 #Same as above due to recycling

# R recycles happily, so long as the longer is divisible by the shorter; BE
# CAREFUL!
x5*c(2,3)	

# However, if one vector's length isn't divisible by the other's, R still gives you an answer, but also a warning. Warnings are different than errors. Error means that R didn't perform the operation and tries to tell you why. Warnings mean that R DID perform an operation, but there is reason to believe it might not have been what you had intended. Both errors and warnings are YOUR FRIENDS - read them and try to understand what they're saying.
x5*c(2,3,4,5)	

# [4] returns the fourth element in x5
x5[4]					
# The [4] indexes the fourth element in vector x5, and changes it to 99
x5[4] <- 99
x5[6] <- -4
#NOTE that we've changed x5 by assignment:
x5

#We could have done this all at once:
x5 <- c(1,2,3,4,5,6)
x5[c(4,6)] <- c(99,-4)

# ";" operator allows you to put multiple commands on the same line. I don't use it much but it is sometimes helpful
x5;c(1,2,3,4,5,6)	

#Thus, there are TWO ways we've covered to index vectors:
x5[c(1,3,6)] #with a numeric vector that does not have to be the length of x5
x5[c(T,F,T,F,F,T)] #with a logical that should be

x5[c(T,F)] #Note that the logical doesn't HAVE to be the same length, but in this case, it is recycling. Thus, the above is just like:
x5[c(T,F,T,F,T,F)]




#10 LS() AND REMOVING OBJECTS ---------------------------------
ls()
x4 <- c(5,6,7)
ls()

# Remove object x4
remove(x3)				
ls()




#HW Problem 3 ---------------------------------
hw.vector  <- c(33,85,28,12,6,97,25,55,45,78)
# (a) There are two ways to index vectors that we've covered. First, using a vector of numbers. Second, using a logical vector. Change the 4th and 6th element of hw.vector to be -60 and -90 respectively. Do this first using a numeric vector and call this new vector hw3an. Do it again using a logical vector and call this vector hw3av.
hw3an <- hw3av <- hw.vector
hw3an[c(4,6)] <- c(-60,-90)
hw3av[c(F,F,F,T,F,T,rep(F,4))] <- c(-60,-90)
# (b) In a single line of code using a logical operator & multiplication, make another vector "hw3b" that is 3 if hw.vector element <= 50 and 0 otherwise
hw3b <- (hw.vector<=50)*3
# (c) In a single line of code using logical operators & vector multiplication, make vector "hw3c" whose elements = 5 if hw.vector element is between 30 and 80 and -99 otherwise
hw3c <- (hw.vector>=30 & hw.vector<=80)*5 + (hw.vector<30 | hw.vector>80)*-99
#another way to do it; longer but useful to understand
index1 <- (hw.vector>=30 & hw.vector<=80)  
index2 <- index1==FALSE
a3 <- hw.vector
a3[index1] <- 5
a3[index2] <- -99
# check your answer visually with "rbind" (row bind) function
answer <- rbind(hw.vector,hw3a,hw3b,hw3c) 
# (d) In a single line of code, create a new vector, hw3d, that only contains the elements of hw.vector that are between 30 and 70
hw3d <- hw.vector[hw.vector >30 & hw.vector <70]




#11 FUNCTIONS, USING THE REP  & SEQ FUNCTIONS AS EXAMPLES ---------------------------------
x11a <- c(2,2,2,2,2,2)
# The "rep" (repeat) function; you'll use this a lot too
x11b <- rep(2,6)			
x11b

#Is x11a the same as x11b?
x11a==x11b
#This returns a logical vector of the same length as x11a. Check they're all TRUE:
sum(x11a==x11b)==length(x11a)

#If we change the first element of x11a to be 99, our logical vector changes:
x11a[1] <- 99
x11a==x11b

# What is the format for arguments in rep? "?", the help function, tells us
?rep

rep(x=2,times=6)
# Same behavior as above! When we typed "rep(2,6)", this was shorthand for rep(x=2,times=6). Thus, unless you explicitly name arguments, R functions will assume that the parameters you supply are in the order of the arguments in the function. In general, it is usually a good idea to explicitly specify the arguments you use.


#Read the ?rep help page carefully and see if you can understand the behavior of the following uses of the rep() function
rep(1:3,4)
rep(1:3,times=4)
rep(1:3, each=4)
rep(4:8,times=c(1,2,4,0,1)) #reading the "times" entry under rep tells us that times can be a vector!
rep(1:3,length.out=10)

#We can also create sequences of numbers using seq
?seq #read the help page first!!
seq(from=100,to=1000,by=100)
seq(100,1000,100) #same behavior as above - do you understand why?
seq(from=100,to=1000,length.out=5)
seq(100,1000,5) #NOT the same behavior as above - do you understand why?



#12 ANOTHER TYPE OF OBJECT: THE MATRIX ---------------------------------
?matrix
(x12 <- seq(10,60,by=10))		
#the "matrix" function creates matrices
matrix(x12, nrow=2, ncol=3,byrow=TRUE) 
mat12a <- matrix(x12, nrow=2, ncol=3,byrow=TRUE)
mat12a
mat12b <- matrix(x12, nrow=2, ncol=3,byrow=FALSE)	
mat12b
# Default is for byrow=FALSE, so we don't have to write it
matrix(x12, nrow=2, ncol=3)	
mat12a;mat12b

# Element by element multiplication
mat12a*mat12b					
mat12a-mat12b

# %*% function is for matrix multlication; can't multiply 2*3 by a 2*3
mat12a %*% mat12b				
# "t" is transpose function
t(mat12b)					
(mat12c <- mat12a %*% t(mat12b))



#13 INDEXING MATRICES ---------------------------------
mat12a
# Index specific values with []; [1,3] is 1st row, 3rd column
mat12a[1,3]					
# The "[1,]" means row 1, all columns
mat12a[1,]					
# All true
mat12a[1,]==mat12a[1,1:3]	
# 2nd column
mat12a[,2]					
# Rearrange columns and rows
col.index <- c(3,1,2)
row.index <- c(2,1)
(mat13 <- mat12a[row.index,col.index])

#We can also repeat columns or rows
mat12a[c(2,1),c(3,1,2,2,2,2)]	
mat13

# Only columns 1 & 3
mat12a[,c(1,3)]

# We can also index by a logical vector of the same length as the data row or column
mat12a[,c(TRUE,FALSE,TRUE)]       	
my.awesome.index <- c(TRUE,FALSE,TRUE)
mat12a[,my.awesome.index]         	

# Same thing! Indexing my logical vectors is IMPORTANT to understand & use because you'll eventually want to come up with conditional statements and use them to organize, subset, or slice data, like so:
colSums(mat12a) #colSums() is a function that takes the sum of columns
logical.index <- colSums(mat12a) > 60
mat12a[,logical.index]



#14 SIMULATING DATA ---------------------------------
a <- 50
# Same thing; a is now an numerical object that equals 50
1:50;1:a					   		 	
# use rnorm() function to make x14, a vector of 50 random normal variables
x14 <- rnorm(a)				    	 	
# cbind = "column bind" - binds columns together
?cbind                             
# rnorm(a) and x14 are not the same; R re-randomizes each time 
cbind(rnorm(a),x14)					 	
# The 40th entry of x14
x14[a-10]		

# y14 is linear combination of 40% x14 & 60% random noise (on average). Ask me to go over why once you get to this point of the script
y14 <- x14*sqrt(.4) + rnorm(a)*sqrt(.6) 
# column bind x14 & y14 so that we can look at them side by side
(z <- cbind(x14,y14))
#We can empirically check the r2 between x14 and y14. On average it will equal .40, but there will be sampling variance between different people
cor(x14,y14)^2

# The "sample" function; useful for permutation testing (replace=FALSE) 
sample(x14, size=50, replace=FALSE)	
# or for bootstrapping (replace=TRUE)
sample(x14, size=10, replace=TRUE)	
#We'll cover these techniques later in the course.



#15 DATA FRAMES ---------------------------------
#Data.frames are the first type of object we've covered that can be a MIX of different modes, one mode per column. Data.frames are used a lot for storing datasets. Let's create one via simulation:
?rbinom #creates random binomially distributed data
dis <- rbinom(n=50,size=1,prob=.15)
intv <- sample(c("Lisa","John","Alice"),50,replace=TRUE)
dataset15 <- data.frame(col1=x14,col2=y14,disease=dis,interviewer=intv)
# Rename the variables in dataset
names(dataset15) <- c("var1","IQ","disorder","interviewer") 

#Look at the dataset
dataset15

#What is it's mode and class?
mode(dataset15) #A list is a "mixed" mode that can store multiple different modes of data. 
class(dataset15) #A data.frame is a special type of list that has one mode per column

# The default behavior of functions depend on the class of the object. Here are the default behaviors for summary() and plot() for class "data.frame"
summary(dataset15)
plot(dataset15) 				




#16 INDEXING DATA FRAMES ---------------------------------
# Data.frames can be indexed like matrices
dataset15[,4]
dataset15[3,]
dataset15[1:10,c(1,4)]

# Or using the "$" notation
dataset15$IQ			
x16 <- dataset15$IQ
mode(x16)
class(x16) #x16 is now just a numeric vector we pulled out of dataset15

#A quick way of looking at the top of a data.frame or matrix is to use the function head()
head(dataset15)
#you can also specify how many rows to look at; see ?head
head(dataset15,n=15)
?head #Note that the default n=6L. The "L" following a number in R is just to specify that the number must be integer. 
6L==6
6.5L



#17 CHANGING AN OBJECT'S CLASS ---------------------------------
# the as.xxx() series of functions are used to change the classes of objects
# Change the class of mat12b from matrix to data.frame
mode(mat12b)
class(mat12b) #a matrix of mode numeric
(mat17 <- as.data.frame(mat12b))
mode(mat17)
class(mat17) #a data.frame of mode "list"
# It is quite common to change between class matrix and class data.frame. Data.frames are useful for storing mixed data. Matrices are useful if you want to do matrix 

#Note that all data.frames have column names whereas matrices may or may not. If you don't supply colnames to the data.frame, R does it for you automatically:
colnames(mat12b)
colnames(mat17)

#We can use the same colnames() function to assign names, and do it for both in a single line using double assignment:
colnames(mat12b) <- colnames(mat17) <- c('n1','n2','n3')
mat12b
mat17

#Using the $ operator works on data.frames only, not matrices:
mat12b$n1
mat17$n1

# you might want to change data.frames to matrices in order to do (e.g.) matrix manipulation (or to arrays to store 3+ dimensions - we'll get to arrays later)
class(mat17)
mat17b <- as.matrix(mat17)			
mat17b
class(mat17b)

# Warning: be careful changing data.frames to matrices when you have data of mixed modes! R will force everything in the matrix to have the same mode (the "lowest" on possible, so character)
dataset.mat17 <- as.matrix(dataset15)
dataset.mat17 #everything is a character now
dataset.mat17$IQ *2 #won't work because these are now characters, not numbers
                               




#HW Problem 4 ---------------------------------
# (a) Create a 2500 x 3 data.frame that is named "hw4a"
# The first columns is normally dist. data, mean=100, sd=15; see ?rnorm
# The second column is normally dist but 10% of them are outliers (add 30 to a random 10% of the scores from the first column); see ?rbinom to help in choosing the random 10% of scores
# The third column is binary, with overall prob(x=1) = .25
# Give each column an informative name
normal.dat <- rnorm(2500,mean=100,sd=15)
adder <- rbinom(n=2500,size=1,prob=.1)*30
outlier.dat <- normal.dat+adder
binary.dat <- rbinom(n=2500,size=1,prob=.25)
hw4a <- cbind.data.frame(normal.dat,outlier.dat,binary.dat)
# (b) Plot a histogram of the first two variables. The do a scatterplot of the first two variables against each other. See ?hist and?plot
hist(hw4a[,1])
hist(hw4a[,2])
plot(hw4a[,1:2])
# (c) What is the mean, median, & var of the first two columns? What % of the third column is equal to 1? See ?mean ?median ?var ?summary
summary(hw4a)
var(hw4a[,1]);var(hw4a[,2])
# (d) What is the mean, median, & variance of the first two columns WHEN the third column is equal to 1? (hint: first try creating a vector that is TRUE when the third column is 1 and FALSE when 0, then use this vector as an index). Place the mean, median, & var for each of these column into two 3 element vectors named hw4d1 and hw4d2 respectively
(hw4d1 <- c(mean(hw4a[hw4a[,3]==1,1]),median(hw4a[hw4a[,3]==1,1]),var(hw4a[hw4a[,3]==1,1])))
(hw4d2 <- c(mean(hw4a[hw4a[,3]==1,2]),median(hw4a[hw4a[,3]==1,2]),var(hw4a[hw4a[,3]==1,2])))
# (e) Take a random sample of 50 rows of hw4a (without replacement) using the "sample" function. What is the mean & standard deviation (see ?sd) of the second column for this subset of rows from hw4a? Place this information into a 2 element vector named hw4e
dat2 <- hw4a[sample(1:nrow(hw4a),size=50,replace=FALSE),]
(hw4e <- c(mean(dat2[,2]),sd(dat2[,2])))

# (f) We'd like to do a monte carlo experiment where we randomly sample 50 rows (without replacement) like above, but do this 5 times. Each time, find the mean and sd of the first column of hw4a and place this information in a row of a matrix of 5 rows by 2 columns named "hw4f". In the end, the first column of hw4f should contain 5 resampled means and the second column 5 resampled sd's. This is a (small) sampling distribution of means and sd's from the original hw4a population data. Eventually, we'll do this much much more effeciently using a for loop, but for now...
hw4f <- matrix(NA,nrow=5,ncol=2)
dat2 <- hw4a[sample(1:nrow(hw4a),size=50,replace=FALSE),]
hw4f[1,] <- c(mean(dat2[,1]),sd(dat2[,1]))
dat2 <- hw4a[sample(1:nrow(hw4a),size=50,replace=FALSE),]
hw4f[2,] <- c(mean(dat2[,1]),sd(dat2[,1]))
dat2 <- hw4a[sample(1:nrow(hw4a),size=50,replace=FALSE),]
hw4f[3,] <- c(mean(dat2[,1]),sd(dat2[,1]))
dat2 <- hw4a[sample(1:nrow(hw4a),size=50,replace=FALSE),]
hw4f[4,] <- c(mean(dat2[,1]),sd(dat2[,1]))
dat2 <- hw4a[sample(1:nrow(hw4a),size=50,replace=FALSE),]
hw4f[5,] <- c(mean(dat2[,1]),sd(dat2[,1]))

# (g) Provide hw4f with useful column names
colnames(hw4f) <- c('means','sds')



#18 SAVING AND QUITTING ---------------------------------
# there are three basic ways to save objects from an R session: 
# (1) write out the object into a format (e.g., tab delimitted) that can be read by other programs
# (2) save all the objects you want to save in an .RData file or 
# (3) save the syntax file so it can be rerun, thereby recreating all objects(my favorite method unless the creation of objects took a long time)
# We'll go through these in order

#18.1 (writing objects out) Save monte carlo hw as an object of class "data.frame"
write.table(hw4f,file="my.montecarlo.hw.txt",quote=FALSE,col.names=TRUE,row.names=FALSE)
?write.table #Look at the options
#I almost ALWAYS specify the following options:
#quote=  if TRUE, R will put quotes around characters. I usually set this FALSE
#col.names=  if TRUE, R writes out the column names
#row.names=  if TRUE, R writes out the row names

#Where exactly is that file our wrote above? It's in your working directory of course! 
getwd() #this is the default location for writing/reading files
#if you want to specify a different location, you'd do it like this
write.table(hw4f,file="/Users/mmkeller/myfolder/mysubfolder/my.montecarlo.hw.txt",quote=FALSE,col.names=TRUE,row.names=FALSE)

# 18.2 (save as a .RData file). Say that we don't know how to save. help.search searches through titles & keywords to match "save"
help.search("save")                    
?save

# saves these three objects in an .RData file
save(tf7,x12,x11a,file="threeobjects.RData")  
#.RData files are compressed files that contain all your R objects. They can be read back into R using the load() command. E.g., load("threeobjects.RData")

# saves ALL objects you've created into an .RData file
save.image("all.objects.RData")

#18.3 - obviously, the most crucial thing to do regardless of whether you've written out files or not is to SAVE YOUR SCRIPT! DO SO NOW! This way you can easily go back to your script and recreate all the objects you've made just by selecting all and running the script

#Make sure to turn in your finished script to Canvas. I should be able to run the entire script and get the objects you've created in your HWs

# Note, to remove all objects you've created from your environment, you'd use the function remove(list=ls()). 								       

# If you want to quit R and not save any objects, you'd do this: q('no')  





#THE END ---------------------------------




#@@Keller check hw2
hw2a;hw2b
#@@Keller check hw3
rbind(hw.vector,hw3an,hw3av,hw3b,hw3c);hw3d
#@@Keller check hw4
head(hw4a)
apply(hw4a,2,sd)
hw4f



ans2c 

