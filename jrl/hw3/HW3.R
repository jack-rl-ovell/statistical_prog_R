
# LECTURE/HW #3 - GRAPHING AND STATISTICS FOR THE LINEAR MODEL


#1 R'S WORKING DIRECTORY ---------------------
getwd()				# Get working dirctory; asking R, "where is the default folder?"
setwd("C:/temp")		# "C:/temp" is new default folder for saving and importing data for this session
setwd("/temp")			# for Mac users
getwd()
#PLEASE go ahead and use setwd() to set your default directory

#Read in a script I wrote that contains handy functions like info(x), LS(), and look(x), The function source() simply reads in another R script and runs every line of code
source("http://www.matthewckeller.com/R.Class/KellerScript2.R") 




#2 R version & default options"  ---------------------
#To check what version of R you're using:
R.version #you should be on version 4+. If not, please update R through the CRAN website

#To check what default options are in effect:
(default.ops <- options()) #default.ops is now an object (a list)
#To see what these options mean:
?options
#We can change options. Say we want to change the number of decimals. Let's look at pi before and after changing this:
default.ops$digits
pi
options(digits=5)
pi

#One default option that is commonly fiddled with is whether R by default should convert characters to factors when reading in data (read.table()) or when creating data.frames(). The default behavior (which should be FALSE and was just changed about a year ago) can be accessed like so:
options()$stringsAsFactors
#IMO, this default is SO MUCH BETTER than it used to be - the less R does automatically/behind the scenes, the better. To see what this does, let's create a data.frame first with the default stringsAsFactors=FALSE option, then change it:
x2.char <- data.frame(a=1:8,b=c('a','b','b','b','c','d','e','e'))
str(x2.char)
#I personally like this better, and it's easy to change from char to factor on the fly:
summary(as.factor(x2.char$b))
#Now change the default
options(stringsAsFactors=TRUE) 
x2.fac <- data.frame(a=1:8,b=c('a','b','b','b','c','d','e','e'))
str(x2.fac) 
summary(x2.fac)
#Notice the interesting warning about this option being deprecated in the future. I'm not privy to the reasons for this, but my guess is that it was never a very good idea to have something like this be changeable because it makes code sharing problematic (depending on the default each user sets). Anyway, in the near future, we won't even have default options like this and you'll just have to deal with character vectors (and change them to factors) as needed - which is how it always should have been!

#Let's reset the default factor behavior back to how it should be:
options(stringsAsFactors=FALSE) 

#So now we've reset one option:
options()$digits #changed
options()$stringsAsFactors #default - should be FALSE

#We can also, e.g., change the prompt:
options(prompt="$$~~ ")

#If we want to revert back to our default options, we can simply put the object we created above into the function:
options(default.ops)
options()$digits
options()$stringsAsFactor
options()$prompt




#3 INSTALLING PACKAGES "TOOLS" & "CAR"  ---------------------
# Let's look at the packages that have been loaded by default:
search()

# The tools package has some useful functions in it. It should already be installed in R, so all we need to do is load it
require(tools) #this loads the package; the library() function does the same
search() #now we see tools is loaded in pos=2. We can list the functions in there:
ls(2)

# CAR stands for Companion to Applied Regression, a book (and corresponding package) by John Fox. We'll use this package for its good datasets and also for some nice stats functions often used by psychologists. The following example is based partly on Fox's book of the same name.
#To load most packages plus their dependencies, we can typically just use the following:
install.packages("car",dependencies=TRUE,type="binary")   # installs "car" package as well as dependencies (packages upon which car relies) and dependencies of dependencies, etc. from binaries rather than from source. A few notes:
#1) You only need to install a package ONCE on a given machine. 
#2) You might get a warning there that some binaries have more recent source packages. When developers first make their package, they create a source package and then for user convenience convert to binaries. Usually it's fine to ignore this and just not have the most up-to-date packages on your system
#3) You can check your default options relevant to installing packages:
options()$repos #the repository 
options()$verbose #if you want output on status of installing packages
options()$pkgType #"binary", "source", or "both

#We can see what the package dependencies are like this:
package_dependencies("car")

# shows all *loaded* packages - "car" isn't there yet b/c we haven't loaded it in this session, just installed it
search() 	
require(car)	# now we've loaded it
search()        # note that most of car's dependencies (e.g., lme4) are NOT yet loaded - they will load when needed (called "lazy loading")

LS(2)	 #shows what objects are in "car"
LS(3)  #shows the car dataset examples 
info(Duncan)	# occupational prestige dataset originally analyzed by Duncan (1961)
Duncan    	
?Duncan #to see what the variables mean
str(Duncan)	# function "structure"

          				


#4 BASIC PLOT OF THE DATA  ---------------------
# Note: Researchers are increasingly realizing that visual inspection of the data should be the first step in analyses. R's excellent graphing ability, and its interactive nature, makes such data exploration simple. For the next two lectures, we'll mainly rely on R's pre-built graphics functions; later in the class we'll get to customizing publication-quality graphs
?plot		# plot says its first 2 arguments are x & y
# as a first pass, we can look at "Duncan" - from reading ?plot, can you figure out why plot works on a single object (no x & y)?
plot(Duncan)			
methods(plot)	 # this lists all the methods associated with "plot" function. What methods(plot) is telling you is all the different functions that "plot" actually is, depending on the class you feed to it

class(Duncan)    # remember from the 1st lecture: the CLASS of an object determines how a function ("plot" in this case) treats it
?plot.data.frame # *this* then is actually the function being called by plot when you feed it a data.frame and only specify the x argument

Duncan$type; as.numeric(Duncan$type) 	# shows us the coding of prof, wc, and bc
search()
prestige				# returns an error
attach(Duncan)	# you can attach data.frames (and lists) to the search path as well. In GENERAL, I recommend against doing this, but it does save us from having to do lots of typing "Duncan$" throughout the rest of this script, so just this once... This is an example of "do as you're told, not as I do" :O
search()
prestige				# doesn't return an error. Make sure you understand why!




#5 HISTOGRAMS OF THE DATA	---------------------
hist(prestige)
hist(type)	# "hist" function only wants objects of mode numeric; such errors HELP the user, making sure we know what we're doing. If you're unclear, see what the mode of type is!
hist(as.numeric(type))	# notice how it's easy to change the mode of an object ("type" here) on the fly. But please remember that to change from something that looks numeric but that's actually a factor in R, you have to do it differently:
(fac <- as.factor(c(3,3,3,2,2,'a','a','b',1,1,'c',4,4)))
#THIS is how to change a factor to a numeric (if the factor labels have a numeric meaning)
(num.fac <- as.numeric(as.character(fac))) 
mode(num.fac)
summary(num.fac)
#this is how NOT to do it:
(num.fac.bad <- as.numeric((fac)))#this is how to change it
mode(num.fac.bad)
summary(num.fac.bad) #mean is 3.54 instead of 2.56 - 2.56 is probably the right way to think about these values


# "par" is a lot like the function "options" except it changes the defaults for graphics only. It is a A VERY USEFUL FUNCTION to have in your bag of goodies (or book of spells for you Harry Potter fans)
op <- par(mfrow=c(2,2))	 # mfrow is a handy option - it says to split the screen up into (here) 2 rows and 2 columns. When you assign "par" to something, it returns the *original* values of "par" - making it easy to change the graphing options back to its defaults, just like we did with options() above
op			 # see - "op" is just the value that "mfrow" was *before* we changed it
hist(as.numeric(type))
hist(income,col='blue')
hist(education,col='red')
hist(prestige,col='green',main="This be my HISTO")
par()$mfrow
par(op)			# now we change the mfrow back to being a 1x1 (a single graph)
par()$mfrow




#6 BASIC SCATTERPLOTS OF THE DATA		---------------------
plot(Duncan)		# we've already seen the default "plot" behavior for class(x)=data.frame; now let's look in more detail
plot(prestige~income)	# this is the default behavior for when "plot" gets a formula (y~x is formula notation in R)
formula.p.i <- prestige~income
info(formula.p.i)
plot(formula.p.i)	# same thing; so we see a very strong relationship between income and prestige
abline(lm(formula.p.i))	# when "abline" or "a to b line" gets an object of class "lm" (linear model), it returns a best fit line. Note that the best fit line was ADDED to the existing plot - many plotting functions in R add to an existing plot rather than create a new one. VERY useful for building cool plots. And again, it's important to be aware of the class of the object being passed to R functions
plot(formula.p.i,main="Prestige regressed on income",pch=19,col='blue')
abline(lm(formula.p.i),col='red',lty=2,lwd=3)    # changing the defaults to "abline"





#7 ADVANCED SCATTERPLOTS OF THE DATA	---------------------
# Here is the regular old default plot using the pairs() function
pairs(Duncan)

# Let's see how we can modify the above to make a cooler graph. The rationale of the following syntax is not at all easy to see - don't worry too much right now about understanding this. It requires understanding the syntax for the "function" class - something we'll get to later in the course, but if you want to try, start with ?pairs and look at "panel" option
pairs(Duncan,
  panel = function(x,y){ 			   # panel option requires input of class "function"
        points(x,y)
        abline(lm(y~x),col="red",lwd=2,lty=2)        # the red lines are linear best fits
        lines(lowess(x,y),col="darkgreen",lwd=2)},   # the green lines are loess (smoothed) curves - useful for checking linearity 
)

#And we can add new plots to the diagonals too using "diag.panel" option
pairs(Duncan,
      panel = function(x,y){ 			   # panel option requires input of class "function"
        points(x,y)
        abline(lm(y~x),col="red",lwd=2,lty=2)        # the red lines are linear best fits
        lines(lowess(x,y),col="darkgreen",lwd=2)},   # the green lines are loess (smoothed) curves - useful for checking linearity 
      diag.panel = function(x){			   # diag.panel option also requires input of class "function"
        par(new=TRUE)			 # par(new=TRUE) disallows hist to clear the old graphs before drawing a new histogram
        hist(x,main="",axes=FALSE)}	 # creates histograms along the diagonal
)

#Rather than type all this in by hand each time, we can create a function on the fly:
MyPairs <- function(DAT){pairs(DAT,
                              panel = function(x,y){ 			  
                              points(x,y)
                              abline(lm(y~x),col="red",lwd=2,lty=2)        
                              lines(lowess(x,y),col="darkgreen",lwd=2)}, 
                              diag.panel = function(x){			   
                              par(new=TRUE)			
                              hist(x,main="",axes=FALSE)}	 )}
#Check that it works
MyPairs(Duncan)



#HW Problem 1 	---------------------
# (a) Produce a histogram of the prestige variable that has 5 bins rather than 10. Title this histogram "HW1a histogram"
hist(Duncan$prestige, breaks = 9, main = "HW1a Histogram")

# (b) use the plot(), abline(), and lines() functions to create a single scatterplot (just one, not the matrix of them) exactly like the one in row 2 column 1 from the pairs() graph in #7 immediately above. Title this "HW1b scatterplot". Note that you'll need to change the mode of the variable "type" to be numeric on the fly. Also note that pairs() (which only create matrices of scatterplots) and points() (which only adds points to existing plots) won't accomplish what you want. see ?plot to figure out how to make a title on the figure.
attach(Duncan)
type_numeric <- as.numeric(Duncan$type)
formula.t.i <- income~type_numeric
plot(type_numeric, income, main = "HW1b scatterplot")
abline(lm(formula.t.i),col="red",lwd=2,lty=2)
lines(lowess(formula.t.i),col="darkgreen",lwd=2)

# (c) What is your verbal interpretation for the loess (green) curve for income~type in the plot you did in HW1b (or in row 2 column 1 from the pairs() graph in #7 immediately above if you couldn't do HW1b)? Assign your answer (the character string) to an object "hw1c". You can read information on this dataset using ?Duncan
hw1c <- "After smoothing our line of best fit it is clear that each distinct category has seperate levels of income. Although, it should be noted that the way the line is behvaing might not be because of some quadrtatic pattern in the data. Rather it should be noted that R automatically coded our factors in an alphabetical way, giving our graph this shape.

# (d) How do you interpret the linear line (red) for type~income? Assign your answer to hw1d."
hw1d <- "The linear positive trend suggests that as the factors of employment increase, income might as well. But again the reason these are increasing is because of their alphabetical trend."

# (e) Run the following code:
my.x <- rnorm(1e4)			      # 1e4 = 10,000 random normal numbers
my.y <- sqrt(.3)*my.x + sqrt(.7)*rnorm(1e4)   # my.y is 60% x (r2 = .6) and 40% random noise
Huge.data <- data.frame(y=my.y,x=my.x)
# It is difficult to 'look' at large amounts of data. Run the following:
Huge.data 		# not helpful!!
plot(Huge.data)
# As you can tell, with large datasets it is difficult to see what is going on. Using the function "sample", figure out a way to plot a random subset (n=300) of the data so that you can see the data pattern better. Title this plot "HW1e scatterplot subset"
plot(Huge.data[sample(nrow(Huge.data), size = 300),])

# (f) Now take four random samples of the data, each of size=300. Plot each in one of the panes of a 2x2 plot. Title each one "HW1f plot X" where X is 1, 2, 3, or 4 (in order). Remember the par(mfrow=) option to change R's default plotting behavior! 
par(mfrow=c(2,2))
plot(Huge.data[sample(nrow(Huge.data), size = 300),], main = "HW1f plot 1")
plot(Huge.data[sample(nrow(Huge.data), size = 300),], main = "HW1f plot 2")
plot(Huge.data[sample(nrow(Huge.data), size = 300),], main = "HW1f plot 3")
plot(Huge.data[sample(nrow(Huge.data), size = 300),], main = "HW1f plot 4")
par(op)
#8 LINEAR REGRESSION	---------------------
# the function "lm" takes in an object of class "formula"
duncan.model.1 <- lm(prestige~income+education, data = Duncan)    
# notice that your assign the output to be an object (duncan.model.1 in this case)
duncan.model.1	
# class is "lm" - automatically assigned by the function "lm"
info(duncan.model.1)				
# summary deals with class "lm" as below; both predictors are highly significant: for every $1K gain in income, prestige tends to increase by .5987
summary(duncan.model.1)
# Looking at your data is crucial. Let's use the function we wrote above
MyPairs(Duncan[,2:4])

# check out all the goodies we have access to...
names(duncan.model.1)				
#can you figure out how to look at the residuals of the model?
duncan.model.1$residuals			# OK - I'll tell you. This works because mode(duncan.model.1) = list
# what's the most negative residual?
(d.min <- min(duncan.model.1$residuals))
# can you understand this syntax? Shows which profession has the least prestige given its income & educational level. 
Duncan[duncan.model.1$residuals==d.min,]         
# here are some default methods of graphing the linear model; let's get more specific...
plot(duncan.model.1) #hit your RETURN key IN THE CONSOLE to forward through the various plots
#A few notes:
#1) Leverage is how much POTENTIAL influence a point has (basically, how much of an outlier it is on the predictor)
#2) Cook's D tells you how much ACTUAL influence a point has (after leaving it out and rechecking the slope). You should double check highly influential points
#3) residuals ~ fitted helps us to see if there is homogeneity of variance across y-hat (the x-axis)
#4) qq-plot tells us if a variable (here residuals) follows what's expected from a given distribution (here, normal)



                                           

#9 REGRESSION DIAGNOSTICS - ASSESSING NORMALITY	---------------------
# qq.plot is a function from "car" package; are the resids normal?
qqPlot(duncan.model.1$residuals)
#Point 6 appears to be a bit of an outlier. What is it?
Duncan[6,] #makes sense - much more prestige than expected based on income
# The default of qqPlot is to plot the studentized residuals of an object of class lm (which use the leverage to account for fact that resids at extreme of x have larger variance)
qqPlot(duncan.model.1)	
#Note that point 6 appears to be a bit of an outlier when using regular residuals, but not after we account for the fact that it has higher uncertainty. The difference between this plot is that we let R use the default qqPlot behavior for class lm (studentized residuals) whereas the first time we used qqPlot, we explicitly provided it an object of class numeric and it changed its behavior accordingly
info(duncan.model.1$residuals)




#10 REGRESSION DIAGNOSTICS II - ASSESSING INFLUENCE VIA COOK'S D	---------------------
# function "coookd" returns the Cook's D measure of influence on the regression slope
duncan.cooks <- cooks.distance(duncan.model.1)	
duncan.cooks
# if you give a single vector to plot, it plots the vector as y and index (its order) as x - type='n' means "don't plot" - i.e., make an empty plot that we'll add to
plot(duncan.cooks,xlab="Row Number",type='n') 
# add the row numbers of cook's D to the plot
text(duncan.cooks,labels=as.character(1:length(duncan.cooks))) 
# our n = 45 and # parameters = 3
n <- 45; p <- 3						  
# rule of thumb for Cook's D > 4/(n-p) is high; the option "h" makes a horizontal line
abline(h=4/(n-p))		        
# here we add the Cook's D values onto the Duncan data.frame
Duncan$cooks <- round(duncan.cooks,3)
# looking at rows via indexing; make sure you understand this syntax
Duncan[duncan.cooks > 4/(n-p),]		





#11 REGRESSION DIAGNOSTICS III - ASSESSING LINEARITY & INFLUENCE	---------------------
# Let's construct some partial regression plots - these are nice for visualizing the (partial) slope you get in multiple regression, but are also helpful for detecting non-linearity & influential points. If there is nonlinearity, the data may move in a pattern about the linear line. If there is an influential point, it should stick out, away from the other points. 
avPlots(duncan.model.1)
#The lefthand plot is the residuals of prestige ~ education vs. residuals of income~education. The righthand plot is the same but now controlling for income instead of education. In the lefthand plot, you can see that "minister" is influential, reducing the effect of income on prestige

# Let's create a vector to be used later; all 1's except for when cooks D is large (=17)
duncan.points <- rep(1,nrow(Duncan))
duncan.points[duncan.cooks > 4/(45-3)] <- 17  #can you figure out this syntax? Give it a try!
duncan.points
# see ?points to figure out how to see what "17" means for points... its a triangle
?points			       
# partial regression plot showing big Cook's D's as triangles
avPlots(duncan.model.1,pch=duncan.points)  

#We can also JUST plot the partial plot of prestige on income holding education constant:
?avPlots
avPlots(duncan.model.1,terms = ~income, pch=duncan.points)  

#let's do the same thing as above, but do it ourselves to see exactly what's happening
#Make two windows
op <- par(mfrow=c(1,2))
# as above in the lefthand plot
avPlots(duncan.model.1,terms = ~income,pch=duncan.points)  
# Now making our own partial regression plot to put in the righthand plot
income.ed <- lm(income~education)$residuals
prestige.ed <- lm(prestige~education)$residuals
plot(income.ed,prestige.ed,pch=duncan.points,main="Our partial plot")  
abline(lm(prestige.ed ~ income.ed))  
par(op)

#12 REGRESSION DIAGNOSTICS IV - HOMOGENEITY OF VARIANCE	---------------------
# lets plot residuals against y-hat. This will help us see constant variance and whether non-linearity exists. Here's a reminder of what we have access to
names(duncan.model.1)					      
#Plot residuals on fitted values. This slope is always zero by definition. We don't see much of a pattern - linearity looks OK, and I don't see much increase or decrease in spread. Regressing the fitted values on the *absolute value* of the residuals can help us see homogeneity of variance easier
plot(duncan.model.1$residuals~duncan.model.1$fitted.values)     
abline(lm(duncan.model.1$residuals~duncan.model.1$fitted.values)) 
# this slope is NOT always zero again, we don't pick up much trend for variance to change - it increases slightly, but not much
plot(abs(duncan.model.1$residuals)~duncan.model.1$fitted.values)     
abline(lm(abs(duncan.model.1$residuals)~duncan.model.1$fitted.values))    

#13 REGRESSION DIAGNOSTICS V - COLINEARITY	---------------------
# the variance infation factor (vif) estimates how much variance of betas is increased by colinearity with other predictors. Because income and education are related, the variances of the betas are ~ 2x higher than if they were unrelated. vif() is another CAR function. Note: these are the same for both predictors when there are two predictors
vif(duncan.model.1)	
# the sqrt of this estimates how much higher the Std. Error is than it otherwise would be
sqrt(vif(duncan.model.1))  
#We can also calculate VIF by hand:
R2 <- summary(lm(education~income))$r.squared
(VIF <- 1/(1-R2))

#14 LINEAR REGRESSION II - DROPPING POINTS	---------------------
# "minister" & "conductor" professions are outliers - let's see how things change when we remove them. which() is a useful function you should put in your book of spells 
not.include <- which(rownames(Duncan)=="minister" | rownames(Duncan)=="conductor")  
#Check:
Duncan[not.include,] #includes them
Duncan[-not.include,] #excludes them

# this removes minister & conductor rows
duncan.model.2 <- lm(prestige~income+education,subset= -not.include) 
# same thing
duncan.model.2 <- lm(prestige~income+education,data=Duncan[-not.include,]) 
# compare the two; note that income slope increased and education decreased. Of course, whether to drop points depends on why the outlier exists... and may change your model's external validity
summary(duncan.model.1);summary(duncan.model.2) 

#15 LINEAR REGRESSION III - INTERACTIONS	---------------------
# Does the effect of income on prestige depend upon one's level of education? The syntax below says to include the two main effects + the interaction, which is specified using the : operator
duncan.model.3 <- lm(prestige~income+education+income:education)  
summary(duncan.model.3)
# this is shorthand for the above in the case of two predictors. If three+ predictors, it includes ALL interactions, including three, four, etc... higher order interactions
duncan.model.3 <- lm(prestige~income*education)      
summary(duncan.model.3)     

# But beware - the interpretation of main (or better, "simple") effects is unclear in the presence of an interaction when main effects are uncentered. This is because the main effect of one variable is interpretted at the ZERO LEVEL of the other variable. We can help our interpretation of regression parameters in general, especially in context of interactions, by "scaling" them - i.e., transforming all variables to z-scores. The "scale" function scales the variable (mean=0, sd=1)
sd(scale(income))
mean(scale(income))
duncan.model.4 <- lm(scale(prestige)~scale(income)*scale(education)) 
# now the simple effects are interpretable, but must be interpreted in a new scale. We can see that there is little evidence for an income by education interaction, but that the simple effects of income and education are similar to what they were in the original model
summary(duncan.model.3);summary(duncan.model.4)     




#16 LINEAR REGRESSION IV - CATEGORICAL VARIABLES	---------------------
# Now let' look at the effects of job "type" -  blue collar, professional, or white collar. Such categorical variables can be analyzed easily in regression - they just require coding schemes. In R, these types of variables are called "factors", and their coding schemes can be dictated by us although they are given default coding schemes 
class(type)			# class is factor - not ordered factor. This is important for interpretation
# what is the default coding scheme R has given "type"? Remember, we can change this default using options(contrasts=xxx)
contrasts(type)			
duncan.model.5 <- lm(prestige~type,data=Duncan)   # this says to include the main effect of type
summary(duncan.model.5)	# the first effect is how much higher in prestige prof is than blue collar. The second effect is how much higher in prestige wc is than blue collar. A common mistake is to think the (e.g.) first effect is how much higher prof is than the average of bc and wc, but this isn't right. Could you have figured this out? If not, look again at contrasts(type) and ask me to explain it.
#We can look at the means of the three groups using the function tapply:
?tapply
tapply(X=prestige,INDEX=type,FUN=mean,na.rm=TRUE)
#These agree with our interpretations of the two slopes above.

#We can change the type of default contrast by changing the factor to an ordered factor
type.ord <- ordered(type,levels=c('bc','wc','prof'))
type
type.ord
contrasts(type.ord)
duncan.model.6 <- lm(prestige~type.ord) 
summary(duncan.model.6)

# now let's create our OWN contrasts, which I often think is a better way to go than using R's default behavior. Here, we'll try to do the linear and quadratic coding for ordered factors, similar to above, but allowing for more user-friendly interpretation of betas
cont.lin <- (type=="bc")*-1
cbind.data.frame(type,cont.lin) #almost there; we still need to change prof to be 1
cont.lin[type=="prof"] <- 1
cbind.data.frame(type,cont.lin) #almost there; we still need to change prof to be 1
#Now create the second (quadratic) contrast
cont.quad <- (type=="wc")*3-1 #Can you see why this code works?
cbind.data.frame(type,cont.lin,cont.quad) 

#Run the model using our new contrast
duncan.model.7 <- lm(prestige~cont.lin+cont.quad)
summary(duncan.model.7)   
#Compare this to R's default coding for the same linear/quadratic for ordered factors
summary(duncan.model.6) #The p-values are identical, but the interpretations of betas is easier for duncan.model.7 I think.




#17 LINEAR REGRESSION V - MISSING VALUES	---------------------
set.seed(2)	# this is a useful function - it sets the random number generator seed for all random calls hereafter. This means everyone in the class will have the same runif() results in the next line of code
na.matrix <- matrix(runif(nrow(Duncan)*3),nrow=nrow(Duncan),ncol=3)
na.matrix[na.matrix<.05] <- NA
na.matrix[na.matrix>.05] <- 1
Duncan.na <- cbind(type,Duncan[,2:4]*na.matrix)
Duncan.na			# now we have ~ 5% missing data in Duncan
detach(Duncan)			# removed Duncan from the search path
type				# no object called this anymore!
summary(duncan.model.8 <- lm(prestige~type,na.action=na.exclude,data=Duncan.na) ) # this deletes cases if any are missing. Also note the use of the data argument rather than attaching the dataset; I recommend doing it this way in the future rather than attaching datasets.
# Important: na.exclude is NOT the default; na.omit is; these are almost the same, except that na.exclude allows residuals,  predicted values, etc. that are associated with missing data to be "NA" rather than simply not there. This is nice because it means, that there are as many (e.g.) residuals as there are rows in your original data frame. Try plotting original y values vs. residuals if this is not the case... not easy. I recommend using na.exclude whenever possible.

summary(duncan.model.5) 	# compare this model with all valid cases to the one above with missing data. If you want to get fancier, there are many advanced options in packages to impute missing data... casewise deletion can be a killer (can remove too much data) in large models (lots of variables)






#HW Problem 2 	---------------------
# Caveat: I cannot vouch for the accuracy of this data, although I have no reason to believe it is not accurate. Note: this is real data (missingness, the possibility of violated assumptions, etc) and there is no 'right' answer... 
	
# YOUR MISSION, SHOULD YOU CHOOSE TO ACCEPT IT: 
# Determine whether there is evidence for voter fraud as a function of the use of the "Diebold" vote counting machines in the NH 2008 primary. Do counties that use these machines tend to be biased towards a particular candidate? Note that you'll need to take into account demographic variables associated with different counties, since (e.g.) richer counties may be both more likely to endorse a given candidate and to use the Diebold counting machines. for more info, type in google, "New Hampshire democratic primary 2008 diebold"

# Variables that require explanation:
# Dean04, Kerry04 - pct votes for these two candidates in 2004 (Kerry was the establishment candidate whereas Dean was the upstart, analagous to Clinton & Obama in 2008). NOTE: the Dieblold machines only started to be used in 2008 - they were NOT used in 2004. Thus, you might check whether the same results you find in your primary analysis hold up for the 2004 election. If it does, this might suggest it is something about the counties themselves rather than diebold (alternatively, you might control for the degree to which Kerry beat Dean in 2004 in your main analysis)
#Clinton08, etc - pct votes for the 2008 primaries. I suggest making a single Clinton08-Obama08 variable (Clinton MINUS Obama ) your primary DV.
# diebold - if the county used these voting machines (=1), if not (= -1)
# lat, long - latitude and longitude
# Tot.LaborF, Tot.Unemp - I suggest dividing Tot.Unemp/Tot.LaborF to get the % unemployed to use as a variable
# PopDensity - person per square mile. I strongly suggest using the log of this variable. In R, do:
# LogPopDens <- log(PopDensity)   # (this of course assumes you have attached the dataset)

# (a) attach any library that you might use for your analysis and download then read in the data from the class website (its in comma delimitted format) 
library(tidyverse)
library(car)
prim2008 <- read.csv('~/Desktop/psyc5541/jrl/hw3/prim2008.csv')
head(prim2008)
attach(prim2008)
# what do we want our main variable to be? well, we are interested in whether or not the deibold machines had an impact on the election, thus we are interested in the difference in percetage voted for each candidate in 2008, more particularaly between Hilary and Barack
DV <- Clinton08 - Obama08
prim2008$DV <- DV
# we are also interested in how the distribution of this is impacted by a given population of a town
mean(DV)
weighted.mean(DV,TotPop)
#great we see the mean is clearly impacted when the population of each town is taken into consideration
#by suggestion from above we will also include the log transform of the variable PopDensity and compute the unemployment rate
logPopD <- log(PopDensity)
unempRate <- Tot.Unemp/Tot.LaborF
prim2008$logPopD <- logPopD
prim2008$unempRate <- unempRate 

# (b) analyze the data in any way you see fit. You can be as involved or simple as you'd like. Do make sure to check assumptions and produce basic graphs of the data that help you interpret what is going on. Make liberal use of comments to explain to me your interpretation of the data.

#lets now check out each variable and it's distribution kind of like EDA! 
# the best way to do this is with histograms so let's employ that with a bunch of our variables
#let's start with basic info related to demographic
par(mfrow=c(2,3))
hist(unempRate)
hist(logPopD)
hist(Medianage)
hist(TotPop)#VERY SKEWED
hist(log(TotPop))# way better

# lets hang on to the log transform of TotPop, as it could be relevant in our analysis
logTotPop <- log(TotPop)
prim2008$logTotPop <- logTotPop
# lets check out some variables related to education
par(mfrow=c(2,2))
hist(PcntHS.Grad)#skewed! 
hist(asin(sqrt(PcntHS.Grad/100)))#stolen from key unsure why it works so well
hist(PcntColl.Grad) #kinda skewed -- keep an eye on this

#now some graphs looking at income and financial measures
par(mfrow=c(2,2))
hist(MedianInc.) #transform
hist(log(MedianInc.))
hist(PerCapitaInc.)# neg skew, log transform
hist(log(PerCapitaInc.))# better
# we also want to hang onto Median Income, as this is a good measure for us
logMedInc <- log(MedianInc.)
prim2008$logMedInc <- logMedInc

#now we would like to check out the associations between our variables, let's see how much of an association there is between our difference in % voters and unemp rates
plot(DV~unempRate)
abline(lm(DV~unempRate)) # not a strong association, and a there are a looot of zeros

#now lets take a look at if our DV is associated with median income
plot(DV~logMedInc)
abline(lm(DV~logMedInc)) #not much association
#what about w/ college education?
plot(DV~PcntColl.Grad)
abline(lm(DV~PcntColl.Grad))#very nice!! a quite strong association!

# these graphs are nice, but we don't have any statistical measure of how much each variable is associated with our dependent variable DV. let's verage the power of cor() to get a matrix of these associations, stolen from key, ask why

round(cor(cbind(DV,logMedInc,logTotPop,unempRate,PcntColl.Grad,Medianage,logPopD,lat,long),use="pairwise.complete.obs"),3)

# these are all generally related, cool. let's now take a look at teh question we've all be waiting for, how much the difference in percentage is associated with diebold

diebold.md.1 <- lm(DV~diebold)
summary(diebold.md.1)

#wow, this is highly significant, let's take a closer look!
#it's important to control for the other variables that were asscoaited above, so lets do so below
diebold.md.2 <- lm(DV~diebold+PcntColl.Grad+unempRate+Medianage+logPopD+logMedInc,na.action=na.exclude,data = Duncan)
summary(diebold.md.2)#diebold is still signif, but age isn't lets remove it
diebold.md.3 <- lm(DV~diebold+PcntColl.Grad+unempRate+logPopD+logMedInc,na.action=na.exclude)
summary(diebold.md.3)#much more signif, that's cool!

#Lets use the useful info above to run some diagnostics (i.e. check residuals are ok and such)
#the first thing we will like to check is for normalcy by looking a the residuals with qqplot()
par(mfrow=c(1,2))
qqPlot(diebold.md.3$residuals)
# looks pretty normal
info(diebold.md.3$residuals)
# we want to test for colinearity too
vif(diebold.md.3)
#let's also look at the fitted values -- stolen from key
plot(abs(diebold.md.3$residuals)~diebold.md.3$fitted.values)
abline(lm(abs(diebold.md.3$residuals)~diebold.md.3$fitted.values))
#hmmmm a little skewed for sure
#lets check out cooks D now
md.3.cooks <- cooks.distance(diebold.md.3)
md.3.cooks

#let's build our plot like earlier in the script
par(op)
plot(md.3.cooks,xlab="Row Number",type='n') 
text(md.3.cooks,labels=as.character(1:length(md.3.cooks))) 
n <- length(DV);p<-length(diebold.md.3$coefficients)
abline(h=4/(n-p)) #woah! check it out theres quite a few above our horizontal line, lets see where they are in our data
prim2008$cooksD <- md.3.cooks
prim2008[md.3.cooks > 4/(n-p),c(1,2,5,7,8,11,21)]

#lets check for significance once we remove these values
include <- cooks.distance(diebold.md.3) < 4/(n-p)
prim2 <- cbind(prim2008,logMedInc,unempRate,logPopD,logTotPop,DV)
ls()
trim.data <- prim.data2[include,]
summary(diebold.md.4 <- lm(DV~diebold+PcntColl.Grad+unempRate+logPopD+logMedInc,na.action=na.exclude))
summary(diebold.md.4)
# still significant, that's great! 

#lets do a quick check to see how much the same was seen in the 04 prim
mod.04 <- lm(I(Kerry04-Dean04) ~ diebold+PcntColl.Grad+unempRate+logPopD+logMedInc,na.action=na.exclude)
summary(mod.04)
summary(mod.05 <- lm(DV~diebold+PcntColl.Grad+unempRate+logPopD+logMedInc+I(Kerry04-Dean04),na.action=na.exclude))
summary(DieModel.5 <- lm(DV~diebold+PcntColl.Grad+unempRate+logPopD+lat*long,na.action=na.exclude))

#As we see here there is a similar bias with diebold and Kerry in the 04 election, so it is somehat unclear, and the bias may be operating as function of geographical location

# (c) What is your final verdict: fraud or no fraud or uncertain? Justify your answer (put your answer as a comment below - i.e., a "#" and then your answer.
#     Who seems to benefit from the diebold bias and how big is the bias? (Note, there can be a bias even if the explanation for the bias is innocuous/no fraud)
# I will ask someone who did a nice job to go over their answer (in < 5 minutes) in class 4. If you do not want your HW to be in competition, please make a note of that here.

## UNCLEAR, there seems to be some bias, even after outliers are removed, but it is unclear what that is caused by. It is potentially due to geographical location as demostrated with Kerry, but futher analysis must be done to flesh this out. 

#






#THE END ---------------------------------



#@@Keller check hw3

#1c 
hw1c #loess curve interpretation

# 1d
hw1d #linear line interp














