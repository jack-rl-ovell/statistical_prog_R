
# LECTURE/HW #4 - GRAPHING AND STATISTICS FOR ANALYSIS OF VARIANCE MODELS


#1 R'S WORKING DIRECTORY --------------------------
getwd()				# Get working dirctory; asking R, "where is the default folder?"
setwd("~/Desktop/psyc5541/jrl/hw4")		# You're welcome to change this to whatever folder you'd like

#Read in a script I wrote that contains handy functions like info(x), LS(), and look(x). source() simply reads in another R script and runs every line of code
source("http://www.matthewckeller.com/R.Class/KellerScript2.R") 




#2 INSTALLING PACKAGE "CAR"  --------------------------
#install.packages("car",dependencies=TRUE)   # run only if you haven't already installed "car"
search()       # shows all *loaded* packages - "car" isn't there yet b/c we haven't loaded it in this session; just installed it 
library(car)   # now we've loaded it (if you get an error here, you need to install the package in your local machine)
search()       # note that "car"'s dependency ("leaps") is not yet loaded - it will load when needed (called "lazy loading")
LS(2)	       # what's in car? LS() is a function I wrote that is like ls() but provides more info




#3 MISSING DATA AGAIN AND SETTING OPTIONS --------------------------
options()$na.action	# the default is to omit missing data (for functions that ask for the default)
options(na.action='na.exclude')	# this is usually the best option; you might consider placing it in your RProfile.site
options()$na.action	# now the default for this session is na.exclude. Recall that na.exclude this is nice because na.exclude produces NA's for y-hats, residuals, etc associated with values that are missing rather than simply not having any values for these elements associated w/ missing values




#4 LOOKING AT DATA --------------------------
head(Duncan) #look at first 6 lines by default
?head #we can look at the possible arguments to hear
head(Duncan,n=12) #we can change the default
summary(Duncan)	# quick summary of the dataset
barplot(summary(Duncan$type))	# barplot of the data; gives the numbers in each profession type. summary() deals with Duncan$type by giving the numbers in each category because type is of mode factor
myformula <- Duncan$prestige~Duncan$type  # creating a formula
info(myformula)			# class = formula
boxplot(myformula)		# boxplot of the data - notice that boxplot does a particular thing with class = function
boxplot(prestige~type,data=Duncan)	# same thing
#IMPORTANT NOTE: Last HW, we attached Duncan so that we could save typing, e.g., "Duncan$type" throughout. However, I recommended against attaching data.frame because it can get confusing. In this HW, I will show the recommended way of dealing with data.frame, by either specifying the columns explicitly (using $ or an index[,]), or letting a function (like boxplot() above) know the data.frame we're interested in using




#5 SIMPLE ONE-WAY ANOVA OF DATA --------------------------
model.1 <- lm(prestige ~ type,data=Duncan)	# we've done this before in previous HWs
summary(model.1)		# what do the slopes mean?
contrasts(Duncan$type)			# we need to know the contrast to interpret the slopes! This type of contrast is called a "treatment" contrast in R, often referred to as a dummy contrast.
  # The intercept (when both contrasts are 0) is the mean for bc
  # 1st slope is difference in prestige between prof and blue collar
  # 2nd slope is difference in prestige between wc and blue collar
(mns <- tapply(Duncan$prestige,Duncan$type,mean))      # look at means by type
mns[2]-mns[1]   # look familiar? Check out the 1st slope in summary(model.1)
mns["wc"]-mns["bc"] #we can also index using the names for vectors with named elements. Compare to 2nd slope

anova(model.1)			# think of the "anova" function as akin to the "summary" function - just summarizing what happens to your model when a factor is dropped. Here we drop the factor "type", which has k-1=3-1=2 degrees of freedom (corresponding to the 2 slopes in regression). anova() answers the question, "How much worse is our fit when we use just the overall mean (intercept) to predict prestige vs. when we use the intercept plus two slopes?"

# this is how we fit a model with only the intercept (i.e., modeling the mean prestige)
model.2 <- lm(prestige ~ 1,data=Duncan)	
summary(model.2)		# this answers: is the average prestige greater than 0? Not too interesting...
anova(model.2,model.1)	        # this is really what's going on 'inside' anova when it is called as above: if you give anova two models, it subtracts the fit of the alternative model (the full model.1) from the fit of the null model (the intercept model.2); the  (diff in SS) follows an chi-sq distribution [not shown] whereas the ratio of (diff in SS)/df to RSS/df_within follows an F distribution with df 2,42.
anova(model.1)			# compare the F and p-values from this anova and the one above. Do you understand why they're the same? This should help you understand what "anova" does when you give it a model: by default, it merely is  comparing a full model to a model with an intercept only

# aov() is a function for fitting anova models directly
model.1.aov <- aov(prestige ~ type,data=Duncan)  
summary(model.1.aov)		     # same as above
				
	
	
									
#6 SPECIFYING YOUR OWN CONTRASTS BY CREATING NEW VECTORS --------------------------
# let's check out R's 'built in' contrast options:
contr.sum(4)				# if we had a factor with four levels, we'd have three contrasts (the columns)
contr.sum(3)				# here, 3 levels and two contrasts
contr.helmert(3)		# also called 'effects' coding
contr.poly(3)				# each contrast can be interpreted as a linear/quadratic/etc slope
contr.treatment(3)	# treatment contrasts; also called 'dummy' coding - the default in R

# Let's first make a 'helmert' or effects contrast on our own so we can see exactly how "lm" deals with factors
(code.prof <- (Duncan$type=='prof')*3 - 1)		    # let's first manually recode type to be 2 if 'prof' and -1 otherwise
(code.wc <- (Duncan$type!='prof')*((Duncan$type=='wc')*2-1) ) # now 0 if 'prof', -1 if 'bc', and 1 if wc
data.frame(tpp=Duncan$type,code1=code.prof,code2=code.wc)  # make sure we've done it right
model.3 <- lm(prestige ~ code.prof + code.wc,data=Duncan)       # regressing the contrasts we created by hand
summary(model.3)
anova(model.2,model.3)				    # what happens to model fit when we drop both contrasts? 

summary(model.1) #Note that the regression coefficients for model.1 and model.3 are different bc we used different contrasts
anova(model.2,model.1)				    # nevertheless, the ANOVA results are identical. Do you understand why?



#7 SPECIFYING YOUR OWN CONTRASTS USING THE CONTRAST() FUNCTION --------------------------
# Now let's do the same as above in another way, using the contrast() function
contrasts(Duncan$type)					     # default contrast for unordered factor
my.favorite.contrast <- matrix(c(-1,2,-1,-1,0,1),nrow=3)
my.favorite.contrast				     # this is exactly the same contrasts we just manually did above

# now let's change the "type" contrast - R remembers this for "Duncan$type" for this session only
contrasts(Duncan$type) <- my.favorite.contrast	
ls()					# notice that the function "contrasts" moves objects (here, Duncan) to .GlobalEnv as a side effect
contrasts(Duncan$type) 			# just like above
model.4 <- lm(prestige~type,data=Duncan)
summary(model.4);summary(model.3)	# these are exactly the same; one way done by hand (above); one way using contrasts()

(dunc.means <- tapply(Duncan$prestige,Duncan$type,mean,na.rm=TRUE))
#Try to interpret the beta coefficients of model.4 (or model.3) from the means. Why is the first beta equal to what it is? Perhaps the code below will help you understand this
summary(model.4)$coefficients[2,1]
(dunc.means[2] - mean(c(dunc.means[1],dunc.means[3])))/3




#8 SPECIFYING R'S DEFAULT CONTRASTS --------------------------
# Finally, we can do this by altering R's global contrasts - the limit of doing it this way is that you 
# can only choose from among R's default contrast types. Let's use R's 'helmert' coding
options()$contrasts #R's default for unordered then ordered factors
contrasts(TitanicSurvival$passengerClass)	#By default, we get contr.treatment for unordered. Let's change the global default
options(contrasts=c("contr.helmert","contr.poly"))  # changing the default R contrasts to contr.helmert for unordered factors
options()$contrasts
contrasts(TitanicSurvival$passengerClass)			   # same coding scheme as before, except the specific factors are associated with different codes





#!%! HW Problem 1 -------------------------- 
?Moore
Moore
str(Moore)
# 45 subjects came to lab and interacted with a confederate who was either high or low status (partner.status). Subjects made judgments that were ambiguous, and got to see how their partner (the confederate) responded. They could then change their judgments (conformity is the number of times they changed their judgment). Subjects were also measured as being low, medium, or high on authoritarianism (fcategory). Their quantitative score on the authoritarianism scale is also included (fscore).

# (a) Use the function par(mfrow=xxx) to produce a 2x2 plot (see HW3 if you're having trouble recalling how to do this). The top left is a barplot of # of people in each fcategory. Top right is histogram of conformity. Bottom left is a scatterplot of fscore predicting conformity (include best fit line). Bottom right is a boxplot of the amount of conformity as a function of fcategory. (NOTE: if you receive an errors stating "Error in plot.new(): figure margins too large", this sometimes happens in RStudio because the graphics window is too small; try to make that window larger before running your code.)
par(mfrow=c(2,2))
barplot(summary(Moore$fcategory))
hist(Moore$conformity)
plot(conformity~fscore,data=Moore)
abline(lm(conformity~fscore,data=Moore))
boxplot(conformity~fcategory,data=Moore)
# (b) You might notice that your boxplot of conformity as a function of fcategory is in a weird order.  In a single figure (i.e., change your default back to a 1-by-1 figure using mfrow in par), remake the boxplot such that its order is more sensible. To do this, reassign the "fcategory" in the Moore dataset to be an ordered factor (see ?ordered) using ordered() and argument levels=c("low","medium","high")
par(mfrow=c(1,1))
Moore$fcategory <- ordered(Moore$fcategory,levels=c("low","medium","high"))
boxplot(conformity~fcategory,data=Moore)
# (c) Moore$fcategory now should be an ordered factor (from hw1b above) where low < medium < high. Run a one-way ANOVA with fcategory predicting conformity using the lm() function and then getting anova output using the anova() function. Create an object "hw1c" that is the anova() output of this
hw1c <- summary(lm(conformity~fcategory,data=Moore))
anova(lm(conformity~fcategory,data=Moore))
# (d) Create an object "hw1d" that is your interpretation of the first slope testing the linear effect in summary() of your lm() model above. Use contrasts() to help you remember the contrast coding. 
hw1c
contrasts(Moore$fcategory)
hw1d <- "no conformity from low to high authoritarianism. no difference in comformity for medium auth. when compared to other groups"
# (e) As you know, anova() is really just a way to summarize linear model results; thus you can use all our old tricks (from HW3) to check assumptions. Use plots to check normality of residuals, cook's D, and homogeneity of variance of the model you conducted in hw1c
# we only have one predictor so we wont check for colinearity
# Let's check to see if there normality using qqPLot
hw1.model <- lm(conformity~fcategory,data=Moore)
qqPlot(hw1.model) #nothing crazy, that one point and 27 are a little suspicious
#let's just check to see if there are any values to be wary of anyway, using cook's D
cooksD <- cooks.distance(hw1.model)
Moore$cooksD <- cooksD
n <- nrow(Moore);p <- length(model.1$coefficients)
plot(cooksD,xlab="Row Number",type='n')
text(cooksD,labels=as.character(1:length(cooksD))) 
abline(h=4/(n-p))	
# so 16, 19, and 27, let's take a look at each
Moore[cooksD > 4/(n-p),]
#makes sense, lets drop them
not.include <- cooksD > 4/(n-p)
Moore_no_outliers <- Moore[!not.include,]
hw1.model2 <- lm(conformity~fcategory,data=Moore_no_outliers)
anova(hw1.model2)
#difference are still non significant, nice job!

# (f) let's say we want to compare high vs. the average of medium and low fcategory in the first contrast, and then only medium vs. low in the second. Create a new variable, fcategory2, that reflects these new contrasts using the contrast() and matrix() functions, as we did above in #7 above. Then run the summary(lm()) of this model. Assign that summary(lm()) to object called "hw1f".

fcategory2 <- matrix(c(-1,-1,2,-1,1,0), nrow = 3)
contrasts(Moore$fcategory) <- fcategory2
contrasts(Moore$fcategory)

hw1f <- summary(lm(conformity~fcategory, data = Moore))
hw1f
#9 TWO WAY ANOVA - GRAPHICS --------------------------
# now let's explore the question the researchers actually asked: does conformity change as a function of partner status and one's own authoritarianism? First, let's ensure that everyone is treating the factor in the same way. It would be fine to make fcategory ordered, but for now, let's keep it unordered.
options(contrasts=c("contr.helmert","contr.poly"))  
Moore$fcategory <- factor(Moore$fcategory,levels=c("low","medium","high"),ordered=FALSE)  

# do we have balanced data? no! cell numbers are not all equal
table(Moore$fcategory,Moore$partner.status)	

# "tapply" is a very useful function! Learn how to use it.
tapply(X=Moore$conformity,INDEX=Moore$fcategory,FUN=mean,na.rm=TRUE)   #means of conformity as a function of fcategory. 
tapply(Moore$conformity,Moore$partner.status,mean,na.rm=TRUE)# means of conformity as a function of partner status. Note that here, we leave out the explicit argument names and rely on their order for R to know which argument goes with which input

# so, thus far, we've explored the main effects of the two variables. Now let's check out the interaction effect:
(moore.means <- tapply(X=Moore$conformity,INDEX=list(Moore$fcategory,Moore$partner.status),FUN=mean,na.rm=TRUE))  # means of the 6 cells. 

# this is R's default for boxplot of an interaction formula
boxplot(conformity~fcategory*partner.status,data=Moore)    

#Let's tailor the default boxplot to make it look nicer
boxplot(conformity~fcategory*partner.status,data=Moore,
		col=c('red','red','red','blue','blue','blue'),
		names=c('lo auth','med auth','hi auth','lo auth','med auth','hi auth'))

#We can also add a legend to an existing plot. There are several "low level" plotting functions that add points, lines, text, legends, etc. to existing plots. This allows for a great deal of customization
legend(3.5,24,c("high partner status","low partner status"),c('red','blue'),cex=1.25)

# let's look at one last graph - this one is a bit complicated so no worries if you don't 'get it' all for now. This was cribbed from "Companion to Applied Regression". For more advanced users: if you work through this line by line (and bit by bit),  you might get some ideas about how it works... but we'll cover customized graphs later in the course. For plots like these, it's often useful to use the "Zoom" button in Rstudio (below) to see the plot as it's meant to be seen
myplot <- function() {Fcat <- as.numeric(Moore$fcategory)
	plot(c(.5,3.5),c(0,30),xlab='Authoritarianism category',ylab='Conformity',type='n',axes=FALSE)
	axis(1,at=1:3,labels=c('low','medium','high'))
	axis(2)
	points(jitter(Fcat[Moore$partner.status=='low'],amount=.05),Moore$conformity[Moore$partner.status=='low'],pch='L',col='blue')
	points(jitter(Fcat[Moore$partner.status=='high'],amount=.05),Moore$conformity[Moore$partner.status=='high'],pch='H',col='red')
	lines(1:3,moore.means[,1],lty=1,lwd=3,type='b',pch=19,cex=2,col='red')
	lines(1:3,moore.means[,2],lty=3,lwd=3,type='b',pch=19,cex=2,col='blue')
	legend(30,c("high partner status","low partner status"),cex=1.25,lty=c(1,3),pch=c(19,1),col=c('red','blue'),bty='n')}
myplot()




#10 TWO WAY ANOVA - INTERPRETATION OF THE LINEAR MODEL SUMMARY --------------------------
# from the last graph, we can see that an interaction looks possible, but there is also a  concern about homogeneity of variance that we should keep in mind. Before continuing, let's specify the contrasts we want
contrasts(Moore$fcategory) <- matrix(c(-1,1,0, -1,-1,2),nrow=3)
contrasts(Moore$partner.status) <- contr.helmert(2)
contrasts(Moore$fcategory)
contrasts(Moore$partner.status)		# remind us of what the contrasts are
Moore$partner.status <- factor(Moore$partner.status,levels=c('low','high'),ordered=FALSE) # R simply uses alphabetic order to figure out the lowest contrast. Here, we change it to make sure we have 'high' as 1 and 'low' as -1 
contrasts(Moore$partner.status)		# more intuitive that way, don't you think?
moore.mod <- lm(conformity ~ fcategory + partner.status + fcategory:partner.status,data=Moore)
summary(moore.mod)

# how do we interpret the interaction contrasts?. One way to see all the combinations is to take the outer product of the two contrasts
?outer
#just to see what an outer product does.
c(1,2,3) %o% c(4,5)  #The first vector is assumed to be a column vector and the second vector a row vector, meaning r-by-1 %*% 1-by-c = r-by-c matrix

#This does the same thing using matrix multiplication 
matrix(c(1,2,3),nrow=3) %x% matrix(c(4,5),ncol=2)

#Now let's use an outer product to figure out interaction contrasts
contrasts(Moore$fcategory)[,1] %o% contrasts(Moore$partner.status) # This is the first interaction contrast. But be careful - this really should be one long column to be a contrast - not two. This says that low/low and high/high people are 1, high/low and low/high are -1 and medium/low and medium/high are 0 (ignored for this contrast).

# this is a better way to look at it
intcon1 <- matrix(contrasts(Moore$fcategory)[,1] %o% contrasts(Moore$partner.status),ncol=1,dimnames=list(c('lo self/lo partner','med self/lo partner','hi self/lo partner','lo self/hi partner','med self/hi partner','hi self/hi partner'),'fcategory1:partner.status1'))

intcon2 <- matrix(contrasts(Moore$fcategory)[,2] %o% contrasts(Moore$partner.status),ncol=1,dimnames=list(c('lo self/lo partner','med self/lo partner','hi self/lo partner','lo self/hi partner','med self/hi partner','hi self/hi partner'),'fcategory2:partner.status1')) 
cbind(intcon1,intcon2)

(int.interp <- cbind(intcon1,intcon2,matrix(round(moore.means,3),ncol=1),0,0)) # now we can interpret the interaction effects more easily
colnames(int.interp) <- c('cont1','cont2','means','prod1','prod2')
int.interp[,4:5] <- c(int.interp[,1]*int.interp[,3],int.interp[,2]*int.interp[,3])

#This helps in interpretation of the interactions & main effects as well
int.interp	   
summary(moore.mod)
(mean(int.interp[c(2,4),'prod1']) + mean(int.interp[c(1,5),'prod1']))/2  #slope of int 1
(mean(int.interp[c(1,2,6),'prod2']) + mean(int.interp[3:5,'prod2']))/4   #slope of int 2
(mean(int.interp[4:6,'means']) - mean(int.interp[1:3,'means']))/2    #slope of patner.status
(mean(int.interp[c(2,5),'means'])- mean(int.interp[c(1,4),'means']))/2    #slope of fcategory1
#I just noticed that the signs of the contrasts above are opposite of those in summary(). I'll try to track down why - or maybe you can?



#11 TWO WAY ANOVA - INTERPRETATION OF THE ANOVA SUMMARY --------------------------
# now let's look at another type of summary of linear models - analysis of variance. As said before, ANOVA tables started off as a way to compare nested models. However, when we have non-orthogonal contrasts AND/OR we have unequal cell sizes, there is redundancy in the contrast tests, and therefore order matters when testing contrasts (testing contrast1 then contrast2 will give appear to make contrast2 less important vs. testing contrast2 then contrast1). Hence the need to tell the ANOVA test what the order is that we're interested in.

#All that business about "type I" vs "type II" vs "type III" came later, as a time saver, an invention of SAS corp. Type I Sums of Squares = the original (some say only) type of sums of squares. Each subsequent term tests the term in question controlling for all terms coming before. *Order matters!* This is the R default. Type III Sums of Squares = each term is tested *as though it were* the last term in the equation in a SS type I model.

#	In other words, Type II says that the var. for each term is the *unique* portion of var. accounted for by that term. Some experts hate type III for the same reason that it makes no sense to drop a simple effect in the presence of interactions (the SS for the simple effects is what it would be if the simple effect were entered last). 

# BEWARE: SAS & SPSS give type III by default. Type I is the R default, and to get type III, you have to use an anova function in other packages, such as the "car" package.. Of course, none of this will matter if your predictors are uncorrelated with each other (which occurs naturally with balanced data and orthogonal contrasts).

moore.mod <- lm(conformity ~ fcategory + partner.status + fcategory:partner.status,data=Moore)
summary(moore.mod)		# anova summary (type I sums of squares)
moore.mod2 <- lm(conformity ~ partner.status + fcategory + fcategory:partner.status,data=Moore)
summary(moore.mod2)
anova(moore.mod);anova(moore.mod2)     # compare these two. Why do the first two terms have diff p values between models? A: they are different because order matters for type I SS. The p and F values associated with partner.status in the second model asks "what is the drop in fit from a model with only intercept versus a model with intercept + partner.status?" In the first model, this question is "what is the drop in fit from a model with intercept + fcategory versus a model with intercept + fcategory + partner.status?"

# Anova() in car allows you to use type III if you want
Anova(moore.mod,type="III"); Anova(moore.mod2,type="III") 
# note that order no longer matters (because of type III SS). Just realize that the interpretation of the simple effect here is "what is the drop in fit from a model with all terms except one simple effect (and incl. interactions) versus a model with all terms?" But does it make sense to discuss a model with an interaction that doesn't include main effects? If you think it does, then SS type III are OK. If not, maybe not. Also beware: if you are going to use type III SS, make sure you use orthogonal contrasts.





#12 TWO WAY ANOVA - NONPARAMETRIC TESTS --------------------------
# let's first take a look at some diagnostic plots
op <- par(mfrow=c(2,2))
qqPlot(moore.mod2$residuals)
plot(cooks.distance(moore.mod2),xlab="Row Number")
abline(h=4/(45-3))
plot(abs(moore.mod2$residuals)~moore.mod2$fitted.values)    
abline(lm(abs(moore.mod2$residuals)~moore.mod2$fitted.values))    # this slope is always zero by definition 
myplot()
par(op)

# we can see some outliers here. There also seems to be more variance at moderate values of y-hat. What to do? We could try transforming the y variable. We could try rerunning after dropping high cook's D. But another option is the use of non-parametric tests
cbind(Moore$conformity,rank(Moore$conformity))			# showing the "rank" function

# for one-way ANOVA, the following is fine (nonparametric rank F test):
mod.oneway <- lm(rank(conformity) ~ fcategory,data=Moore)
summary(mod.oneway)
anova(mod.oneway)	# you will tend to lose power, but you don't have to meet normality & equality of variance

# for two or more -way ANOVA, this test can give inflated *or* deflated alphas... tread carefully (see Toothaker & DeNewman, 1994)
mod.twoway <- lm(rank(conformity) ~ partner.status + fcategory + fcategory:partner.status,data=Moore)
summary(mod.twoway)
anova(mod.twoway)	# it's hard to trust these results, 
anova(moore.mod2)       # although they are similar to the other results, a better option in my opinion is to use bootstrap confidence intervals & p-values if we have violation of distributional assumptions; we'll get to bootstrapping later in the course




#13 WITHIN SUBJECT ANOVA --------------------------
# repeated measures ANOVA is an issue fraught with pitfalls; I am going to show you the 'typical' approach that is most akin to the one used in SPSS. This works fine for 'balanced' designs (where the numbers in each cell are equal), but it can be problematic in unbalanced designs. Later in the class, we'll cover a couple of other approaches to repeated measures designs with unbalanced data - linear mixed effects and bootstrapping. If you have repeated measures with unbalanced designs, I do not recommend the approach below. These examples come from Baron & Li, 2003. Each subject responds to buttons of square/round or red/blue; measure reaction times
two.within <-c(49,47,46,47,48,47,41,46,43,47,46,45, 48,46,47,45,49,44,44,45,42,45,45,40, 
	49,46,47,45,49,45,41,43,44,46,45,40, 45,43,44,45,48,46,40,45,40,45,47,40)
(two.within.mat <- matrix(two.within, ncol= 4, byrow=FALSE, dimnames = list(paste("subj", 1:12), c("Square.Red", "Circle.Red", "Square.Blue", "Circle.Blue"))) )

# here's how we set it up for analysis in R - one line per measurement (not per subject!)
Hays <- data.frame(rt = two.within,
				subj = factor(rep(paste("subj", 1:12, sep=""), 4)), 
				shape = factor(rep(rep(c("square", "circle"), c(12, 12)), 2)), 
				color = factor(rep(c("red", "blue"), c(24, 24)))) 
Hays
str(Hays)

# this code is WRONG - it doesn't account for shapes or colors nested in subjects
hays.mod1 <- aov(rt ~ shape + color + shape:color,data=Hays) 
summary(hays.mod1)
hays.mod1 <- aov(rt ~ shape*color,data=Hays) 		# note: this is a shortcut for the above formula. Notice also that +, *, and : mean something different when they are "formulas" (used inside the aov() and lm() and other functions). See ?formula
summary(hays.mod1)

# this code CORRECTLY specifies the within subject component; it says combos of shape & color are nested within subject
hays.mod2 <-  aov(rt ~ shape*color + Error(subj/(shape*color)),data=Hays) 
summary(hays.mod2)		# the "Error" option above is smart - R knows which error terms need to be used for which term. A good rule of thumb for fully crossed repeated measures = the part after "subj/" should follow what you have in your formula



#14 BETWEEN/WITHIN SUBJECT ANOVA --------------------------
# now let's say we have one between factor and two within factors
Ela.mat <-matrix(c(19,22,28,16,26,22,11,19,30,12,18,28, 
20,24,24,24,22,29,21,25,25,15,10,26, 
18,24,29,19,26,28,17,23,28,15,23,22, 
20,23,23,26,21,28,14,20,29,25,29,29, 
16,20,24,30,34,36,26,26,26,24,30,32, 
22,27,23,33,36,45,16,18,29,27,26,34, 
19,21,20,22,22,21,20,25,25,29,29,33, 
21,22,23,27,26,35,17,20,22,23,26,28), nrow = 16, byrow = T) 

# this format is easier to see but not how R 'wants' it
Ela.wide <- cbind(subj=1:16, gp=factor(rep(1:2,rep(8,2))), Ela.mat) 
colnames(Ela.wide) <- c("subj","gp","d11","d12","d13","d21","d22","d23") # d12 = drug 1, dose 2, etc. 

# now let's put the dataset in the long format - one line per measurement
Ela <- data.frame(
  effect = as.vector(Ela.mat), 
  subj = factor(paste("s", rep(1:16, 6), sep="")),
  gp = factor(paste("gp", rep(rep(c(1, 2), c(8,8)), 6), sep="")), 
  drug = factor(paste("dr", rep(c(1, 2), c(48, 48)), sep="")), 
  dose=factor(paste("do", rep(rep(c(1,2,3), rep(16, 3)), 2), sep="")), 
  row.names = NULL) 

# we have each & every patient getting three doses of two drugs. There are two (between-subjects) groups, gp1 & gp2
tapply(Ela$effect, IND = list(Ela$gp, Ela$drug, Ela$dose),FUN = mean) # look at means for each cell
boxplot(effect ~ gp,data=Ela)
boxplot(effect ~ drug*dose,data=Ela)
boxplot(effect ~ gp*drug*dose,col=rep(c('red','blue'),6),data=Ela)

ela.mod1 <- aov(effect ~ gp * drug * dose + Error(subj/(dose*drug)),data=Ela) 
# note: we don't want to put Error(subj/(dose*drug*gp)) here - groups are not nested within subjects!
summary(ela.mod1)





##!%! HW Problem 2 --------------------------
#  Simulation of data is often a useful tool in statistical analysis - it allows us to know whether our model recovers what we put into it. Run the following code, which simulates a within/between dataset:
set.seed(98765)
error <- round(sqrt(.3)*rnorm(80),3)
sub <- round(rep(sqrt(.4)*rnorm(20),4),3)
dep <- rep(c(-.3,.3),40)
act <- rep(c(-.04,.04),each=40)
drg <- rep(rep(c(-.02,.02),each=20),2)
drg.act <- 100*act*drg
dep.drg.act <- -1000*act*drg*dep
md <- sub+dep+act+drg+drg.act+dep.drg.act+error

DrugStudy <- data.frame(
  mood = md, subj = factor(paste("s", rep(1:20, 4), sep="")), 
  depressed = factor(rep(c("yes","no"), 40)), 
  activity = factor(rep(c("relaxation","exercize"),each=40)),
  drug=factor(rep(rep(c("placebo","SSRI"),each=20),2)),row.names = NULL) 

DrugStudy

# if you want to 'cheat' & see what's going into each subject's mood score, run the following:
cbind(DrugStudy,sub,dep,act,drg,drg.act,dep.drg.act,error) 
# by looking at this, is it obvious what the "fixed" factors are and what the "random" factors are?

# mood is the subject's mood after the study
# subj is the subject number - each subject came to lab 4 times
# depressed is whether the subject scored as "depressed" on a pretest - between subject factor
# activity is the activity the subjects performed in the lab
# drug is whether the subject received a placebo or a superdose of an SSRI


# (a) What contrasts are being used for each factor? If you want to change them, do so.
str(DrugStudy)
contrasts(DrugStudy$depressed)
contrasts(DrugStudy$activity)
contrasts(DrugStudy$drug)

# these all make sense, for now.

# (b) First analyze this data as if each row were an independent subject.
#  What is the effect of activity, drug, and depressed? Are there any interactions?
#  Are assumptions upheld? Make sure to produce graphs that help you see the data as well as graphs that help you check assumptions.

par(mfrow=c(2,2))
summary(lm(mood ~ depressed * activity * drug,data=DrugStudy))
plot(mood ~ depressed * activity * drug,data=DrugStudy)
# it seems like there is not much of an effect from any of the factors
#although here we are not modeling subj as random effects! due to the nested nature of the data we must consider this

# (c) Next, analyze the data correctly. Drug and activity are nested within subject, but depression is not.

#to address this issue I am going to take examples from bogdan petres walkthrough on modeling twin data from the paingen study

library(lme4) # fits LME model usin REML
library(lmerTest) # gets p-values using Satterthwaite corrected df

# this model will fit a linear model with varying group-intercepts, where group in this case is depressed patients
mdl.1 <- lmerTest::lmer('mood ~  activity + drug + (1 | depressed)', data = DrugStudy)
summary(mdl)
plot(mdl)
# it looks like there isnt much of an effect
#now lets check out nested conditions
mdl.2 <- lmerTest::lmer('mood ~  activity + drug + (1 | depressed/subj)', data = DrugStudy)
summary(mdl.2)
plot(mdl.2)
# it seems that there is still no effect
#lets check out varying slops
# the model below is telling us: vary the the slope of the impact of drug based on the subjects nested in depression
mdl.3 <- lmerTest::lmer('mood ~ activity + drug + (1 + drug|depressed/subj)', data = DrugStudy)
summary(mdl.3)
plot(mdl.3)
mdl.4 <- lmerTest::lmer('mood ~ activity + drug + (1 + activity|depressed/subj)', data = DrugStudy)
summary(mdl.4)
plot(mdl.4)
#again we see no effect but some interesting correlations in the summary

## adding key because i am unsure if the above is correct...
mymod <- aov(mood ~ depressed * activity * drug + Error(subj/(activity*drug)),data=DrugStudy) 
summary(mymod)
plot(mood~depressed*activity,data=DrugStudy)
boxplot(mood~depressed*activity*drug,data=DrugStudy)
aggregate(DrugStudy$mood,by=list(DrugStudy$depressed,DrugStudy$activity,DrugStudy$drug),mean)
tapply(DrugStudy$mood,INDEX=list(DrugStudy$depressed,DrugStudy$activity,DrugStudy$drug),mean)
# forgot to model the interactions in the mixed effects model, it is clear from the above that there is certainly an interaction. 
#if you look at the summary of mdl.4 there are some interesting correlations which may hint at this as well 