
# LECTURE/HW #9 - LOW LEVEL GRAPHING IN R
# Note: last time we learned high level graphics, using functions such as "hist()" or "plot()" to get what we need. In this HW, we'll learn how to customize these graphics and manipulate them with calls to additional (low level) graphics functions. This opens up a lot more fine-tuning of our graphics.

# The basic idea is to make a primary plot using, e.g., plot() or hist(), and then to add to that plot using low level functions such as axes(), text(), titles(), legends(), lines(), etc...




# 1 Basics -------------------------------
# a) Change your working directory to wherever you want it to be. All graphics you write out (save) will go into this folder
setwd("/Users/jacklovell/Desktop/psyc5541/jrl/hw9")
# b) Read in a script I wrote that contains handy functions like info(x), LS(), and look(x). source() simply reads in another R script and runs every line of code
source("http://www.matthewckeller.com/R.Class/KellerScript2.R") 

# c) Load the "car" library. This has datasets that will be fun to plot
require(car)
# d) save the default par() values so that we can always come back to these. Call this object "default.par.vals"
default.par.vals <- par(no.readonly=TRUE)

# e) use the mvrnorm() function in the MASS library to create a 100 x 3 matrix. Make each mean be 0 and var be 1. Columns 1 & 2 should be correlated at r=.5, 2 & 3 r=.7, 1 & 3 r=.3. Call this matrix "sim.males". Set your seed to 1000. We'll use this dataset later. NOTE: if you want to just use one particular function in a library, instead of loading the entire library, you can also do this: <Library>::<function>. E.g., for mvrnorm, you could do this: MASS::mvrnorm(n=...) instead of require(MASS) then mvrnorm(n=...)
require(MASS)
set.seed(1000)
Sigma.m = matrix(c(1,.5,.3,.5,1,.7,.3,.7,1),3,3,byrow = T)
sim.males <- mvrnorm(n=100, rep(0,3), Sigma.m)
sim.males
# f) check out sim.males. Use cor() to find the correlation matrix of sim.males. Similar to expected?
cor(sim.males)
#nice
# g) Make another matrix as above called "sim.females". Each mean is .1 and var 1. Columns 1 & 2 should be correlated  at r=.2, 2 & 3 r=.5, 1 & 3 r=.1. Set your seed to 1000. We'll use this dataset later.
set.seed(1000)
Sigma.f = matrix(c(1,.2,.1,.2,1,.5,.1,.5,1),3,3,byrow = 1)
Sigma.f
sim.females <- mvrnorm(n=1000, rep(.1,3), Sigma.f)
sim.females
cor(sim.females)
# 2 One last high level function - 3-D graphics using persp()!-------------------------------
# example from the R manual
?volcano
#volcano gives you the height of each cell in the 87x61 matrix. Note that info() isn't loaded, so cal in my R script (from previous HWs) to grab this function
info(volcano)  
# we need to rotate it for a better perspective
persp(z=volcano) 

# a) here, we'll make a plot with 4 columns and 3 rows using par(mfrow=). Make each column will be one of 4 positions in the azimuthal direction (controled by theta=) and each row will be one of one of 3 positions in the colatitude direciton (phi=). We'll rotate theta from 0 to 270 and phi from 0 to 90. In the title of each graph, we'll give the theta= & phi=. We'll accomplish this task using persp() within a for loop.
#NOTE: If you get "figure margins too large", either resize the window or, better yet, open up a new graphical device outside of RStudio using windows() or quartz()
quartz(height=3*4,width=4*4) #change to windows() if on windows machine
par(mfrow = c(3, 4))
thetas <- rep(seq(0, 270, length.out = 4), times = 3)
phis   <- rep(seq(0, 90,  length.out = 3), each  = 4)
for (i in 1:12) {
        persp(z = volcano, theta = thetas[i], phi = phis[i],
              main = paste("theta = ", thetas[i], ", phi =", phis[i]))}
par(default.par.vals)
dev.off()

# OR
quartz(height=3*4,width=4*4) #change to windows() if on windows machine
par(mfrow = c(3, 4))
thetas <- rep(seq(0, 270, length.out = 4), times = 3)
phis   <- rep(seq(0, 90,  length.out = 3), each  = 4)
mapply(persp, z = list(volcano), theta = thetas, phi = phis,
       main = paste("theta = ", thetas, ", phi =", phis))
par(default.par.vals)
#dev.off()

# b) find the angle you like the best above and graph it in a single plot, coloring the volcano green3 and shade it such that it approximates daylight illumination (see ?persp)
persp(z = volcano, theta = 270, phi = 0, col = "green3", shade = .69)


# c) Use lphi= argument to change the plot so that the 'sun' is coming from behind your figure at a phi of 60
persp(z = volcano, theta = 320, phi = 45, col = "green3", shade = .69, lphi = 60)


# d) make the same figure as above, but don't include grid lines or a box around it
persp(z = volcano, theta = 320, phi = 45, col = "green3", shade = .69, lphi = 60, box = 0, grid = F)

# e) run the following to see terrain colored volcano:
op <- par(bg='grey')
facet.avgs <- volcano[ -1,-1] + volcano[ -1,-61] +volcano[-87,-1] + volcano[-87,-61] 
volc.cuts <- cut(facet.avgs, seq(min(facet.avgs),max(facet.avgs), len = 21), include.lowest = TRUE)
col.terr <- matrix(terrain.colors(20)[volc.cuts],nrow=nrow(facet.avgs),ncol=ncol(facet.avgs),byrow=FALSE)
persp(z=volcano,theta=90,phi=45,col=col.terr,shade=.63,ltheta=270,lphi=60,box=FALSE,border=NA)
title("R Graphics Rock",col.main='darkred',cex.main=2)
par(op)






# 3 Adding points, lines, legends, and text to existing graphs -------------------------------
# What if we want points of two different colors, depending on some factor (e.g., gender)? Or if we want to plot both points and lines? You cannot do this with just a single call to plot() (try it if you don't believe me). But we can if we call the lower level points() and lines() functions. Run the following - Do this LINE BY LINE to see what is going on!
set.seed(123)
x <- seq(0,2*pi,length.out=51)
sinx <- sin(x)
sinx2 <- (sinx + sqrt(.1)*rnorm(51))
# notice that type='n' means "don't draw" - just set up the coord's
plot(x,sinx,type='n',main="sin plot with and without noise", col.axis = "yellow", col.main = "yellow", col.lab = "yellow") 
lines(x,sinx)
# notice the pch= argument changes the type of point
points(x,sinx,pch=25) 
# adding random noise to sin(x)
points(x,sinx2,pch=18) 
par(op)
# a) Notice that, in the plot above, some of the random noise points are off the chart and therefore don't show up. Redo the same plot above (using the same x, sinx, sinx2) but make sure that no points are left off the plot. Also, make the background be black (see par(bg=)), the line be yellow, the triangles to be red, and the diamonds orange
op <- par(bg = "black")
plot(x,sinx2,type='p', pch=18, col = "orange", main="sin plot with and without noise",) 
lines(x,sinx,col = "yellow")
points(x,sinx,pch=25, col ="red")
par(op)
# b) You'll notice that the graph above doesn't have a title, a box around the plot, x-axis labels or y-axis labels. Redo the plot above but make the axes be yellow, the axes annotation be green and the title be yellow (see col= col.axis= & main.col= arguments in par()). Add tick marks to the x-axis using the axis() function (specifying col="yellow")
op <- par(bg = "black",col.axis = "white", col = "white", col.main = 'yellow')
plot(x,sinx2,type='p', pch=18, col = "orange", main="sin plot with and without noise",) 
lines(x,sinx,col = "yellow")
points(x,sinx,pch=25, col ="red")
par(op)

# c) Let's say we want to add a legend to the graph above to tell readers what the two points refer to. Do so using legend(). Make the text colored yellow and the points as they are in the figure (obviously)
legend("bottomright", legend = c("sin x", "sin x + noise"), col = c('red', 'orange'), text.col = 'white', pch = c(25,18))


# d) You can also add text and arrows (etc) to graphs where you like. E.g.,
arrows(1.57,0,1.57,.8,col='white')
text(1.57,-.1,"1st derivative=0",col='white')
# See if you can figure out a way to add a white arrow to the plot that points to the point that has the biggest absolute residual from the sin curve then place text @ end of arrow saying "largest abs. residual" (this one is kind of tricky)
?arrows
max <- max(abs(sinx2 - sinx))


# e) You could place multiple arrows by using vectors at the coordinates, e.g.:
op <- par(bg='black') 
plot(x,sinx2,type='n',main="sin plot showing residuals",col.main='yellow') 
axis(1,col="yellow",col.axis='green') 
axis(2,col="yellow",col.axis='green') 
lines(x,sinx,col='yellow') 
points(x,sinx,pch=25,col='red') 
points(x,sinx2,pch=18,col='orange') 
legend(4,1.2,legend=c("Noise","No Noise"),pch=c(18,25),col=c('orange','red'),text.col='yellow') 
arrows(x,sinx2,x,sinx,code=3,col='white',length=.1,lwd=.5)
par(op) 




# 4 Overlaying plots -------------------------------
#(REMINDER: You can be using quartz() or windows() instead of the default RStudio graphical device if you prefer!)
# a) make a scatterplot of sim.males[,1] vs. sim.males[,2]. Make the background blue and the points circular, filled, and yellow. Make everything else about the plot (axes, labels, etc) white. Make the x label be "male extraversion" and y label be "male self esteem" and give the plot an informative title. Plot a red best fit line through the points and make that line be twice as wide as normal.
quartz()
par(bg="blue", fg="white", col.lab = "white", col.axis = "white")
plot(sim.males[,1], sim.males[,2], pch = 20, col = 'yellow', xlab = "male extraversion", ylab = 'male self-esteem')
abline(lm(sim.males[,1] ~ sim.males[,2]), col = "red", lwd =2)

# b) Sometimes we want to add (or overlay) a plot over an existing plot. To do that, use par(new=TRUE), then plot the next thing. So here, create a histogram of the residuals and place it in the top left quadrant of this existing graph. Make the bars be filled wheat1". NOTE: see the fig= argument in par() too
par(new=T, fig = c(0,.25,.5,1))
hist(abs(sinx - sinx2), col = 'wheat1', axes = FALSE, xlab = NULL, ylab = NULL)
par(op)

# c) Let's say instead of overlaying a histogram, we want to overlay all the points & regression lines for females. use sim.females to do so. Make the female points & lines green and make the female points be diamonds. There should no longer be a histogram in the figure.
quartz()
par(bg="blue", fg="white", col.lab = "white", col.axis = "white")
plot(sim.males[,1], sim.males[,2], pch = 20, col = 'yellow', xlab = "male extraversion", ylab = 'male self-esteem')
abline(lm(sim.males[,1] ~ sim.males[,2]), col = "red", lwd =2)
par(new=T)
plot(sim.females[,1], sim.females[,2], pch=18, col = "green", axes = F, xlab = NULL, ylab = NULL)


# d) There's something wrong with the previous graph: not all the points are shown! only those that fit in the first plot's axis. Here's a potential solution. See if you can follow:
op <- par(bg = 'blue', fg = 'white', col.lab = 'white', col.axis ='white', col.main = 'white')
plot(c(sim.males[ ,1], sim.females[ ,1]), c(sim.males[ ,2], sim.females[ ,2]), pch = 20,
     xlab = "male extraversion", ylab = 'male self-esteem', col = 'yellow', type = 'n',
     main = "Self-esteem ~ Extraversion X Gender")
points(sim.females[ ,1], sim.females[ ,2], pch = 18, col = 'green')
abline(lm(sim.females[ ,1]~ sim.females[ ,2]), col = 'green', lwd = 2)
points(sim.males[ ,1], sim.males[ ,2], pch = 18, col = 'yellow')
abline(lm(sim.males[ ,1]~ sim.males[ ,2]), col = 'yellow', lwd = 2)
# Notice the use of type = 'n'!
par(op)

# e) If we wanted to add a legend to the plot we'd do something like this:
legend(-2,2,legend=c("Male","Female"),pch=c(20,18),col=c('yellow','green'),
text.col='white', bg = 'grey') 
# Say you don't like that legend - you want it (the dots and text) to be 1.5 times as large (50% larger), and you want the background of the legend to be light blue rather than grey. Make it so:
legend(-2,2,legend=c("Male","Female"),pch=c(20,18),col=c('yellow','green'),
       text.col='white', bg = 'blue', cex=1.5) 
par(op)


# 5 Using par('usr') -------------------------------
# Sometimes (e.g., when we're automating a lot of graphics or writing a graphics function), we don't want to look up the x-y coordinates for each graph. Rather, we want to be able to say "put so-and-so in the top left (e.g., at the 10th pct. of x and 110th pct. of y). Here is a very useful function which allows you to figure out the coordinates of your particular plot:
# this gives the extreme x1, x2 then extreme y1, y2 of the current device
par('usr') 
# you can assign this - just a vector of length 4
(my.lims <- par('usr'))

# a) Rerun the plot you above above in 4e, then use par('usr') to make the legend above appear in about the same place it is now - but no hard coding (figure out how to use the par('usr') vector to achieve the same thing)

op <- par(bg = 'blue', fg = 'white', col.lab = 'white', col.axis ='white', col.main = 'white')
plot(c(sim.males[ ,1], sim.females[ ,1]), c(sim.males[ ,2], sim.females[ ,2]), pch = 20,
     xlab = "male extraversion", ylab = 'male self-esteem', col = 'yellow', type = 'n',
     main = "Self-esteem ~ Extraversion X Gender")
points(sim.females[ ,1], sim.females[ ,2], pch = 18, col = 'green')
abline(lm(sim.females[ ,1]~ sim.females[ ,2]), col = 'green', lwd = 2)
points(sim.males[ ,1], sim.males[ ,2], pch = 18, col = 'yellow')
abline(lm(sim.males[ ,1]~ sim.males[ ,2]), col = 'yellow', lwd = 2)

legend(par('usr')[1],par('usr')[2],legend=c("Male","Female"),pch=c(20,18),col=c('yellow','green'),
       text.col='white', bg = 'blue', cex=1.5) 




# 6 Inner Margins -------------------------------
# We can change the margins of a figure to make them bigger or smaller - this is useful when, e.g., you want to add a sublabel or want larger font to appear in the labels or title. Note: mar() is one of several arguments that can only be set by using the par() function, and unlike most arguments, cannot be set from within a plotting function like plot() or hist(). See ?par for details.

#Here is how I did 5a above. Before resetting my graphical parameters using par(), I'll check what the margins are in the plot:
op <- par(bg = 'blue', fg = 'white', col.lab = 'white', col.axis ='white', col.main = 'white')
plot(c(sim.males[ ,1], sim.females[ ,1]), c(sim.males[ ,2], sim.females[ ,2]), pch = 20,
     xlab = "male extraversion", ylab = 'male self-esteem', col = 'yellow', type = 'n',
     main = "Self-esteem ~ Extraversion X Gender")
points(sim.females[ ,1], sim.females[ ,2], pch = 18, col = 'green')
abline(lm(sim.females[ ,1]~ sim.females[ ,2]), col = 'green', lwd = 2)
points(sim.males[ ,1], sim.males[ ,2], pch = 20, col = 'yellow')
abline(lm(sim.males[ ,1]~ sim.males[ ,2]), col = 'yellow', lwd = 2)
# Notice the use of type = 'n' in the first plot()

legend(par('usr')[1], par('usr')[2]*1.1,legend=c("Male","Female"),pch=c(20,18),col=c('yellow','green'), text.col='white', bg = 'lightblue', cex = 1.5)
#Now I check what the margins are:
par("mar")
#or:
par()$mar
#now I reset my graphical parameters back to the default ones from way above:
par(default.par.vals)

# a) use the par(mar=) argument to increase the upper, lower, and lefthand margins for the scatterplot above. Then make the axis labels be 2 times as large as they currently are, and the main title be three times larger and be bolded. If the text is too large for your device, increase the size of your device (see ?quartz or ?windows)
quartz("new",15,10)
op <- par(bg = 'blue', fg = 'white', col.lab = 'white', col.axis ='white', col.main = 'white', mar=(2*par()$mar), cex.lab = 2, cex.main = 3)
plot(c(sim.males[ ,1], sim.females[ ,1]), c(sim.males[ ,2], sim.females[ ,2]), pch = 20,
     xlab = "male extraversion", ylab = 'male self-esteem', col = 'yellow', type = 'n',
     main = "Self-esteem ~ Extraversion X Gender")
points(sim.females[ ,1], sim.females[ ,2], pch = 18, col = 'green')
abline(lm(sim.females[ ,1]~ sim.females[ ,2]), col = 'green', lwd = 2)
points(sim.males[ ,1], sim.males[ ,2], pch = 20, col = 'yellow')
abline(lm(sim.males[ ,1]~ sim.males[ ,2]), col = 'yellow', lwd = 2)
# Notice the use of type = 'n' in the first plot()

legend(par('usr')[1], par('usr')[2]*.8,legend=c("Male","Female"),pch=c(20,18),col=c('yellow','green'),text.col='white', bg = 'lightblue', cex = 2)

par(default.par.vals)



# 7 Outer Margins -------------------------------
# Run the following to create a 2x2 plot of histograms from the last HW:
quartz()
op <- par(mfrow=c(2,2)) 
hist(a <- rgamma(500,shape=1),main="Random Gamma Variables, Shape=1",xlab="value",ylab="proportion obs.", probability=TRUE,col=colors()[405]) 
lines(density(a),lwd=2,col='red')
hist(a <- rgamma(500,shape=5),main="Random Gamma Variables, Shape=5",xlab="value",ylab="proportion obs.",probability=TRUE,col=colors()[405]) 
lines(density(a),lwd=2,col='red')
hist(a <- rgamma(500,shape=50),main="Random Gamma Variables, Shape=50",xlab="value",ylab="proportion obs.",probability=TRUE,col=colors()[405]) 
lines(density(a),lwd=2,col='red')
hist(a <- rnorm(500),main="Random Normal Variables",xlab="value",ylab="proportion obs.",probability=TRUE,col=colors()[405]) 
lines(density(a),lwd=2,col='red')
par(op) 

# a) Now, using the lower level function mtext(), which adds text to the outer margins, add an overall title to the plot above. Title it "Histograms of the Gamma Family". If it doesn't work, you may need to rerun the plot above but adjust the outer margins to be larger using par(oma=) argument
par(oma = c(0,0,6,0), mfrow = c(2,2))
hist(a <- rgamma(500,shape=1),main="Random Gamma Variables, Shape=1",xlab="value",ylab="proportion obs.",probability=TRUE,col=colors()[405]) 
lines(density(a),lwd=2,col='red')
hist(a <- rgamma(500,shape=5),main="Random Gamma Variables, Shape=5",xlab="value",ylab="proportion obs.",probability=TRUE,col=colors()[405]) 
lines(density(a),lwd=2,col='red')
hist(a <- rgamma(500,shape=50),main="Random Gamma Variables, Shape=50",xlab="value",ylab="proportion obs.",probability=TRUE,col=colors()[405]) 
lines(density(a),lwd=2,col='red')
hist(a <- rnorm(500),main="Random Normal Variables",xlab="value",ylab="proportion obs.",probability=TRUE,col=colors()[405]) 
lines(density(a),lwd=2,col='red')
mtext("Simulated Data",cex = 2, outer = T,line=2)

# b) Lets add a sub-title (in smaller font) under the main title of the plot that says: "shape denotes the number of exponential distributions that are summed". See ?mtext and line= arguments if you get stuck. You may need to rerun the above to place the overall title a bit higher
mtext("shape denotes the number of exponential distributions that are summed",line = 0, outer = T, cex = 1)

# c) Let's say you want to do away with the redundant axis (x & y) labels that are within each graph, and want to place those labels in the outer margins instead (e.g., place "proportion obs." on the outer y-axis label, and have no more "proportion obs." within each subplot). Do so. Note: to have, e.g., no x labels, do xlab=NULL
par(oma = c(0,2,4,0), mfrow = c(2,2))
hist(a <- rgamma(500,shape=1),main="Random Gamma Variables, Shape=1",xlab= NULL ,ylab= NULL,probability=TRUE,col=colors()[405]) 
lines(density(a),lwd=2,col='red')
hist(a <- rgamma(500,shape=5),main="Random Gamma Variables, Shape=5",xlab= NULL ,ylab= NULL,probability=TRUE,col=colors()[405]) 
lines(density(a),lwd=2,col='red')
hist(a <- rgamma(500,shape=50),main="Random Gamma Variables, Shape=50",xlab= NULL ,ylab= NULL,probability=TRUE,col=colors()[405]) 
lines(density(a),lwd=2,col='red')
hist(a <- rnorm(500),main="Random Normal Variables",xlab= NULL ,ylab= NULL,probability=TRUE,col=colors()[405]) 
lines(density(a),lwd=2,col='red')
mtext("Histograms of the Gamma Family",cex = 2, outer = T)
mtext("Prop Obs.",cex=1.5,outer=T,side=2)
par(default.ops)
par(op)




# 8 Including symbols in plots -------------------------------
# Here's how to include math and greek symbols in plots - use the expression() function. See ?plotmath for full details:
x <- seq(-5,5,length=200)
y <- sqrt(1+x^2)
plot(y~x, type='l',ylab=expression( sqrt(1+x^2) ))
title(main=expression("Graph of function f"(x) == sqrt(1+x^2)))

# a) Do something similar to above, but plot y = x^3 -2*x^2 + 100*sin(x) and place the expression on the ylab
y2 <- x^3 - -2*x^2 + 100*sin(x)
plot(y2~x, type='l',ylab=expression( x^3 - -2*x^2 + 100*sin(x) ))
title(main=expression("Graph of function f"(x) == x^3 - (-2)*x^2 + 100*sin(x)))
# b) Redo the plot from 7c, but instead of "shape =" in the title, put greek symbol "lambda=", but don't write out "lambda"; rather make it a greek symbol. You will need to use expression() and paste(). Place the subtitle "Also a Normal Dist." *underneath* the main title of the last hist using mtext()
#For example, for the first (upper left) plot:
#hist(a <- rgamma(500,shape=1),main= expression(paste("Graph of ", lambda, " = 1")),xlab= NULL, ylab= NULL,probability=TRUE,col=colors()[405]) 

par(oma = c(0,2,4,0), mfrow = c(2,2))
hist(a <- rgamma(500,shape=1),main=expression(paste("Random Gamma Var of ", lambda, " =1")),xlab= NULL ,ylab= NULL,probability=TRUE,col=colors()[405]) 
lines(density(a),lwd=2,col='red')
hist(a <- rgamma(500,shape=5),main=expression(paste("Random Gamma Var of ", lambda, " =5")),xlab= NULL ,ylab= NULL,probability=TRUE,col=colors()[405]) 
lines(density(a),lwd=2,col='red')
hist(a <- rgamma(500,shape=50),main=expression(paste("Random Gamma Var of ", lambda, " =50")),xlab= NULL ,ylab= NULL,probability=TRUE,col=colors()[405]) 
lines(density(a),lwd=2,col='red')
hist(a <- rnorm(500),main="Random Normal Variables",xlab= NULL ,ylab= NULL,probability=TRUE,col=colors()[405]) 
lines(density(a),lwd=2,col='red')
mtext("Histograms of the Gamma Family",cex = 2, outer = T)
mtext("Also a Normal Dist.",cex=.5)
par(default.ops)
par(op)



# 9 A couple of fun plots to learn from -------------------------------
# a) From Duncan Murdoch
fn <- function(x, y, scale){ 
        dnorm(x,mean=1,sd=scale)*dnorm(y,mean=-1,sd=scale) - dnorm(x,mean=-1,sd=scale)*dnorm(y,mean=1,sd=scale)}
x <- seq(-4,4,len=100)
y <- seq(-4,4,len=100)
z <- outer(x,y,fn,scale=0.5)
persp(x,y,z,col="green",border=NA,shade=0.75)


# b) From Thomas Lumley:
x<-seq(-10,10,length=400)
y1<-dnorm(x)
y2<-dnorm(x,m=3)
par(mar=c(5,4,2,1))
plot(x, y2, xlim=c(-3,8), type="n", xlab=quote(Z==frac(mu[1]-mu[2],
                                                       sigma/sqrt(n))), ylab="Density")
polygon(c(1.96,1.96,x[240:400],10), c(0,dnorm(1.96,m=3),y2[240:400],0),
        col="grey80", lty=0)
lines(x, y2)
lines(x, y1)
polygon(c(-1.96,-1.96,x[161:1],-10), c(0,dnorm(-1.96,m=0), y1[161:1],0),
        col="grey30", lty=0)
polygon(c(1.96, 1.96, x[240:400], 10), c(0,dnorm(1.96,m=0),
                                         y1[240:400],0), col="grey30")
legend(4.2, .4, fill=c("grey80","grey30"),
       legend=expression(P(abs(Z)>1.96, H[1])==0.85,
                         P(abs(Z)>1.96,H[0])==0.05), bty="n")
text(0, .2, quote(H[0]:~~mu[1]==mu[2]))
text(3, .2, quote(H[1]:~~mu[1]==mu[2]+delta))









































