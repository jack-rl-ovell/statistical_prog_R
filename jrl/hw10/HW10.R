
# LECTURE/HW #10 - DATA MANIPULATION IN R


#1 Starting up -----------------------
#Change your working directory to wherever you want it to be
setwd("/Users/jacklovell/Desktop/psyc5541/jrl/hw10")

#Change the default options for treating strings as factors in data.frames
options(stringsAsFactors=FALSE)

#Read in a script I wrote that contains handy functions like info(x), LS(), and look(x). source() simply reads in another R script and runs every line of code
source("http://www.matthewckeller.com/R.Class/KellerScript2.R") 

#Run all the following (highlight all and run) - this creates our datasets
set.seed(1234)
RNGkind(sample.kind = "Rounding")

#make SNP key data
SNP.data <- data.frame(SNP.id=paste("rs",sample(100000:999999,1000,FALSE),sep=""),
                       MAF=round(runif(1000,.01,.5),3),
                       chrom=sample(c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9,10,10,
                                      11,11,12,12,13,13,14,14,15,15,16,17,18,19,20,21,22),1000,TRUE),
                       location=round(runif(1000,0,3.2e10/22),0))

#make subject dataset 1
famid <- rep(1:20,each=5)             #family ids
reltype <- rep(1:5,times=20)          #reltype; 1 & 2 are parents, 3-5 are children
persid <- sample(1000:9999,100,FALSE) #person ids
id <- paste("TW2007.",sample(LETTERS,100,TRUE),".",famid,".",persid,sep="") # id numbers
dv <- round(rnorm(100,70,10),1)
dv[runif(100,0,1)<.08] <- NA   # about 8% of DVs missing

snp.indx <- sample(1:1000,100)
mafs.in.data <- SNP.data$MAF[snp.indx]
SNPs.in.data <- SNP.data$SNP.id[snp.indx]

subject.data <- data.frame(SUBID=id,FAMID=famid,RelType=reltype,DV=dv)
subject.data.subset <- subject.data[runif(100,0,1)<.9,]  # remove about 10% of data
snp.subject.data <- data.frame(ID=persid)

for (i in 1:length(mafs.in.data)){
  vec <- rbinom(100,1,mafs.in.data[i])
  snp.subject.data <- cbind(snp.subject.data,vec)
}

names(snp.subject.data)[2:ncol(snp.subject.data)] <- SNPs.in.data
snp.subject.data <- snp.subject.data[order(rnorm(100)),] #random order to snp.subject.data


# look at what data we've created:
LS()
# the 4 objects of importance are:
# 1) subject.data: data on each subject in long format
head(subject.data,10)
# 2) subject.data.subset: a subset of this data (92 rows rather than 100)
head(subject.data.subset,10)  # notice there is no subject 7;
# 3) snp.subject.data: data on each subject with 100 additional variables (snp data: 0 = homozygous allele A, 1 = het, 2 = homz. B). NOTE: order of subjects not the same as subject.data
head(snp.subject.data)
# 4) SNP.data: data on the statistics/information of each of 1000 different SNPs - 100 of which are in snp.subject.data
head(SNP.data,10)
# I've tried to simulate data in formats we might typically need to manipulate and use. Now let's have fun!





#2 SUBSETTING DATA -----------------------
# a) Create a subset of subject.data that only includes family members who have RelType of 3 or 4. Call it subject.data.subset2. It should have 40 rows
reltype34 <- (subject.data$RelType==3) | (subject.data$RelType==4)
subject.data.subset2 <- subject.data[reltype34,]
nrow(subject.data.subset2)
# b) Create a subset of subject.data that only includes individuals with non-missing data on subject.data$DV. Call it subject.data.subset3. It should have 90 rows.
not.na <- !is.na(subject.data$DV)
subject.data.subset3 <- subject.data[not.na,]
nrow(subject.data.subset3)

#3 USING MATCH FUNCTION -----------------------
# "match" matches two vectors - it finds the index for the elements 1st vector that are in the 2nd. I.e.,
match(1:10,c(1,5,30))
# the above says: "what elements of 1:10 are in c(1,5,30)?" The answer is: 1 is in the 1st element, not applicable for elements 2-4, 5 is in the 2nd elements, not app for 6-10

#We can do the opposite, asking what elements of the second vector are in the first
match(c(1,5,30),1:10)

#Can you understand how this works?:
match(1:10,100:1)

# it works for text vectors too:
(my.letters <- LETTERS[1:10])
(other.letters <- LETTERS[8:26])
match(my.letters,other.letters)
# match returns a vector the same length of the first vector. The elements are either NA if not found in the 2nd vector OR is the index of where the element was found in the 2nd vector

#Run this for the next questions
my.letters2 <- c("A","J","B","L")

# a) what will happen when you type match(my.letters2,other.letters)? WRITE YOUR RESPONSE, DO NOT USE R:

#FTFT

# b) now use R to check that what you wrote above is correct
match(my.letters2,other.letters)
# c) what will happen when you type match(other.letters,my.letters2)? WRITE YOUR RESPONSE, DO NOT USE R:
#NA2NA4
# d) now use R to check that what you wrote above is correct
match(other.letters,my.letters2)





#4 USING THE MATCH FUNCTION & %in% FOR SUBSETTING DATA -----------------------
# a) Create a subset of snp.subject.data that only includes SNPs (the columns) that have a maf<.10 in SNP.data. Call this new dataset snp.subject.sub1. This one is quite challenging! Hint: I first created a vector of the names of all the snps that have maf less than 10%. Then I used "match" to create a vector of the indexes of which columns of snp.subject.data were in this group of SNPs with maf<.1. I then used that vector of indexes (after removing the NAs in it) to subset snp.subject.data.  Make sure to figure out a way to retain the ID in snp.subject.sub1.

maf.less.10 <- SNP.data[SNP.data$MAF < .1, "SNP.id"]
ind <- na.omit(match(maf.less.10, names(snp.subject.data)))
snp.subject.sub1 <- snp.subject.data[,ind]
snp.subject.sub1$ID <- row.names(snp.subject.sub1)

# There is probably a better way to do the above. x %in% y returns a TRUE for the ith element if x[i] is in y; FALSE otherwise. E.g.:
1:10 %in% c(1,5,30)

# b) Let's say we now want a vector (length=21) of the actual MAF's of the SNPs in snp.subject.sub1. Call this vector maf.sub1. I recommend using %in%

index <- SNP.data$SNP.id %in% names(snp.subject.sub1) 
maf.sub1 <- SNP.data$MAF[index]
length(maf.sub1)

#5 TAKING SUBSETS OF NUMBERS & STRINGS -----------------------
# Often, we want to take the last 3 digits in a number or text string. E.g. a is a nine-digit number:
a <- 123456789
# Say we want to take only the last 3 digits. We do that like this:
a %% 1e3
# Say we want to take all EXCEPT the last 3 digits. We do that like this:
a %/% 1e3
# This works for vectors too:
c(123456,4567) %%1e2
# Unfortunately, it does NOT work for text strings:
c("ABCDEF","PQRSTUVWXYZ") %%1e4
# To do something similar for text, use substr:
substr("ABCDEF",start=4,stop=6)
substr(c("ABCDEF","PQRSTUVWXYZ"),start=4,stop=6)
# if we really want to mimic %% (take the last x digits), you can do it using "nchar"
a <- "ABCDEF"
substr(a,start=nchar(a)-3,stop=nchar(a))
# Finally, we can split a string according to some character:
a <- "AX.109.y39808"
strsplit(a,".",fixed=TRUE)  # I typically use "fixed" here - otherwise you have to use regular expressions

# a) The SUBID we have in subject.data doesn't conform to the ID we have in snp.subject.data. We want to extract the last 4 numbers in SUBID, and create a new variable "ID" in subject.data that is this new id. I recommend using substr, but it is possible to accomplish this using strsplit. Make sure "ID" is a numeric variable

subject.data$ID <- as.numeric(substr(subject.data$SUBID, start = nchar(subject.data$SUBID)-3, stop = nchar(subject.data$SUBID)))



#6 MERGING DATA -----------------------
# We now have a variable "ID" in both subject.data and in snp.subject.data & snp.subject.sub1.
# a) merge subject.data and snp.subject.sub1 using the ID variable. See ?merge if unsure how to do it. Call the new dataset subject.data.merged
subject.data.merged <- merge(subject.data,snp.subject.sub1,by="ID",all=TRUE)

# b) Now do the same thing except merge subject.data.subset with snp.subject.sub1. Make sure to include all subjects in subject.data.subset, but only those subjects in snp.subject.sub1 that are also in subject.data.subset. I.e., subject.data.subset is the 'keyed' data in SPSS lingo. Call this new dataset subject.data.merged2

subject.data.subset$ID <- as.numeric(substr(subject.data.subset$SUBID,nchar(subject.data.subset$SUBID)-3,nchar(subject.data.subset$SUBID)))
subject.data.merged2 <- merge(subject.data.subset,snp.subject.sub1,by="ID",all.x=TRUE,all.y=FALSE)

# Note that merge is quite flexible. E.g., you can also merge based on multiple variables. Note also that we could have used "match" to merge data, which is more flexible yet, but also more cumbersome.





#7 SORTING DATA -----------------------
# to sort a vector, use the function sort(). This simply puts the elements of a vector in order of lowest to highest. 
vec <- c(1,3,5,3,1,3,3,9,-50)
sort(vec)

# Sorting a matrix. The sorted values are stored in a vector 
(a <- matrix(c(3,56,2,3,9,1,50,5,32,16,8,100),nrow=4))
(a.sort <- sort(a))
info(a.sort)

# order() does not sort the elements. Rather, it returns a vector with the ordered indices of the original (unsorted) vector
order(vec)

# of course, you can use these indices to accomplish the same thing as sort():
vec[order(vec)]

# If we're trying to sort matrices or data.frames according to the order of a particular column (e.g., the first column), we do NOT want to do something like this:
a
sort(a)

# Nor something like this:
a
cbind(sort(a[,1]),a[,-1]) #just sorts the first column and other columns do not retain the order of the first columns

#Instead, to actually reorder sorting data frames/matrices in R, we use the order() function to get the ordered indices we want to rearrange by. This is nothing special vis a vis normal R usage. Just create an index that has the order of whatever variables you want to use, then use that index to reorder the rows. E.g., let's sort by the first column, and if there are ties there, by the 2nd column
(ind <- order(a[,1],a[,2]))
# now all we need to do is order the rows according to ind:
(a.sorted <- a[ind,])
a

# a) sort subject.data by subject.data$DV. Creat a new object called subject.data.sortdv
subject.data.sortdv <- subject.data[order(subject.data$DV),]

# b) do the same as above, except make the NA's come first, and call this subjerct.data.sortdv2
subject.data.sortdv2 <- subject.data[order(subject.data$DV,na.last=FALSE),]

# c) now re-sort subject.data by RelType; break ties using FAMID Call this subject.data.sortreltype
subject.data.sortreltype <- subject.data[order(subject.data$RelType,subject.data$FAMID,na.last=FALSE),]





#8 RESHAPING DATA -----------------------
# It is often useful to move data from "long" format to "wide" format or vice versa. To reshape to wide, here is the typical format:
# reshape(long.data,idvar="wide.id",direction="wide",timevar="long.id")
# idvar is the id for the whole row
# direction is either "wide" or "long"
# timevar is the way we distinguish records in the wide format.

# a)  Use the "reshape" function to reshape subject.data such that each row is a family rather than an individual. Call this data "subject.data.wide"
subject.data.wide <- reshape(subject.data, idvar = "FAMID", direction = "wide", timevar = "RelType")

# b) recall that subject.data.subset has some rows missing. Do the same as above, but create a reshaped subject.data.subset that is called subject.data.sub.wide
subject.data.sub.wide <- reshape(subject.data.subset,idvar="FAMID",direction="wide",timevar="RelType")

# Here is how you go from wide to long. The typical format is:
# reshape(wide.data,direction="long",varying=list(2:3,4:5),timevar=NULL)
# varying must be a list; place here the grouped column indexes that corresponds to what become single variables in long format. Timevar is not really necessary, so I place "NULL" here

#Here's an example dataset in wide format:
wide.dat <- data.frame(famid=1:11,pers1=50:60,pers2=70:80,dv1=rnorm(11),dv2=rnorm(11))

# c) Now reshape wide.dat into long format:
reshape(wide.dat, direction="long", varying=list(2:3,4:5))

# After an object has been reshaped, it is simple to revert it back to its old format. E.g.,
subject.data.long <- reshape(subject.data.wide,direction="long")





# 9 USING DUPLICATED & UNIQUE & WHICH -----------------------
# It's easier to show than to describe:
(dup.vec <- c(1,2,3,4,1,2,7,8,9,3))
duplicated(dup.vec)
unique(dup.vec)

#Sometimes it is useful to know the actual index that corresponds to TRUE in some logical vector rather than the TRUE/FALSE's themselves. e.g:
c(1,2,3,4,5)==3
which(c(1,2,3,4,5)==3)
dup.vec > 5
which(dup.vec > 5)

# Run the following code:
set.seed(12345)
a2 <- subject.data.merged[sample(1:nrow(subject.data.merged),500,TRUE),]
head(a2)

# a) Check out a2. There are lots of duplicated subjects. Create a new dataset, a3, that has no duplicated ID numbers
a3 <- a2[duplicated(a2$ID)==FALSE,]

# b) Use the unique() function to figure out how many different families are in subject.data. Call this "num.fams"
num.fams <- length(unique(subject.data$FAMID))


# c) Create a vector that indexes the rows of subject.data in which the DV is less than 70
ind.vec <- subject.data$DV < 70

# d) Use the index you just created in (c) above to create an object "subject.data.less70" that is subject.data where DV < 70

subject.data.less70 <- subject.data[ind.vec,]

# e) Create a vector that indexes the rows of SNP.data in which MAF is between .3 and .4, then use this index to create an object, "SNP.data.bw.3.4" that has only SNPs with .3 < MAF < .4

ind3 <- which(SNP.data$MAF>.3 & SNP.data$MAF<.4)
SNP.data.bw.1.4 <- SNP.data[ind3,]




# 10 ADVANCED DATA PROCESSING -----------------------
# a) Let's say we can only have four people in each family - 2 parents (RelType 1 or 2) and 2 children (RelType 3, 4, or 5). Create a subset of subject.data that includes two children (such that you drop a third child at random). Write code that accomplishes this, and would accomplish the same no matter which dataset you have (i.e., do NOT "hard code" the data - ie., don't come up with rows can be dropped in this particular dataset). Call this new dataset "subjects". Make sure it is ordered by FAMID 1st and by RelType 2nd. This one is tough, but there are a lot of ways to accomplish it. PS - if you really want to challenge yourself, figure out how to selectively drop children with missing data - i.e., never keep a child with NA's on the DV and throw one out with valid data.

keep <- c()
for (i in 1:num.fams){
  keep <- c(keep,sample(1:3,2,FALSE))}

adder <- seq(2,nrow(subject.data),by=5)
adder <- rep(adder,each=2)

keep2 <- keep+adder
kids <- subject.data[keep2,]
parents <- subject.data[subject.data$RelType==1 | subject.data$RelType==2,]
subjects <- rbind(parents,kids)
subjects <- subjects[order(subjects$FAMID,subjects$RelType),]




# b) Create a new variable "cumulative.distance" in SNP.data that represents the cumulative genomic distance for each SNP across chromosomes. Realize that SNPs on latter chromosome should 'cumulate' the distances from SNPs on previous chromosomes. So, e.g. if the distances are like this:
#snp.id  chrom  loc
#SNP1     1      100
#SNP2     1      150
#SNP3     2      30
#SNP4     2      80
# then the cumulative distance would look like this:
#snp.id  chrom  loc   cum loc
#SNP1     1      100   100
#SNP2     1      150   150
#SNP3     2      30    180    # (i.e., this is 150+30; 150 being the last location of the last SNP of chromosome 1)
#SNP4     2      80    230    # (this is 150+80)
#SNP5     2      160   310
#SNP6     3      30    340

# Hint1: Here's how I would think about this problem:
#snp.id  chrom  loc   cum loc   adder
#SNP1     1      100   100        0
#SNP2     1      150   150        0
#SNP3     2      30    180        150
#SNP4     2      80    230        150
#SNP5     2      160   310        150       # (310=150+160)
#SNP6     3      30    340        310       # (340=310+30)

# Hint 2: there is a function called 'cumsum' that you might find handy
# Note: This is a very very tough one!! Don't spend more than 1 hour trying to get it. Give it a try and come to class. 

SNP.data2 <- SNP.data[order(SNP.data$chrom,SNP.data$location),]
#SNP.data2 <- look(SNP.data2,2,50)
# this gets the index of the last SNP for each chromosome
dups <- duplicated(SNP.data2$chrom)*1
dups[1] <- 1
indx <- which(dups==0)

# this gets the 'adder' column from above
ddd <- rep(0,nrow(SNP.data2))
ddd[indx] <- SNP.data2$location[indx-1]
ddd2 <- cumsum(ddd)

# Now we just need to add cumsum to "location"
SNP.data2$cum.location <- ddd2+SNP.data2$location


# 11 ADVANCED DATA PROCESSING 2 -----------------------
# Run the following code. This will create a new "subject.data" but it will be messier ("messy.data"):
set.seed(4444)
messy.data <- data.frame(subject.data[sample(1:nrow(subject.data),120,replace=TRUE),],row.names=1:120)
messy.data[duplicated(messy.data$SUBID),"DV"] <- round(rnorm(sum(duplicated(messy.data$SUBID)),80,10),1)
messy.data <- messy.data[order(rnorm(120)),]
# why messy? There are some subjects who are in there 2+ times and some subjects who have been dropped altogether.

# a) How many times do duplicated rows exist in your data? Recall that sum() on a logical vector gives you the number of TRUE's
sum(duplicated(messy.data$SUBID))

# b) How many subjects have duplicate ID numbers? NOTE: this isn't the same question as above, and is trickier than it sounds. I want the number of individual subjects who have duplicated data, not the number of rows that have duplicated data. This isn't the same because 1 subject can be in here more than twice. Please assume that, as would be the case in the real world, you had no idea that there were originally 100 subjects.
dups <- duplicated(messy.data$SUBID)
length(unique(messy.data$SUBID[dups]))

# c) Of the subjects in messy.data, how many times are they represented (from 1 and up)? Create a vector that has how many times each subject is in the dataset. HINT: try summary(as.factor(x)) where x is the subject id
summary(as.factor(messy.data$SUBID))
sum(messy.data$SUBID >= 1)

# d) One question we might be interested in knowing: Is there a mean difference in the DV between subjects whose data is duplicated versus those whose data is in there exactly once? For the purposes of this class, the approach we're going to take is this: First, find the MEAN DV value for each person who has duplicated data. Call this new DV "meanDV".Make sure that if a person has missing data in one row and not the other that you remove the missing data when figuring out the mean. Second, create a new dataset, "messy.data2" that has a new variable stating whether that particular subject had missing data or not. Another new variable will be the new DV "meanDV". Third, for those subjects who have multiple rows of data, *randomly* remove duplicated rows, such that each subject will now have exactly one row. Fourth, compare the "meanDV" between those who have duplicated data vs. those who do not using a t-test. NOTE: This uses a lot of what you learned above, and is very difficult!  But oh so satisfying once you have it. Hint: try to follow the steps below to help you:

# First, create a new variable that says whether each subject's data is duplicated. I did this using duplicated() first to identify the duplicated SUBIDs, followed by %in% to see which SUBIDS are in that vector of duplicated SUBID's:
mid <- messy.data$SUBID[duplicated(messy.data$SUBID)]
messy.data$duplicated <- messy.data$SUBID %in% mid

# Second, find the mean DV for those who have duplicated data, and then put that DV into messy.data and call that object messy.data2. I did this by creating the meanDV using tapply(), then creating a new data.frame that had data.frame(SUBID=names(meanDV),DV2=meanDV), and then used merge() to merge messy.data with this newly created data.frame to create messy.data2, and then removed duplicated IDs in messy.data2:
meanDV <- tapply(messy.data$DV,messy.data$SUBID,mean,na.rm=TRUE)
meanDV <- data.frame(SUBID=names(meanDV),DV2=meanDV)
messy.data2 <- merge(messy.data,meanDV,by="SUBID")
messy.data2 <- messy.data2[order(rnorm(nrow(messy.data2))),]  # randomly ordering messy.data2, to make duplicated remove random rows
messy.data2 <- messy.data2[duplicated(messy.data2$SUBID)==FALSE,]

# Finally, run the t-test
t.test(data=messy.data2$duplicated,messy.data2$DV2)
