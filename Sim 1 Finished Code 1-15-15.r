

# We must initialize and assign values to several variables for simulation.
# var is the population variance, from which the random obs are drawn.
# treatEffect is the size of the effect we want to test by adding
# 	it to the obs in a treatment condition.
# populationP is the proportion of that treatment condition that has 
# 	the effect added to it; we start at 0% b/c we must test all combos. 
 
var<-c(1)
treatEffect<-c(3)
populationP<-c(0.0)

# Now, we must initialize several data structures to hold the end results.
# Each structure holds data for a specific trtmt condition + statistic.
# SDT = standard deviation of a treatment condition.
# SDD = standard deviation of the difference between a trtmt obs and matching control obs.
# ES = effect size estimate for a trtmt condition.
# PT = p value of a trtmt condition.
# 1 = trtmt cond. where we ADD treatEffect
# 2 = trtmt. cond. where we SUBTRACT treatEffect
# C = control condition

SDT1<-matrix(, nrow= 51, ncol = 1)
SDT2<-matrix(, nrow= 51, ncol = 1)
SDC<-matrix(, nrow= 51, ncol = 1)
SDD1<-matrix(, nrow= 51, ncol = 1)
SDD2<-matrix(, nrow= 51, ncol = 1)
ES1<-matrix(, nrow= 51, ncol = 1)
ES2<-matrix(, nrow= 51, ncol = 1)
PT1<-matrix(, nrow= 51, ncol = 1)
PT2<-matrix(, nrow= 51, ncol = 1)

# Now, we must begin a loop that will test all our levels of populationP.
# the length = 51 b/c:
# our interval of msrmt is 2% of trtmt cond. pervasiveness
# we start @ 0.0...so there are 51 levels of this variable

for(p in 1:51){

# Create  temp data structures (DS) to hold results for each level of populationP
# the size = 100 b/c:
# we perform 100 trials of each experiment, and take avgs of stats 
# for more stable measurements (replication)
# the same values for DS above are used for these, with a D added to name

SDT1D<-matrix(,nrow = 100, ncol = 1)
SDT2D<-matrix(,nrow = 100, ncol = 1)
SDCD<-matrix(,nrow = 100, ncol = 1)
SDD1D<-matrix(,nrow = 100, ncol = 1)
SDD2D<-matrix(,nrow = 100, ncol = 1)
ES1D<-matrix(,nrow = 100, ncol = 1)
ES2D<-matrix(,nrow = 100, ncol = 1)
PT1D<-matrix(,nrow = 100, ncol = 1)
PT2D<-matrix(,nrow = 100, ncol = 1)

# Begin loop that performs 100 trials of a trtmt combo 

for(z in 1:100){

# Now, we create a master DS to hold the individual obs. data we generate.
# We fill it wil the generic value "99" b/c when we review the data, we
# 	can easily see if a value wasn't generated into a specfifc cell.
# each row is a single "subject"; there are 50 in our matrix.
# each column is an experimental condition; they are outlined
# in the colnames() function below

dataStructure<-matrix( 99, nrow=50, ncol=6)
colnames(dataStructure)<-c('distMeans','Control','Treatment[+]','Treatment[-]','Treatment[+]-Control','Treatment[-]-Control')

# Next, we generate individual means for the distributions of 
# our "subjects"; the rows in the matrix. the function rnorm()
# 	generates 50 obs. from the standard normal dist. mean = 0, var = 1.

distMeans<-rnorm(50)

# Now, we put those obs. into the first column of dataStructure

dataStructure[1:50,1]<-distMeans[1:50]

# Next, we want to generate the rest of our experimental data. 
# For the number of rows in dataStructure, generate a random obs.
# from a normal dist. with mean = distMeans[row #], var = var.
# Do this for control (col 2), trt1 (col 3), and trt2 (col 4) conditions!
# Next, calculate difference of matching trt1 (col4), trt2 (col 5) obs.
#    and control obs.

for(a in 1:50){	
dataStructure[a,2]<-rnorm(1,dataStructure[a,1], var )
dataStructure[a,3]<-rnorm(1,dataStructure[a,1], var )
dataStructure[a,4]<-rnorm(1,dataStructure[a,1], var )
dataStructure[a,5]<-dataStructure[a,3]-dataStructure[a,2] 
dataStructure[a,6]<-dataStructure[a,4]-dataStructure[a,2] 
}

# This next step figures out which obs. in trtmt conditions to modify.
# Given populationP, it will generate randomly index numbers for use
# 	in an effect modification step later on.

d<-rbinom(1,50, populationP) 
d2<-rbinom(1,50,populationP) 
rndmIndex1 <- sample.int(50, d, replace = FALSE)
rndmIndex2 <- sample.int(50, d2, replace = FALSE)

# This next step is the treatEffect modificatin step. With R, you can
# just give functions full DS and it will perform the function linearly
# through the length of the DS, as shown below. We just cycle through the DS
# and add/subtract treatEffect, recalculating the difference values accordingly.

dataStructure[rndmIndex1,3] <- '+'(dataStructure[rndmIndex1,3], treatEffect)
dataStructure[rndmIndex1,5]<-dataStructure[rndmIndex1,3] - dataStructure[rndmIndex1,2]
dataStructure[rndmIndex2,4] <- '-'(dataStructure[rndmIndex2,4], treatEffect)
dataStructure[rndmIndex2,6]<-dataStructure[rndmIndex2,4] - dataStructure[rndmIndex2,2]

# *** We now have observations filled in and data modified ***

# Next, we will calculate our stats:
# 1. the standard deviations of trt1, control, and trt2
# 2. the standard deviation of trt1-control, and trt2 - control

sdTreat1<-sd(dataStructure[,3])
sdCont<-sd(dataStructure[,2])
sdTreat2<-sd(dataStructure[,4])
sdDiff1<-sd(dataStructure[,5])
sdDiff2<-sd(dataStructure[,6])

# 3. the effect sizes of trt1 and trt2 

effectSize1<-((mean(dataStructure[,3])-mean(dataStructure[,2]))/sd(dataStructure[,2]))
effectSize2<-((mean(dataStructure[,4])-mean(dataStructure[,2]))/sd(dataStructure[,2]))

# 4. p - value of paired t test for trt1 + control, trt2 + control
# 	We have to pool data, do t test, the extract p val.

tData1<-c(dataStructure[,3],dataStructure[,2])
tData2<-c(dataStructure[,4],dataStructure[,2])
group <- c(seq(from=0,to=0,length.out=50),seq(from=1,to=1,length.out=50))
test1<-t.test( tData1 ~ group, paired=TRUE)
test2<-t.test( tData2 ~ group, paired=TRUE)
ptest1<-test1$p.value
ptest2<-test2$p.value

# Now put data into temp DS
# Matches up corresponding row in DS with stat

SDT1D[z,1]<-sdTreat1
SDT2D[z,1]<-sdTreat2
SDCD[z,1]<-sdCont
SDD1D[z,1]<-sdDiff1
SDD2D[z,1]<-sdDiff2
ES1D[z,1]<-effectSize1
ES2D[z,1]<-effectSize2
PT1D[z,1]<-ptest1
PT2D[z,1]<-ptest2

# End of trial loop for data generation/analysis

}

# Next, we must translate p vals into boolean values:
# 1 if p val < 0.05, 0 otherwise
# we do this to record the proportion of p vals <0.05, a measurement
# 	we graph in the paper.

pdata<-0
p2data<-0
for(i in 1:100){
if(PT1D[i,1]<0.05){
pdata<-'+'(pdata,1) }
if(PT2D[i,1]<0.05){
p2data<-'+'(p2data,1) }
}

# Now, we store the avg. statistic ( or proportion of p vals <0.05)
# for the 100 trials into our master DS! each cell, again, represents
# 	the avg value of a statistic for the given level of 
# 	treatEffect, var, and populationP. NOTE that each row represents
# 	a different level of populationP. 

SDT1[p,1]<-mean(SDT1D)
SDT2[p,1]<-mean(SDT2D)
SDC[p,1]<-mean(SDCD)
SDD1[p,1]<-mean(SDD1D)
SDD2[p,1]<-mean(SDD2D)
ES1[p,1]<-mean(ES1D)
ES2[p,1]<-mean(ES2D)
PT1[p,1]<-pdata/100
PT2[p,1]<-pdata/100

if(p<50){populationP<-'+'(populationP, 0.02)}

# End of simulation!

}

# Generates some data series for graphing purposes, which are self-explanatory

trueTreatEffectYData<-seq(0,treatEffect,(treatEffect*0.02))
truePropXData<-seq(0,1,0.02)

