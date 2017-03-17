treatEffect<-3


for(){

for(q in 1:100){

treatData <- matrix(rnorm(400), nrow = 20, ncol = 20)
contData <- matrix(rnorm(400), nrow = 20, ncol = 20)
# we have filled in our 2 20x20 arrays with N(0,1) obs.
for(b in 1:20){
  treatData[b,b] <-'+'(treatData[b,b],treatEffect) }
# added treatment effect to diagonal
tSubjMeans<-seq(0,0,length.out=20)
tItemMeans<-seq(0,0,length.out=20)
cSubjMeans<-seq(0,0,length.out=20)
cItemMeans<-seq(0,0,length.out=20)
for(w in 1:20){
tSubjMeans[w] <- mean(treatData[w,(1:20)])
cSubjMeans[w] <- mean(contData[w,(1:20)])
tItemMeans[w] <- mean(treatData[(1:20),w])
cItemMeans[w] <- mean(contData[(1:20),w]) }
subjDiff<-seq(0,0,length.out=20)
itemDiff<-seq(0,0,length.out=20)
for(s in 1:20){
subjDiff[s]<-tSubjMeans[s]-cSubjMeans[s]
itemDiff[s]<-tItemMeans[s]-cItemMeans[s] }
SubjDiffSD<-sd(subjDiff)
ItemDiffSD<-sd(itemDiff)
Treat<-c(seq(0,0,length.out=20),seq(1,1,length.out=20))
# VVV calculates subject ANOVA VVV
subjData<-c(cSubjMeans,tSubjMeans)
subjLM<-lm(subjData ~ Treat)
subjAnova<-aov(subjLM)
subjAnovaPVal<-summary(subjAnova)[[1]][["Pr(>F)"]][[1]] 
# VVV calculates item ANOVA VVV
itemData<-c(cItemMeans,tItemMeans)
itemLM <-lm(itemData ~ Treat)
itemAnova<-aov(itemLM)
subjAnovaPVal<-summary(itemAnova)[[1]][["Pr(>F)"]][[1]] 

******code for effect sizes... PLUG IN  subject std error term******************

SubjEffectSize <- (mean(tSubjMeans)-mean(cSubjMeans)/.2289 )
ItemEffectSize <- (mean(tItemMeans)-mean(cItemMeans)/.2289 )
ItemEffectSize

SubjEffectSize


# VVV begins code for quasi-F and min-F calculations VVV

# VVV generates coding for treatment

tTreat<- matrix(1, nrow = 20, ncol = 20)
cTreat<- matrix(0, nrow = 20, ncol = 20)

# VVV generate coding for subject factor

tSubj<- matrix(0, nrow = 20, ncol = 20)
cSubj<- matrix(0, nrow = 20, ncol = 20)

for(a in 1:20){
tSubj[a,]<-a
cSubj[a,]<-a }

# VVV generate coding for item factor

tWord<- matrix(0, nrow = 20, ncol = 20)
cWord<- matrix(0, nrow = 20, ncol = 20)

for(b in 1:20){
for(d in 1:20){
tWord[b,d]<-d
cWord[b,d]<-d } }

Data<-c(cData,tData)
trt<-c(cTreat,tTreat)
sub<-c(cSubj,tSubj)
wrd<-c(cWord,tWord)
Treat<- factor(trt)
Subj<- factor(sub)
Word <- factor(wrd)

aov(Data ~ Treat + Treat:Subj + Treat:Word + Subj:Treat:Word)

#Mean Squares for Quasi F can be calculated


********CALCULATING OF F CRIT VALUE USING FORMULAs***************************

quasiF<-(MSt + MStsw)/(MSts + MStw)
minF<-MSt/(MSts + MStw) 



i<-(MSt + MStsw)^2/(MSt^2 + (MStsw^2/722))

j<-(MSts + MStw)^2/(MSts^2/38 + MStw^2/38)

Fcrit <-qf(.95, df1 = i , df2 = j)
Fcrit

quasiF

minF

i

j

}

}