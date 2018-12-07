
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

par(mfrow=c(1,2))
hist(sample(x,5,replace=FALSE))
hist(sample(x,50,replace=FALSE))

x1<-sample(x,50,replace=FALSE)
mean(x1<25 & x1>23)

x2<-rnorm(50,23.9,0.43)
mean(x2<25 & x2>23)

pnorm( (25-23.9) / 0.43)  - pnorm( (23-23.9) / 0.43)

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- read.csv(filename) 
dat <- na.omit( dat )
dat

library(dplyr)
x<-filter(dat,Sex=="M" & Diet=="chow")
x

mean(x$Bodyweight)

library(rafalib)
popsd(x$Bodyweight)

set.seed(1)
X<-sample(x$Bodyweight,25)
mean(X)


library(dplyr)
y<-filter(dat,Sex=="M" & Diet=="hf") %>% select("Bodyweight")


mean(y$Bodyweight)
popsd(y$Bodyweight)

set.seed(1)
Y<-sample(y$Bodyweight,25)
mean(Y)

abs((mean(y$Bodyweight)-mean(x$Bodyweight))-(mean(Y)-mean(X)))



#for FEMALE#

library(dplyr)
x<-filter(dat,Sex=="F" & Diet=="chow")
x

mean(x$Bodyweight)

library(rafalib)
popsd(x$Bodyweight)

set.seed(1)
X<-sample(x$Bodyweight,25)
mean(X)


library(dplyr)
y<-filter(dat,Sex=="F" & Diet=="hf") %>% select("Bodyweight")


mean(y$Bodyweight)
popsd(y$Bodyweight)

set.seed(1)
Y<-sample(y$Bodyweight,25)
mean(Y)

abs((mean(y$Bodyweight)-mean(x$Bodyweight))-(mean(Y)-mean(X)))






#######CLT######


library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- na.omit( read.csv(filename) )



pnorm(3)-pnorm(-3)

library(dplyr)
library(rafalib)
y<-filter(dat,Diet=="chow") %>% select("Bodyweight")
sd1<-popsd(y$Bodyweight)
mean(y$Bodyweight)

y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
mean( abs(z) <=1 )

mean(abs(z) <=2)
mean(abs(z) <=3)

qqnorm(z)
abline(0,1)


mypar(2,2)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)








y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)

mean(avgs)
popsd(avgs)


### CLT in Practice####


library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if(!file.exists("femaleMiceWeights.csv")) download(url,destfile=filename)
dat <- read.csv(filename)

x=sample(1:6, 100, replace=TRUE)
mean(x==6)

set.seed(1)
set<- replicate(10000,f(100))
f<-function(n){
  x=sample(1:6,n, replace=TRUE)
  p=1/6
  z=(mean(x==6)-p)/sqrt(p*(1-p)/n)
}

mean(abs(set) > 2)

pnorm(-2)+(1-pnorm(2))



set.seed(1)
n <- 100
sides <- 6
p <- 1/sides
zs <- replicate(10000,{
  x <- sample(1:sides,n,replace=TRUE)
  (mean(x==6) - p) / sqrt(p*(1-p)/n)
}) 
qqnorm(zs)
abline(0,1)#confirm it's well approximated with normal distribution
mean(abs(zs) > 2)



ps <- c(0.5,0.5,0.01,0.01)
ns <- c(5,30,30,100)
library(rafalib)
mypar(4,2)
for(i in 1:4){
  p <- ps[i]
  sides <- 1/p
  n <- ns[i]
  zs <- replicate(10000,{
    x <- sample(1:sides,n,replace=TRUE)
    (mean(x==1) - p) / sqrt(p*(1-p)/n)
  }) 
  hist(zs,nclass=7)
  qqnorm(zs)
  abline(0,1)
}


X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist

mean(X)
set.seed(1)
X1<-sample(X,12,replace = F)
mean(X1)
var(X)
sd(X)
sum(X1)/11

