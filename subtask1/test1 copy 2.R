set.seed(1001)
mStudent<-rnorm(100,1.75,0.316)
fStudent<-rnorm(100,1.65,0.316)
totalStu1<-c(mStudent,fStudent)
tStuReSam<-sample(totalStu,200,replace=T)
test1<-dnorm(totalStu1[2],1.7,1)
test2<-dnorm(totalStu1[2],1.6,1)
pb=test1/(test1+test2)
pg=test2/(test1+test2)
### LL function ##
### E step #####
eStep.fn=function(data,flag, muB,sigmaB,muG,sigmaG){
  listB = c()
  listG = c()
  numB=0
  numG=0
  for(i in 1:200){
    test1<-dnorm(data[i],muB,sigmaB)
    test2<-dnorm(data[i],muG,sigmaG)
    pb=test1/(test1+test2)
    pg=test2/(test1+test2)
    if(pb>=pg){
      numB=numB+1
      listB[numB]=data[i]
    }else{
      numG=numG+1
      listG[numG]=data[i]
    }
  }
  if(flag==1){
    return (listB)
  }else
    return (listG)
}
### 
b=eStep.fn(totalStu1,1,1.75,0.3,1.65,0.3)
g=eStep.fn(totalStu1,2,1.75,0.3,1.65,0.3)

LLB <- function(mu, sigma) {
  R = suppressWarnings(dnorm(b, mu, sigma))
  -sum(log(R))
}
LLg <- function(mu, sigma) {
  R = suppressWarnings(dnorm(g, mu, sigma))
  -sum(log(R))
}

b.mle=coef(mle(LLB, start = list(mu = 1.75, sigma=0.31)))
g.mle=coef(mle(LLg, start = list(mu = 1.65, sigma=0.31)))

hist(b)
