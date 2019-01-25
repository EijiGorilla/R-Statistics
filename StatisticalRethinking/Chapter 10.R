library(rethinking)
data("chimpanzees")
d=chimpanzees
head(d)

m10.1=map(
  alist(
    pulled_left~dbinom(1,p),
    logit(p)<-a,
    a~dnorm(0,10)
  ),data=d)

m10.2=map(
  alist(
    pulled_left~dbinom(1,p),
    logit(p)<-a+bp*prosoc_left,
    a~dnorm(0,10),
    bp~dnorm(0,10)
  ),data=d)

m10.3=map(
  alist(
    pulled_left~dbinom(1,p),
    logit(p)<-a+(bp+bpC*condition)*prosoc_left,
    a~dnorm(0,10),
    c(bp,bpC)~dnorm(0,10)
  ),data=d)

compare(m10.1,m10.2,m10.3)
source("C:\\Users\\oc3512\\Documents\\StatisticalRethinking\\compare_plot_Rethinking.R")
op=par(mfrow=c(2,2))
plot(compare(m10.1,m10.2,m10.3))

precis(m10.3)
exp(0.61)

pred.data=data.frame(prosoc_left=c(0,1,0,1), #right/left/right/left
                     condition=c(0,0,1,1)) #control/control/partner/partner
chimp.ensemble=ensemble(m10.1,m10.2,m10.3,data=pred.data)                     
pred.mean=apply(chimp.ensemble$link,2,mean)
pred.PI=apply(chimp.ensemble$link,2,PI)

head(d)
unique(d$actor)
p=by(d$pulled_left,list(d$prosoc_left,d$condition,d$actor),mean)

# plot
plot(0,0,type="n",xlab="prosoc_left/conditino",ylab="proportion pulled_left",
     xlim=c(1,4),ylim=c(0,1),xaxt="n")
axis(1,at=1:4,labels=c("0/0","1/0","0/1","1/1"))
str(p)
for(chimp in 1:7){
  lines(1:4,as.vector(p[,,chimp]),col=rangi2,lwd=1.5)
}

d2=d
d2$recipient=NULL
m10.3stan=map2stan(m10.3,data=d2,iter=1e4,warmup=1000)
precis(m10.3stan)
show(m10.3stan)
pairs(m10.3stan)

m10.4=map2stan(
  alist(
    pulled_left~dbinom(1,p),
    logit(p)<-a[actor]+(bp+bpC*condition)*prosoc_left,
    a[actor]~dnorm(0,10),
    c(bp,bpC)~dnorm(0,10)
  ),data=d2,cores=2,chains=2,iter=2500,warmup=500)

precis(m10.4,depth=2)
pairs(m10.4)
post=extract.samples(m10.4)
str(post)
dens(post$a[,2])
postcheck(m10.4)

chimp=1
d.pred=list(
  #pulled_left=rep(0,4),
  prosoc_left=c(0,1,0,1),
  condition=c(0,0,1,1),
  actor=rep(chimp,4)
)
mu=link(m10.4,data=d.pred)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI)

plot(0,0,type="n",xlab="prosoc_left/condition",ylab="proportion pulled left",
     xlim=c(1,4),ylim=c(0,1),xaxt="n")
mtext(paste("Actor=",chimp))
axis(1,at=1:4,labels=c("0/0","1/0","0/1","1/1"))
p=by(d$pulled_left,list(d$prosoc_left,d$condition,d$actor),mean)
lines(1:4,as.vector(p[,,chimp]),col=rangi2,lwd=2)
lines(1:4,mu.mean)
shade(mu.PI,1:4)

chimp=7
d.pred=data.frame(prosoc_left=c(0,1,0,1),condition=c(0,0,1,1),actor=rep(chimp,4))
mu=link(m10.4,data=d.pred)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI)
plot(0,0,type="n",xlab="prosoc_left/condition",ylab="proportino pulled left",xaxt="n",
     xlim=c(1,4),ylim=c(0,1))
mtext(paste("Actor ",chimp))
axis(1,at=1:4,labels=c("0/0","1/0","0/1","1/1"))
p=by(d$pulled_left,list(d$prosoc_left,d$condition,d$actor),mean)
lines(1:4,as.vector(p[,,chimp]),col=rangi2,lwd=1.5)
lines(1:4,mu.mean)
shade(mu.PI,1:4)

d.agg=aggregate(d$pulled_left,list(d$prosoc_left,d$condition,d$actor),sum)
