library(rethinking)

op=par(mfrow=c(2,2))
curve(dnorm(x,178,20),from=100,to=250)
curve(dunif(x,0,50),from=10,to=60)

sample_mu=rnorm(1e4,178,20)
sample_sigma=runif(1e4,0,10)
prior_h=rnorm(1e4,sample_mu,sample_sigma)
dens(prior_h)

data("Howell1")
d=Howell1
d2=d[d$age>=18,]

mu.list=seq(from=140,to=160,length.out=200)
sigma.list=seq(from=4,to=9,length.out=200)
post=expand.grid(mu=mu.list,sigma=sigma.list)
post$LL=sapply(1:nrow(post),function(i) sum(dnorm(
  d2$height,
  mean=post$mu[i],
  sd=post$sigma[i],
  log=TRUE)))
post$prod=post$LL+dnorm(post$mu,178,20,TRUE)+dunif(post$sigma,0,500,TRUE)
post$prob=exp(post$prod-max(post$prod))
head(post)
contour_xyz(post$mu,post$sigma,post$prob)
image_xyz(post$mu,post$sigma,post$prob)

sample.row=sample(1:nrow(post),1e4,replace = TRUE,prob=post$prob)
sample.mu=post$mu[sample.row]
sample.sigma=post$sigma[sample.row]
plot(sample.mu,sample.sigma,cex=0.5,col=col.alpha(rangi2,0.1))
dens(sample.mu,norm.comp = TRUE)
dens(sample.sigma,norm.comp=TRUE)
HPDI(sample.mu);HPDI(sample.sigma)

d3=sample(d2$height,size=20)
mu.list=seq(from=150,to=170,length.out=200)
mu.sigma=seq(from=4,to=20,length.out=200)
post2=expand.grid(mu=mu.list,sigma=sigma.list)
head(post2)
post2$LL=sapply(1:nrow(post2),function(i) sum(dnorm(
  d3,post2$mu[i],post2$sigma[i],log=TRUE
)))
post2$prod=post2$LL+dnorm(post2$mu,178,20,TRUE)+dunif(post2$sigma,0,50,TRUE)
post2$prob=exp(post2$prod-max(post2$prod))
sample2.row=sample(1:nrow(post2),size=1e4,replace = TRUE,prob=post2$prob)
sample2.mu=post2$mu[sample2.row]
sample2.sigma=post2$sigma[sample2.row]
plot(sample2.mu,sample2.sigma,cex=0.5,col=col.alpha(rangi2,0.1),pch=16,xlab="muc",ylab="sigmac")
dens(sample2.sigma,norm.comp = TRUE,xlim=c(5,12))

flist<-alist(
  height~dnorm(mu,sigma),
  mu~dnorm(178,20),
  sigma~dunif(0,50)
)
m4.1<-map(flist,data=d2)
precis(m4.1)

m4.2=map(
  alist(
    height~dnorm(mu,sigma),
    mu~dnorm(178,0.1),
    sigma~dunif(0,50)
  ),data=d2)
m4.2
precis(m4.2)
vcov(m4.1)
diag(vcov(m4.1))
cov2cor(vcov(m4.1))

post=extract.samples(m4.1,n=1e4)
head(post)
precis(post)
precis(m4.1)

m4.1_logsigma=map(
  alist(
    height~dnorm(mu,exp(log_sigma)),
    mu~dnorm(178,20),
    log_sigma~dnorm(2,10)
  ),data=d2)
post=extract.samples(m4.1_logsigma)
head(post)
sigma=exp(post$log_sigma)

plot(d2$height,d2$weight)

m4.3=map(
  alist(
    height~dnorm(mu,sigma),
    mu<-a+b*weight,
    a~dnorm(178,100),
    b~dnorm(0,10),
    sigma~dunif(0,50)
  ),data=d2
)
m4.3
precis(m4.3)
s=extract.samples(m4.3,1e4)
dens(s$sigma)
HPDI(s$a)
precis(m4.3,corr=TRUE)
precis(m4.3,corr=TRUE)
precis(m4.3,corr=TRUE)

d2$weight.c=d2$weight-mean(d2$weight)
m4.4=map(
  alist(
    height~dnorm(mu,sigma),
    mu<-a+b*weight.c,
    a~dnorm(178,20),
    b~dnorm(0,10),
    sigma~dunif(0,50)
  ),data=d2)
precis(m4.4,corr=TRUE)
mean(d2$height)
plot(height~weight,data=d2,col=col.alpha(rangi2,0.5))
abline(a=coef(m4.3)["a"],b=coef(m4.3)["b"])

post=extract.samples(m4.3)
head(post)

N=150
dN=d2[1:N,]
mN=map(
  alist(
    height~dnorm(mu,sigma),
    mu<-a+b*weight,
    a~dnorm(178,20),
    b~dnorm(0,10),
    sigma~dunif(0,50)
  ),data=dN)
post=extract.samples(mN,n=20)
plot(dN$weight,dN$height,xlim=range(d2$weight),ylim=range(d2$height),
     col=rangi2,xlab="weight",ylab="height")
tt=sapply(1:50,function(i) 
  abline(a=post$a[i],b=post$b[i],col=col.alpha("black",0.3)))

mu_at_50=post$a+post$b*50
dens(mu_at_50,col=rangi2,lwd=2)
HPDI(mu_at_50)

m4.3
nrow(d2)
mu=link(m4.3)
str(mu)

weight.seq=seq(from=25,to=70,by=1)
mu=link(m4.3,data=data.frame(weight=weight.seq))
str(mu)
plot(height~weight,d2,type="n")
for(i in 1:100){
  points(weight.seq,mu[i,],pch=16,col=col.alpha(rangi2,0.1))
  }
op=par(mfrow=c(2,2))
mu.mean=apply(mu,2,mean)
mu.HPDI=apply(mu,2,HPDI,prob=0.89)
plot(height~weight,data=d2,col=rangi2)
lines(weight.seq,mu.mean)
shade(mu.HPDI,weight.seq)

post=extract.samples(m4.3)
mu.link=function(weight) post$a+post$b*weight
weight.seq=seq(from=25,to=70,by=1)
mu=sapply(weight.seq,function(weight) post$a+post$b*weight)
head(mu)
mu.mean=apply(mu,2,mean)
mu.HPDI=apply(mu,2,HPDI,prob=0.89)
plot(height~weight,data=d2,type="n")
for(i in 1:100){points(weight.seq,mu[i,],pch=16,col=col.alpha(rangi2,0.1))}
plot(height~weight,data=d2,col=rangi2)
lines(weight.seq,mu.mean)
shade(mu.HPDI,weight.seq)

sim.height=sim(m4.3,data=list(weight=weight.seq))
str(sim.height)
height.PI=apply(sim.height,2,PI,prob=0.89)
plot(height~weight,data=d2,col=rangi2)
lines(weight.seq,mu.mean)
shade(mu.HPDI,weight.seq)
shade(height.PI,weight.seq)

sim.height=sim(m4.3,data=list(weight=weight.seq),n=1e4)
str(sim.height)
height.PI=apply(sim.height,2,PI,prob=0.89)
plot(height~weight,data=d2,col=rangi2)
lines(weight.seq,mu.mean)
shade(mu.HPDI,weight.seq)
shade(height.PI,weight.seq)

d$weight.s=(d$weight-mean(d$weight))/sd(d$weight)
d$weight.s2=d$weight.s^2

m4.5=map(
  alist(
    height~dnorm(mu,sigma),
    mu<-a+b1*weight.s+b2*weight.s2,
    a~dnorm(178,100),
    b1~dnorm(0,10),
    b2~dnorm(0,10),
    sigma~dunif(0,50)
  ),data=d)
precis(m4.5,corr=TRUE)

# A linear line of the mean height based on samples from posterior distributions
weight.seq=seq(from=-2.2,to=2,length.out=30)
mu=link(m4.5,data=list(weight.s=weight.seq,weight.s2=weight.seq^2))
mu.mean=apply(mu,2,mean)
mu.HPDI=apply(mu,2,HPDI,prob=0.89)

sim.height=sim(m4.5,data=list(weight.s=weight.seq,weight.s2=weight.seq^2))
height.HPDI=apply(sim.height,2,HPDI,prob=0.89)

plot(height~weight.s,data=d,col=col.alpha(rangi2,0.5))
lines(weight.seq,mu.mean)
shade(mu.HPDI,weight.seq)
shade(height.HPDI,weight.seq)

# cubic regression
d$weight.s3=d$weight.s^3

m4.6=map(
  alist(
    height~dnorm(mu,sigma),
    mu<-a+b1*weight.s+b2*weight.s2+b3*weight.s3,
    a~dnorm(178,100),
    b1~dnorm(0,10),
    b2~dnorm(0,10),
    b3~dnorm(0.10),
    sigma~dunif(0,50)
  ),data=d)
mu=link(m4.6,data=list(weight.s=weight.seq,weight.s2=weight.seq^2,weight.s3=weight.seq^3))
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
sim.height=sim(m4.6,data=list(weight.s=weight.seq,weight.s2=weight.seq^2,weight.s3=weight.seq^3))
height.PI=apply(sim.height,2,PI,prob=0.89)

plot(height~weight.s,data=d,col=col.alpha(rangi2,0.5))
lines(weight.seq,mu.mean)
shade(mu.PI,weight.seq)
shade(height.PI,weight.seq)

plot(height~weight.s,data=d,col=col.alpha(rangi2,0.5),xaxt="n")
at=c(-2,-1,0,1,2)
labels=at*sd(d$weight)+mean(d$weight)
axis(side=1,at=at,labels=round(labels,1))

#4M1
prior.mean=rnorm(1e4,0,10)
prior.sigma=runif(1e4,0,10)
simulated.height.from.prior=rnorm(1e4,prior.mean,prior.sigma)
dens(simulated.height.from.prior)

prior.mu=rnorm(1e4,0,10)
prior.sig=runif(1e4,0,10)
sim.height=rnorm(1e4,prior.mu,prior.sig)

#4M2
flist=alist(
  y~dnorm(mu,sigma),
  mu<-dnorm(0,10),
  sigma~dunif(0.10)
)

#4M3

#4H1
data("Howell1")
d=Howell1
d$weight.c=d$weight-mean(d$weight)

model=map(
  alist(
    height~dnorm(mu,sigma),
    mu<-a+b*weight.c,
    a~dnorm(170,50),
    b~dnorm(0.10),
    sigma~dunif(0,50)
  ),data=d)
precis(model,corr = TRUE)
ind.weight=c(46.95,43.72,64.78,32.59,54.63)
ind.weight.c=ind.weight-mean(ind.weight)
sim.height=sim(model,data=list(weight.c=ind.weight.c),n=1e4)
sapply(1:5,function(i) PI(sim.height[,i],prob=0.89))
sapply(1:5,function(i) mean(sim.height[,i]))


model=map(
  alist(
    height~dnorm(mu,sigma),
    mu<-a+b*weight.c,
    a~dnorm(170,50),
    b~dnorm(0,10),
    sigma~dunif(0,10)
  ),data=d)

ind.weight=c(46.95,43.72,64.78,32.59,54.63)
ind.weight.c=ind.weight-mean(ind.weight)
post=extract.samples(model)
sim.height=sapply(ind.weight.c,function(weight)
  rnorm(
    n=nrow(post),
    mean=post$a+post$b*weight,
    sd=post$sigma
  ))
str(sim.height)
mean=apply(sim.height,2,mean)
PI=apply(sim.height,2,PI,prob=0.89)

#link = mean height
post=extract.samples(model)
mu.link=sapply(ind.weight.c,function(weight) post$a+post$b*weight)
mu.link.mean=apply(mu.link,2,mean)
mu.link.PI=apply(mu.link,2,PI,prob=0.89)

# Simulate: acutual height
post=extract.samples(model)
sim.height=sapply(ind.weight.c,function(weight)
  rnorm(
    n=nrow(post),
    mean=post$a+post$b*weight,
    sd=post$sigma
  ))
apply(sim.height,2,mean)
apply(sim.height,2,HPDI,prob=0.89)

#4H2
head(d)
d2=d[d$age<18,]
d2$weight.c=d2$weight-mean(d2$weight)

#(a)
model=map(
  alist(
    height~dnorm(mu,sigma),
    mu<-a+b*weight.c,
    a~dnorm(170,20),
    b~dnorm(0,10),
    sigma~dunif(0,50)
  ),data=d2)
precis(model,corr=TRUE)

#(b)
min(d2$weight.c);max(d2$weight.c)
weight.seq=seq(from=-15,to=27,by=1)
post=extract.samples(model)
mu.link=sapply(weight.seq,function(weight) post$a+post$b*weight)
mu.link.mean=apply(mu.link,2,mean)
mu.link.HPDI=apply(mu.link,2,HPDI,prob=0.89)

sim.height=sapply(weight.seq,function(weight)
  rnorm(
    n=nrow(post),
    mean=post$a+post$b*weight,
    sd=post$sigma
  ))
sim.height.HPDI=apply(sim.height,2,HPDI,prob=0.89)

plot(height~weight.c,data=d2,col=col.alpha(rangi2,0.5))
lines(weight.seq,mu.link.mean)
shade(mu.link.HPDI,weight.seq)
  shade(sim.height.HPDI,weight.seq)
  
#4H3
d$weight.log=log(d$weight)

model=map(
  alist(
    height~dnorm(mu,sigma),
    mu<-a+b*log(weight),
    a~dnorm(178,100),
    b~dnorm(0,100),
    sigma~dunif(0,50)
  ),data=d)
precis(model)

post=extract.samples(model)
weight.seq=seq(from=1,to=70,length.out=100)

# MAP and 89%HPDI for the mean
mu.link=sapply(weight.seq,function(weight) post$a+post$b*log(weight))
mu.link.mean=apply(mu.link,2,mean)
mu.link.HPDI=apply(mu.link,2,HPDI,prob=0.89)

plot(height~weight,data=d,col=col.alpha(rangi2,0.5))
lines(weight.seq,mu.link.mean)
shade(mu.link.HPDI,weight.seq)
shade(sim.height.HPDI,weight.seq)

# 89% HPDI for actual height
sim.height=sapply(weight.seq,function(weight)
  rnorm(
    n=nrow(post),
    mean=post$a+post$b*log(weight),
    sd=post$sigma
  ))
sim.height.HPDI=apply(sim.height,2,HPDI,prob=0.89)

plot(height~log(weight),data=d,col=col.alpha(rangi2,0.5))
lines(log(weight.seq),mu.link.mean)
shade(mu.link.HPDI,log(weight.seq))
shade(sim.height.HPDI,log(weight.seq))
precis(model)
log(1)


##############
data(Howell1)
d <- Howell1
trials <- 1e5

model <- map(
  alist(
    height ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta*log(weight),
    alpha ~ dnorm(mean = 178, sd = 100),
    beta ~ dnorm(mean = 0, sd = 100),
    sigma ~ dunif(min = 0, max = 50)
  ),
  data = d
)
precis(model)
??mvrnorm
library(MASS)
weight.seq <- seq(from = 1, to = 70, length.out = 100)
posterior.samples <- data.frame( mvrnorm(n = trials, mu = coef(model), Sigma = vcov(model)) )
mu.link <- function(weight) posterior.samples$alpha + posterior.samples$beta * log(weight)
mu <- sapply(X = weight.seq, FUN = mu.link)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.hpdi <- apply(X = mu, MARGIN = 2, FUN = HPDI, prob = .89)

# simulate heights then compute hpdi
height.link <- function(weight) rnorm(n = nrow(posterior.samples), mean = mu.link(weight), sd = posterior.samples$sigma)
height.samples <- sapply(X = weight.seq, FUN = height.link)
height.hpdi <- apply(X = height.samples, MARGIN = 2, FUN = HPDI, prob = .89)

# plot results
plot(height ~ weight, data = d, col = col.alpha(rangi2, .4))
lines(x = weight.seq, y = mu.mean)
shade(object = mu.hpdi, lim = weight.seq)
shade(object = height.hpdi, lim = weight.seq)