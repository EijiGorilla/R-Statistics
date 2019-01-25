library(rethinking)
data("rugged")
d=rugged
d$log_gdp=log(d$rgdppc_2000)
dd=d[complete.cases(d$rgdppc_2000),]

m8.1=map(
  alist(
    log_gdp~dnorm(mu,sigma),
    mu<-a+bR*rugged+bA*cont_africa+bAR*rugged*cont_africa,
    a~dnorm(0,100),
    c(bR,bA,bAR)~dnorm(0,10),
    sigma~dunif(0,10)
  ),data=dd)
precis(m8.1)

dd.trim=dd[,c("log_gdp","rugged","cont_africa")]
str(dd.trim)

m8.1stan=map2stan(
  alist(
    log_gdp~dnorm(mu,sigma),
    mu<-a+bR*rugged+bA*cont_africa+bAR*rugged*cont_africa,
    a~dnorm(0,100),
    c(bR,bA,bAR)~dnorm(0,10),
    sigma~dcauchy(0,2)
  ),data=dd.trim)
precis(m8.1stan)

m8.1stan_2chains=map2stan(m8.1stan,chains=2,core=2)
precis(m8.1stan_2chains)
post=extract.samples(m8.1stan)
str(post)
pairs(post)
pairs(m8.1stan)
show(m8.1stan)

plotchains(m8.1stan,col="blue",alpha=0.5,bg=gray(0.7,0.5))
tracerplot(m8.1stan)

library(rethinking)
y=c(-1,1)
m8.2=map2stan(
  alist(
    y~dnorm(mu,sigma),
    mu<-alpha
  ),data=list(y=y),start=list(alpha=0,sigma=1),chains=2,iter=4000,warmup=1000)
precis(m8.2)
plot(m8.2)

op=par(mfrow=c(2,2))
plot(m8.2)

m8.3=map2stan(
  alist(
    y~dnorm(mu,sigma),
    mu<-alpha,
    alpha~dnorm(1,2),
    sigma~dcauchy(0,1)
  ),data=list(y=y),start=list(alpha=0,sigma=1),chains=2,iter=4000,warmup=1000)
plot(m8.3)
precis(m8.3)
post=extract.samples(m8.3)
dens(post$alpha)
pr=rnorm(1e5,1,1)
dens(pr,lty=2,add=TRUE)

y=rnorm(100,mean=0,sd=1)
m8.4=map2stan(
  alist(
    y~dnorm(mu,sigma),
    mu<-a1+a2,
    sigma~dcauchy(0,1)
  ),data=list(y=y),start=list(a1=0,a2=0,sigma=1),chains=2,iter=4000,warmup=1000)
precis(m8.4)
plot(m8.4)

m8.5=map2stan(
  alist(
    y~dnorm(mu,sigma),
    mu<-a1+a2,
    c(a1,a2)~dnorm(0,10),
    sigma~dcauchy(0,1)
  ),data=list(y=y),start=list(a1=0,a2=0,sigma=1),chains=2,iter=4000,warmup=1000)
precis(m8.5)
plot(m8.5)

#8M1
data("rugged")
d=rugged
d$rugged.c=d$rugged-mean(d$rugged)
d$log_gdp=log(d$rgdppc_2000)
dd=d[complete.cases(d$rgdppc_2000),]
dd.trim=dd[,c("log_gdp","rugged","cont_africa")]

# uniform prior
m.8m1=map2stan(
  alist(
    log_gdp~dnorm(mu,sigma),
    mu<-a+bR*rugged+bA*cont_africa+bAR*rugged*cont_africa,
    a~dnorm(0,10),
    c(bR,bA,bAR)~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=dd.trim,chains=2,iter=4000,warmup=1000)

m.8m1.exp=map2stan(
  alist(
    log_gdp~dnorm(mu,sigma),
    mu<-a*bR*rugged+bA*cont_africa+bAR*rugged*cont_africa,
    a~dnorm(0,10),
    c(bR,bA,bAR)~dnorm(0,1),
    sigma~dexp(1)
  ),data=dd.trim,chains=2,iter=4000,warmup=1000)

# prior's posterior distributions
dc.post=extract.samples(m.8m1)
exp.post=extract.samples(m.8m1.exp)

# plot posterior distributions and prior
dens(dc.post$sigma)
dens(exp.post$sigma)

curve(dunif(x,0,10))
curve(dcauchy(x,0,10))
curve(dexp(x,1))

pairs(m.8m1)
pairs(m.8m1.exp)
op=par(mfrow=c(2,2))

precis_plot(precis(m.8m1))
precis_plot(precis(m.8m1.exp))
compare(m.8m1,m.8m1.exp)

#8M2
## dcauchy
m.dc1=map2stan(
  alist(
    log_gdp~dnorm(mu,sigma),
    mu<-a+bR*rugged+bA*cont_africa+bAR*rugged*cont_africa,
    a~dnorm(0,5),
    c(bR,bA,bAR)~dnorm(0,1),
    sigma~dcauchy(0,1)
  ),data=dd.trim,chains=2,iter=4000,warmup=1000,cores=2)

m.dc2=map2stan(
  alist(
    log_gdp~dnorm(mu,sigma),
    mu<-a+bR*rugged+bA*cont_africa+bAR*rugged*cont_africa,
    a~dnorm(0,5),
    c(bR,bA,bAR)~dnorm(0,1),
    sigma~dcauchy(0,0.5)
  ),data=dd.trim,chains=2,iter=4000,warmup=1000,cores=2)

## dexp
m.exp1=map2stan(
  alist(
    log_gdp~dnorm(mu,sigma),
    mu<-a+bR*rugged+bA*cont_africa+bAR*rugged*cont_africa,
    a~dnorm(0,5),
    c(bR,bA,bAR)~dnorm(0,1),
    sigma~dexp(1)
  ),data=dd.trim,chains=2,iter=4000,warmup=1000,cores=2)

m.exp2=map2stan(
  alist(
    log_gdp~dnorm(mu,sigma),
    mu<-a+bR*rugged+bA*cont_africa+bAR*rugged*cont_africa,
    a~dnorm(0,5),
    c(bR,bA,bAR)~dnorm(0,1),
    sigma~dexp(0.5)
  ),data=dd.trim,chains=2,iter=4000,warmup=1000,cores=2)

op=par(mfrow=c(2,2))

precis_plot(precis(m.dc1))
precis_plot(precis(m.dc2))
precis_plot(precis(m.exp1))
precis_plot(precis(m.exp2))
compare(m.exp1,m.exp2)

pairs(m.dc1)
pairs(m.dc2)
pairs(m.exp1)
pairs(m.exp2)

post.dc1=extract.samples(m.dc1)
post.dc2=extract.samples(m.dc2)
dens(post.dc1$sigma)
dens(post.dc2$sigma)

post.exp1=extract.samples(m.exp1)
post.exp2=extract.samples(m.exp2)
dens(post.exp1$sigma)
dens(post.exp2$sigma)  

#8M3
library(rethinking)
m=map2stan(
  alist(
    log_gdp~dnorm(mu,sigma),
    mu<-a+bR*rugged+bA*cont_africa+bAR*rugged*cont_africa,
    a~dnorm(0,10),
    c(bR,bA,bAR)~dnorm(0,1),
    sigma~dcauchy(0,2)
  ),data=dd.trim)

m.w10=map2stan(m,chains=4,warmup=10,iter=1000,cores=4)
m.w100=map2stan(m,chains=4,warmup=100,iter=1000,cores=4)
m.w200=map2stan(m,chains=4,warmup=200,iter=1000,cores=4)
m.w500=map2stan(m,chains=4,warmup=500,iter=1000,cores=4)
m.w1000=map2stan(m,chains=4,warmup=1000,iter=1000,cores=4)

precis(m.w10)
precis(m.w100)
precis(m.w200)
precis(m.w500)
precis(m.w1000)

#8H1
mp=map2stan(
  alist(
    a~dnorm(0,1),
    b~dcauchy(0,5)
  ),data=list(y=1),start=list(a=0,b=0),iter=1e4,warmup=100,WAIC=FALSE,cores=4)
?dcauchy
precis(mp)
pairs(mp)
tracerplot(mp)
post=extract.samples(mp)
str(post)
op=par(mfrow=c(2,2))
dens(post$a)
dens(post$b,xlim=c(-50,50))

#8H2
library(rethinking)
data("WaffleDivorce")
d=WaffleDivorce

op=par(mfrow=c(2,2))
d$MedianAgeMarriage.s=(d$MedianAgeMarriage-mean(d$MedianAgeMarriage))/sd(d$MedianAgeMarriage)
d$Marriage.s=(d$Marriage-mean(d$Marriage))/sd(d$Marriage)

d.trim=d[,c("Divorce","MedianAgeMarriage.s","Marriage.s")]

m5.1stan=map2stan(
  alist(
    Divorce~dnorm(mu,sigma),
    mu<-a+bA*MedianAgeMarriage.s,
    a~dnorm(0,5),
    bA~dnorm(0,1),
    sigma~dcauchy(0,2)
  ),data=d.trim,chains=4,iter=4000,cores=4,warmup=1000)
tracerplot(m5.1stan)

m5.2stan=map2stan(
  alist(
    Divorce~dnorm(mu,sigma),
    mu<-a+bM*Marriage.s,
    a~dnorm(0,5),
    bM~dnorm(0,1),
    sigma~dcauchy(0,2)
  ),data=d.trim,chains=4,iter=4000,cores=4,warmup=1000)
tracerplot(m5.2stan)

m5.3stan=map2stan(
  alist(
    Divorce~dnorm(mu,sigma),
    mu<-a+bM*Marriage.s+bA*MedianAgeMarriage.s,
    a~dnorm(0,5),
    c(bM,bA)~dnorm(0,1),
    sigma~dcauchy(0,2)
  ),data=d.trim,chains=4,iter=4000,cores=4,warmup=1000)
tracerplot(m5.3stan)

compare(m5.1stan,m5.2stan,m5.3stan)
precis(m5.1stan)
coeftab(m5.1stan,m5.2stan,m5.3stan)
#WAIC indicates that a model including only median age marriage gives has better weight (0.7).
# The number of effective sample is high and Rhat is one, indicating chains were efficient.

#Get posterior distributions
plot(Divorce~MedianAgeMarriage.s,data=d.trim,col=rangi2)
median.age.seq=seq(from=-4,to=4,length.out = 50)
pred.data=data.frame(MedianAgeMarriage_s=median.age.seq)
mu=link(m5.1stan,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
lines(median.age.seq,mu.mean)
shade(mu.PI,median.age.seq)
post=extract.samples(m5.1stan)
dens(post$a)
pairs(m5.1stan)

#8H3
N=100
height=rnorm(N,10,2)
leg_prop=runif(N,0.4,0.5)
leg_left=leg_prop*height+rnorm(N,0,0.02)
leg_right=leg_prop*height+rnorm(N,0,0.02)
d=data.frame(height,leg_left,leg_right)

m5.8s=map2stan(
  alist(
    height~dnorm(mu,sigma),
    mu<-a+bl*leg_left+br*leg_right,
    a~dnorm(10,100),
    c(bl,br)~dnorm(2,10),
    sigma~dcauchy(0,1)
  ),data=d,chains=4,start=list(a=10,bl=0,br=0,sigma=1)
)
tracerplot(m5.8s)

m5.8s2=map2stan(
  alist(
    height~dnorm(mu,sigma),
    mu<-a+bl*leg_left+br*leg_right,
    a~dnorm(10,100),
    bl~dnorm(2,10),
    br~dnorm(2,10) & T[0,],
    sigma~dcauchy(0,1)
  ),data=d,chains=4,start=list(a=10,bl=0,br=0,sigma=1)
)
tracerplot(m5.8s2)
post=extract.samples(m5.8s)
post2=extract.samples(m5.8s2)

str(post)
dens(post$bl)
dens(post$br)
dens(post2$bl)
dens(post2$br)
# Because both parameters bl and br are strongly correlated, the change in the prior for br had 
# a great impact on bl showing left-skewed distribution.

precis(m5.8s)
precis(m5.8s2)
compare(m5.8s,m5.8s2)
# The effective number of parameters are relativey high for m5.8s, while the second model has too low an effective
# number of parameters indicating chains are inefficient. The inefficient chains seem to be derived from
# skwed posterior distribution. Chains are so inefficient for the m5.8s2 that chains did not converge and 
# not stationary and poor mixing.
