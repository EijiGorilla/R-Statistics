library(rethinking)
data("WaffleDivorce")
d=WaffleDivorce

head(d)
op=par(mfrow=c(2,2))
d$MedianAgeMarriage.s=(d$MedianAgeMarriage-mean(d$MedianAgeMarriage))/sd(d$MedianAgeMarriage)

plot(Divorce~MedianAgeMarriage.s,data=d,col=col.alpha(rangi2,0.5))

#fit model
m5.1=map(
  alist(
    Divorce~dnorm(mu,sigma),
    mu<-a+b*MedianAgeMarriage.s,
    a~dnorm(10,10),
    b~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d)
precis(m5.1,corr=TRUE)

# MAP and 89% PI
MAM.seq=seq(from=-3,to=3.5,length.out = 30)
post=extract.samples(m5.1)
mu=link(m5.1,data=data.frame(MedianAgeMarriage.s=MAM.seq))
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)

# plot
plot(Divorce~MedianAgeMarriage.s,data=d,col=rangi2)
abline(m5.1)
lines(MAM.seq,mu.mean)
shade(mu.PI,MAM.seq)
precis(m5.1)

d$Marriage.s=(d$Marriage-mean(d$Marriage))/sd(d$Marriage)
m5.2=map(
  alist(
    Divorce~dnorm(mu,sigma),
    mu<-a+b*Marriage.s,
    a~dnorm(10,10),
    b~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d)
plot(Divorce~Marriage.s,data=d,col=rangi2)
MA.seq=seq(from=-3,to=3,length.out=40)
post=extract.samples(m5.2)
mu=sapply(MA.seq,function(Marriage) post$a+post$b*Marriage)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
abline(m5.2)
shade(mu.PI,MA.seq)
precis(m5.2)

head(d)
m5.3=map(
  alist(
    Divorce~dnorm(mu,sigma),
    mu<-a+bR*Marriage.s+bA*MedianAgeMarriage.s,
    a~dnorm(10,10),
    bR~dnorm(0,1),
    bA~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d)
precis(m5.3,corr=TRUE)

library(rethinking)
plot(precis(m5.3))

op=par(mfrow=c(2,2))

# Predictor residuals
m5.4=map(
  alist(
    Marriage.s~dnorm(mu,sigma),
    mu<-a+b*MedianAgeMarriage.s,
    a~dnorm(0,10),
    b~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d)
m5.4
mu=coef(m5.4)["a"]+coef(m5.4)["b"]*d$MedianAgeMarriage.s
m.resid=d$Marriage.s-mu
plot(Marriage.s~MedianAgeMarriage.s,d,col=rangi2)
abline(m5.4)
for(i in 1:length(m.resid)){
  x=d$MedianAgeMarriage.s[i]
  y=d$Marriage.s[i]
  lines(c(x,x),c(mu[i],y),lwd=5,col=col.alpha("black",0.7))
}
library(rethinking)

plot(Marriage.s~MedianAgeMarriage.s,d,col=rangi2)
abline(m5.4)
for(i in 1:length(m.resid)){
  x=d$MedianAgeMarriage.s[i]
  y=d$Marriage.s[i]
  lines(c(x,x),c(mu[i],y),lwd=0.5,col=col.alpha("black",0.7))
}
op=par(mfrow=c(2,2))

plot(Divorce~m.resid,d,col=rangi2)
abline(v=0)

m5.41=map(
  alist(
    MedianAgeMarriage.s~dnorm(mu,sigma),
    mu<-a+b*Marriage.s,
    a~dnorm(0,10),
    b~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d)
m5.41
mu=coef(m5.41)["a"]+coef(m5.41)["b"]*d$Marriage.s
m.resid=d$MedianAgeMarriage.s-mu
plot(MedianAgeMarriage.s~Marriage.s,d,col=rangi2)
abline(m5.41)
for(i in 1:length(m.resid)){
  x=d$Marriage.s[i]
  y=d$MedianAgeMarriage.s[i]
  lines(c(x,x),c(mu[i],y),col=col.alpha("black",0.4))
}
plot(d$Divorce~m.resid,col=rangi2)
abline(v=0,l)
?abline

# Counterfactual plots
m5.3=map(
  alist(
    Divorce~dnorm(mu,sigma),
    mu<-a+bR*Marriage.s+bA*MedianAgeMarriage.s,
    a~dnorm(10,10),
    bR~dnorm(0,1),
    bA~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d)

A.ave=mean(d$MedianAgeMarriage.s)
R.seq=seq(from=-3,to=3,length.out = 30)
post=extract.samples(m5.3)
head(post)
pred.data=data.frame(Marriage.s=R.seq,MedianAgeMarriage.s=A.ave)

# Compute counterfactual mean divorce
mu=link(m5.3,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)

# compute counterfactual divorce outcome
R.sim=sim(m5.3,data=pred.data,n=1e4)
R.sim.mean=apply(R.sim,2,mean)
R.sim.PI=apply(R.sim,2,PI,prob=0.89)
plot(Divorce~Marriage.s,data=d,type="n")
mtext("MedianAgeMarriage.s=0")
lines(R.seq,mu.mean)
shade(mu.PI,R.seq)
shade(R.sim.PI,R.seq)

plot(Divorce~MedianAgeMarriage.s,d)
R.ave=mean(d$Marriage.s)
A.seq=seq(from=-3,to=3,length.out=50)
pred.data=data.frame(MedianAgeMarriage.s=A.seq,Marriage.s=R.ave)

# compute counterfactual mean median age at marriage
mu=link(m5.3,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)

# compute counterfactual actual median age at marriage
A.sim=sim(m5.3,data=pred.data,n=1e4)
A.sim.PI=apply(A.sim,2,PI,prob=0.89)

plot(Divorce~MedianAgeMarriage.s,data=d,type="n")
mtext("Marriage.s=0")
lines(A.seq,mu.mean)
shade(mu.PI,A.seq)
shade(A.sim.PI,A.seq)


# Posterior prediction plots
# call link to compute predicted mean for each observation of Divorce
mu=link(m5.3)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)

# plot obs. vs pred. for Divorce
plot(mu.mean~d$Divorce,col=rangi2,ylim=range(mu.PI),xlab="Observed Divorce",ylab="Predicted Divorce")
abline(a=0,b=1,col=col.alpha("black",0.4))
for(i in 1:nrow(d)){
  lines(rep(d$Divorce[i],2),c(mu.PI[1,i],mu.PI[2,i]),col=col.alpha("black",0.7))
}
identify(x=d$Divorce,y=mu.mean,labels=d$Loc,cex=0.8)

# compute residulas
div.resid=d$Divorce-mu.mean

# simulate for computing 89% predicted values for acutual divorce
div.sim=sim(m5.3,n=1e4)
sim.PI=apply(div.sim,2,PI,prob=0.89)

# plot predicted residuals by states
div.resid
o=order(div.resid)
dotchart(div.resid[o],labels=d$Loc[o],cex=0.6)
abline(v=0,col=col.alpha("black",0.4))
for(i in 1:nrow(d)){
  j=o[i]
  lines(d$Divorce[j]-c(mu.PI[1,j],mu.PI[2,j]),rep(i,2),col=col.alpha("black",0.4))
  points(d$Divorce[j]-c(sim.PI[1,j],sim.PI[2,j]),rep(i,2),pch=3,col=col.alpha("black",0.4),cex=0.6)
}

#
data("milk")
d=milk
dcc=d[complete.cases(d),]
head(dcc)

m5.5=map(
  alist(
    kcal.per.g~dnorm(mu,sigma),
    mu<-a+b*neocortex.perc,
    a~dnorm(45,100),
    b~dnorm(0,1),
    sigma~dunif(0,1)
  ),data=dcc)
precis(m5.5,digits=3)
plot(kcal.per.g~neocortex.perc,dcc,col=rangi2)

np.seq=0:100
pred.data=data.frame(neocortex.perc=np.seq)
mu=link(m5.5,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
lines(np.seq,mu.mean)
shade(mu.PI,np.seq)

str(dcc)
plot(kcal.per.g~log(mass),dcc,col=rangi2)

dcc$log.mass=log(dcc$mass)
m5.6=map(
  alist(
    kcal.per.g~dnorm(mu,sigma),
    mu<-a+b*log.mass,
    a~dnorm(45,100),
    b~dnorm(0,1),
    sigma~dunif(0,1)
  ),data=dcc)
precis(m5.6)
mas.seq=seq(from=-3,to=5,length.out = 50)
pred.data=data.frame(log.mass=mas.seq)
mu=link(m5.6,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
lines(mas.seq,mu.mean)
shade(mu.PI,mas.seq)

#
m5.7=map(
  alist(
    kcal.per.g~dnorm(mu,sigma),
    mu<-a+bn*neocortex.perc+bm*log.mass,
    a~dnorm(45,100),
    bn~dnorm(0,1),
    bm~dnorm(0,1),
    sigma~dunif(0,1)
  ),data=dcc)
plot(precis(m5.7,corr=TRUE))

# counterfactual plots: holding body mass constant at its mean
mass.ave=mean(dcc$log.mass)

neo.seq=seq(from=50,to=80,length.out=50)
pred.data=data.frame(neocortex.perc=neo.seq,log.mass=mass.ave)
mu=link(m5.7,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)

plot(kcal.per.g~neocortex.perc,data=dcc,col=rangi2)
lines(neo.seq,mu.mean)
shade(mu.PI,neo.seq)

# counterfactual plots: holding neocortex (brain mass) constant at its mean
neo.ave=mean(dcc$neocortex.perc)
plot(kcal.per.g~log.mass,dcc)
log.mass.seq=seq(from=-3,to=5,length.out=50)
pred.data=data.frame(neocortex.perc=neo.ave,log.mass=log.mass.seq)
mu=link(m5.7,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
lines(log.mass.seq,mu.mean)
shade(mu.PI,log.mass.seq)
plot(neocortex.perc~log.mass,dcc,col=rangi2)

#
N=100
height=rnorm(N,10,2)
leg_prop=runif(N,0.4,0.5)
leg_left=leg_prop*height+rnorm(N,0,0.02)
leg_right=leg_prop*height+rnorm(N,0,0.02)
d=data.frame(height,leg_left,leg_right)

m5.8=map(
  alist(
    height~dnorm(mu,sigma),
    mu<-a+bl*leg_left+br*leg_right,
    a~dnorm(0,100),
    bl~dnorm(2,10),
    br~dnorm(2,10),
    sigma~dunif(0,10)
  ),data=d)
precis(m5.8)

precis_plot(precis(m5.8))
post=extract.samples(m5.8)
head(post)
plot(bl~br,post,col=col.alpha(rangi2,0.1),pch=16)
sum_blbr=post$bl+post$br
dens(sum_blbr,col=rangi2,lwd=2)

m5.9=map(
  alist(
    height~dnorm(mu,sigma),
    mu<-a+bl*leg_left,
    a~dnorm(10,100),
    bl~dnorm(2,10),
    sigma~dunif(0,10)
  ),data=d)
precis(m5.9)
precis_plot(precis(m5.9))

#
data("milk")
d=milk
m5.10=map(
  alist(
    kcal.per.g~dnorm(mu,sigma),
    mu<-a+bf*perc.fat,
    a~dnorm(0.6,10),
    bf~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d)

m5.11=map(
  alist(
    kcal.per.g~dnorm(mu,sigma),
    mu<-a+bl*perc.lactose,
    a~dnorm(0.6,10),
    bl~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d)

precis(m5.10,digits=3)
precis_plot(precis(m5.10))
precis(m5.11,digits=3)
precis_plot(precis(m5.11))
head(d)
plot(kcal.per.g~perc.fat,d,col=rangi2)
p.fat.seq=seq(from=-10,to=60,length.out = 100)
pred.data=data.frame(perc.fat=p.fat.seq)
mu=link(m5.10,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
lines(p.fat.seq,mu.mean)
shade(mu.PI,p.fat.seq)

m5.12=map(
  alist(
    kcal.per.g~dnorm(mu,sigma),
    mu<-a+bl*perc.fat+br*perc.lactose,
    a~dnorm(0.6,10),
    bl~dnorm(0,1),
    br~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d)
precis_plot(precis(m5.12))

pairs(~kcal.per.g+perc.fat+perc.lactose,data=d,col=rangi2)
cor(d$perc.fat,d$perc.lactose)

# number of plants
N=100

#simulate initial heights
h0=rnorm(N,10,2)

treatment=rep(0:1,each=N/2)
fungus=rbinom(N,size=1,prob=0.5-treatment*0.4)
h1=h0+rnorm(N,5-3*fungus)

d=data.frame(h0=h0,h1=h1,treatment=treatment,fungus=fungus)
m5.14=map(
  alist(
    h1~dnorm(mu,sigma),
    mu<-a+bh*h0+bt*treatment,
    a~dnorm(10,100),
    c(bh,bt)~dnorm(0,10),
    sigma~dunif(0,10)
  ),data=d)
precis(m5.14)

#
data("Howell1")
d=Howell1
str(d)

m5.15=map(
  alist(
    height~dnorm(mu,sigma),
    mu<-a+bm*male,
    a~dnorm(178,100),
    bm~dnorm(0,10),
    sigma~dunif(0,50)
  ),data=d)
precis(m5.15)
post=extract.samples(m5.15)
head(post)
mu.male=post$a+post$bm
PI(mu.male)

m5.15b=map(
  alist(
    height~dnorm(mu,sigma),
    mu<-af*(1-male)+am*male,
    af~dnorm(178,100),
    am~dnorm(178,100),
    sigma~dunif(0,150)
  ),data=d)
precis(m5.15b)

#5.4.2
library(rethinking)
data("milk")
d=milk
head(d)
unique(d$clade)

# 4 categories for clade: Ape, NWM, OWM, and S
# Create dummy variables for NWM, OWM and S. Ape=reference variable
d$clade.NWM=ifelse(d$clade=="New World Monkey",1,0)
d$clade.OWM=ifelse(d$clade=="Old World Monkey",1,0)
d$clade.S=ifelse(d$clade=="Strepsirrhine",1,0)

# model
m5.16=map(
  alist(
    kcal.per.g~dnorm(mu,sigma),
    mu<-a+b.NWM*clade.NWM+b.OWM*clade.OWM+b.S*clade.S,
    a~dnorm(0.6,10),
    c(b.NWM,b.OWM,b.S)~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d)
precis(m5.16)
post=extract.samples(m5.16)
head(post)
mu.ape=post$a
mu.NWM=post$a+post$b.NWM
mu.OWM=post$a+post$b.OWM
mu.S=post$a+post$b.S
precis(data.frame(mu.ape,mu.NWM,mu.OWM,mu.S))
diff.NWM.OWM=post$b.NWM-post$b.OWM
quantile(diff.NWM.OWM,prob=c(0.025,0.5,0.975))

d$clade.id=coerce_index(d$clade)
unique(d$clade.id)
head(d)
d1=d[complete.cases(d),]

m5.16_alt=map(
  alist(
    kcal.per.g~dnorm(mu,sigma),
    mu<-a[clade_id],
    a[clade_id]~dnorm(0.6,1),
    sigma~dunif(0,10)
  ),data=d)

# 5M1 (Spurious association: one predictor influences or highly correlates with outcome and another predictor)
N=100
x_real=rnorm(N)
x_spur=rnorm(N,x_real)
y=rnorm(N,x_real)
d=data.frame(y,x_real,x_spur)
pairs(d)

m.spur=map(
  alist(
    y~dnorm(mu,sigma),
    mu<-a+b1*x_real+b2*x_spur,
    a~dnorm(0,10),
    c(b1,b2)~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d
)
precis(m.spur)
precis_plot(precis(m.spur))

op=par(mfrow=c(2,2))
plot(y~x_real,d,col=rangi2)

# Counterfactual plots
# holding x_spur constant at its mean
x_spur.ave=mean(d$x_spur)
x_real.seq=seq(from=-3,to=4,length.out = 50)
pred.data=data.frame(x_real=x_real.seq,x_spur=x_spur.ave)
mu=link(m.spur,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
sim.y=sim(m.spur,data=pred.data)
sim.y.PI=apply(sim.y,2,PI,prob=0.89)

lines(x_real.seq,mu.mean)
shade(mu.PI,x_real.seq)
shade(sim.y.PI,x_real.seq)
abline(a=0,b=1)
mtext("Holding x_spur constat at its mean")

# holding x_real constant at its mean

x_real.ave=mean(d$x_real)
x_spur.seq=seq(from=-4,to=4,length.out=50)
pred.data=data.frame(x_real=x_real.ave,x_spur=x_spur.seq)
mu=link(m.spur,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
sim.y=sim(m.spur,data=pred.data)
sim.y.PI=apply(sim.y,2,PI,prob=0.89)
plot(y~x_spur,data=d,col=rangi2)
lines(x_spur.seq,mu.mean)
shade(mu.PI,x_spur.seq)
shade(sim.y.PI,x_spur.seq)

# 5M2 Masked relatioinship: Predictors are highly correlated with one another, and one is positivily correlated while
# another negatively correlated with an outcome variable
# InCorrect method: (i.e. in the masked relationship, fitting a bivariate mode (model with x1 or x2 with y separately
N=100
rho=0.7
x_pos=rnorm(N)
x_neg=rnorm(N,rho*x_pos,sqrt(1-rho^2))
y=rnorm(N,x_pos-x_neg)
d=data.frame(y,x_pos,x_neg)

m.mask=map(
  alist(
    y~dnorm(mu,sigma),
    mu<-a+b*x_pos,
    a~dnorm(0,10),
    b~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d)
precis_plot(precis(m.mask))

post=extract.samples(m.mask)
x_pos.seq=seq(from=-4,to=4,length.out=50)
mu=sapply(x_pos.seq,function(x_pos) post$a+post$b)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
sim.y=sapply(x_pos.seq,function(x_pos) rnorm(
  n=nrow(post),
  mean=post$a+post$b*x_pos,
  sd=post$sigma
))
sim.y.PI=apply(sim.y,2,PI,prob=0.89)
plot(y~x_pos,d,col=rangi2)
lines(x_pos.seq,mu.mean)
shade(mu.PI,x_pos.seq)
shade(sim.y.PI,x_pos.seq)

# Correct method: (i.e. usnig both variables captures a good relationship between the predictors and y)
N=100
rho=0.7
x_pos=rnorm(N)
x_neg=rnorm(N,rho*x_pos,sqrt(1-rho^2))
y=rnorm(N,x_pos-x_neg)
d=data.frame(y,x_pos,x_neg)
pairs(d)

m.mask=map(
  alist(
    y~dnorm(mu,sigma),
    mu<-a+b1*x_pos+b2*x_neg,
    a~dnorm(0,10),
    c(b1,b2)~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d
)
precis_plot(precis(m.mask))

op=par(mfrow=c(2,2))

# plot using both variables]
mu=link(m.mask)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
sim.y=sim(m.mask)
sim.y.PI=apply(mu,2,PI,prob=0.89)

plot(mu.mean~d$y,ylab="Predicted",xlab="Observed",col=rangi2)
abline(a=0,b=1,lty=2)

# plot x_post (x) and y (y) with holding x_neg constat at its mean
x_neg.ave=mean(d$x_neg)
x_pos.seq=seq(from=-4,to=4,length.out = 50)
pred.data=data.frame(x_pos=x_pos.seq,x_neg=x_neg.ave)
mu=link(m.mask,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
sim.y=sim(m.mask,data=pred.data)
sim.y.PI=apply(sim.y,2,PI,prob=0.89)
plot(y~x_pos,data=d,col=rangi2)
lines(x_pos.seq,mu.mean)
shade(mu.PI,x_pos.seq)
shade(sim.y.PI,x_pos.seq)

# plot x_neg with holding x_pos constant at its mean
x_pos.ave=mean(d$x_pos)
x_neg.seq=seq(from=-4,to=4,length.out=50)
pred.data=data.frame(x_pos=x_pos.ave,x_neg=x_neg.seq)
mu=link(m.mask,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
sim.y=sim(m.mask,data=pred.data)
sim.y.PI=apply(sim.y,2,PI,prob=0.89)
plot(y~x_neg,data=d,col=rangi2)
lines(x_neg.seq,mu.mean)
shade(mu.PI,x_neg.seq)
shade(sim.y.PI,x_neg.seq)

#5M4
data("WaffleDivorce")
d=WaffleDivorce
head(d)
unique(d$Location)
str(d)
d$pct_LDS <- c(0.75, 4.53, 6.18, 1, 2.01, 2.82, 0.43, 0.55, 0.38,
               0.75, 0.82, 5.18, 26.35, 0.44, 0.66, 0.87, 1.25, 0.77, 0.64, 0.81,
               0.72, 0.39, 0.44, 0.58, 0.72, 1.14, 4.78, 1.29, 0.61, 0.37, 3.34,
               0.41, 0.82, 1.48, 0.52, 1.2, 3.85, 0.4, 0.37, 0.83, 1.27, 0.75,
               1.21, 67.97, 0.74, 1.13, 3.99, 0.92, 0.44, 11.5 )

# standardize predctors
d$Marriage.s=(d$Marriage-mean(d$Marriage))/sd(d$Marriage)
d$MedianAge.s=(d$MedianAgeMarriage-mean(d$MedianAgeMarriage))/sd(d$MedianAgeMarriage)
d$pct_LDS.s=(d$pct_LDS-mean(d$pct_LDS))/sd(d$pct_LDS)

# fit model
m5M4=map(
  alist(
    Divorce~dnorm(mu,sigma),
    mu<-a+b1*Marriage.s+b2*MedianAge.s+b3*pct_LDS.s,
    a~dnorm(0,10),
    c(b1,b2,b3)~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d)
precis_plot(precis(m5M4))

str(d)
names(d)
d1=d[,c(7,15,16,17)]
head(d1)
pairs(d1)

#5H1
data("foxes")
d=foxes
head(d)
str(d)
pairs(d)

# weight ~ area
d$area.c=d$area-mean(d$area)
m5h1=map(
  alist(
    weight~dnorm(mu,sigma),
    mu<-a+b*area,
    a~dnorm(4,2),
    b~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d)
post=extract.samples(m5h1)
area.seq=seq(from=1,to=6,length.out = 50)
mu=sapply(area.seq,function(area) post$a+post$b)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
plot(weight~area,d,col=rangi2)
lines(area.seq,mu.mean)
shade(mu.PI,area.seq)

precis_plot(precis(m5h1));mtext("Weight~area")

# weight ~ groupsize
str(d)
plot(weight~groupsize,d,col=rangi2)
m5h1=map(
  alist(
    weight~dnorm(mu,sigma),
    mu<-a+b*groupsize,
    a~dnorm(4,2),
    b~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d)
precis(m5h1)
post=extract.samples(m5h1)
gsize.seq=seq(from=2,to=8,length.out = 50)
mu=sapply(gsize.seq,function(groupsize) post$a+post$b)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
lines(gsize.seq,mu.mean)
shade(mu.PI,gsize.seq)

precis_plot(precis(m5h1));mtext("Weight~groupsize")

#5H2
pairs(d[,c(3:5)])
m5h2=map(
  alist(
    weight~dnorm(mu,sigma),
    mu<-a+b1*groupsize+b2*area,
    a~dnorm(4,2),
    c(b1,b2)~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d)
precis_plot(precis(m5h2))


# holding groupsize constant at its mean
gsize.ave=mean(d$groupsize)
area.seq=seq(from=1,to=6,length.out=50)
pred.data=data.frame(groupsize=gsize.ave,area=area.seq)
mu=link(m5h2,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
sim.y=sim(m5h2,data=pred.data)
sim.y.PI=apply(sim.y,2,PI,prob=0.89)

# plot
plot(weight~area,d,col=rangi2)
lines(area.seq,mu.mean)
shade(mu.PI,area.seq)
shade(sim.y.PI,area.seq)
mtext("Holding groupsize constant")

#holiding area constant at its mean
area.ave=mean(d$area)
gsize.seq=seq(from=1,to=9,length.out = 50)
pred.data=data.frame(groupsize=gsize.seq,area=area.ave)
mu=link(m5h2,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
sim.y=sim(m5h2,data=pred.data)
sim.y.PI=apply(sim.y,2,PI,prob=0.89)

# plot
plot(weight~groupsize,d,col=rangi2)
lines(gsize.seq,mu.mean)
shade(mu.PI,gsize.seq)
shade(sim.y.PI,gsize.seq)
mtext("holding area constat")

#5H3
pairs(d)
m5h3.1=map(
  alist(
    weight~dnorm(mu,sigma),
    mu<-a+b1*avgfood+b2*groupsize,
    a~dnorm(4,2),
    c(b1,b2)~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d)
precis(m5h3.1,corr=TRUE)
precis_plot(precis(m5h3.1))

# holding avgfood at constant
avgfood.ave=mean(d$avgfood)
gsize.seq=seq(from=1,to=9,length.out = 50)
pred.data=data.frame(avgfood=avgfood.ave,groupsize=gsize.seq)
mu=link(m5h3.1,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
sim.y=sim(m5h3.1,data=pred.data)
sim.y.PI=apply(sim.y,2,PI,prob=0.89)
plot(weight~groupsize,d,col=rangi2);mtext("Holding avgfood constant")
lines(gsize.seq,mu.mean)
shade(mu.PI,gsize.seq)
shade(sim.y.PI,gsize.seq)

# Holding groupsize constant
gsize.ave=mean(d$groupsize)
avgfood.seq=seq(from=0.2,to=1.4,length.out = 50)
pred.data=data.frame(avgfood=avgfood.seq,groupsize=gsize.ave)
mu=link(m5h3.1,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
sim.y=sim(m5h3.1,data=pred.data)
sim.y.PI=apply(sim.y,2,PI,prob=0.89)
plot(weight~avgfood,d,col=rangi2);mtext("holding groupsize constant")
lines(avgfood.seq,mu.mean)
shade(mu.PI,avgfood.seq)
shade(sim.y.PI,avgfood.seq)

# posterior prediction
mu=link(m5h3.1)
mu.mean=apply(mu,2,mean) #predicted

plot(mu.mean~d$weight,col=rangi2,xlab="Observed",ylab="Predicted");mtext("Obs vs Pred Weight")
abline(a=0,b=1)

##
m5h3.2=map(
  alist(
    weight~dnorm(mu,sigma),
    mu<-a+b1*avgfood+b2*groupsize+b3*area,
    a~dnorm(4,2),
    c(b1,b2,b3)~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d)
precis(m5h3.2,corr=TRUE)
precis_plot(precis(m5h3.2,corr=TRUE))

# Holding avgfood and groupsize constant
avgfood.ave=mean(d$avgfood)
gsize.ave=mean(d$groupsize)
area.seq=seq(from=1,to=6,length.out=50)
pred.data=data.frame(avgfood=avgfood.ave,groupsize=gsize.ave,area=area.seq)
mu=link(m5h3.2,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
sim.y=sim(m5h3.2,data=pred.data)
sim.y.PI=apply(sim.y,2,PI,prob=0.89)
plot(weight~area,d,col=rangi2);mtext("Holiding avgfood & groupsize constant",cex=0.8)
lines(area.seq,mu.mean)
shade(mu.PI,area.seq)
shade(sim.y.PI,area.seq)

# Holding avgfood and area constant
avgfood.ave=mean(d$avgfood)
area.ave=mean(d$area)
gsize.seq=seq(from=1,to=9,length.out = 50)
pred.data=data.frame(avgfood=avgfood.ave,groupsize=gsize.seq,area=area.ave)
mu=link(m5h3.2,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
sim.y=sim(m5h3.2,data=pred.data)
sim.y.PI=apply(sim.y,2,PI,prob=0.89)
plot(weight~groupsize,d,col=rangi2);mtext("holding avgfood & area constant",cex=0.8)
lines(gsize.seq,mu.mean)
shade(mu.PI,gsize.seq)
shade(sim.y.PI,gsize.seq)

# Holding groupsize and area constant
gsize.ave=mean(d$groupsize)
area.ave=mean(d$area)
avgfood.seq=seq(from=0.2,to=1.4,length.out = 50)
pred.data=data.frame(avgfood=avgfood.seq,groupsize=gsize.ave,area=area.ave)
mu=link(m5h3.2,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
sim.y=sim(m5h3.2,data=pred.data)
sim.y.PI=apply(sim.y,2,PI,prob=0.89)
plot(weight~avgfood,d,col=rangi2);mtext("holding groupsize & area constant",cex=0.8)
lines(avgfood.seq,mu.mean)
shade(mu.PI,avgfood.seq)
shade(sim.y.PI,avgfood.seq)

# obs vs predicted plot
mu=link(m5h3.2)
mu.mean=apply(mu,2,mean)
plot(mu.mean~d$weight,col=rangi2,xlab="Observed",ylab="Predicted");mtext("Obs vs Pred Weight")
abline(a=0,b=1)
