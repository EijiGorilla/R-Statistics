library(rethinking)
data("milk")
d=milk
rm(list=ls())
d=d[complete.cases(d),]

d$neocortex=d$neocortex.perc/100

a.start=median(d$kcal.per.g)
sigma.start=log(sd(d$kcal.per.g))

op=par(mfrow=c(2,2))

m6.11=map(
  alist(
    kcal.per.g~dnorm(a,exp(log.sigma))
  ),data=d,start=list(a=a.start,log.sigma=sigma.start))

m6.12=map(
  alist(
    kcal.per.g~dnorm(mu,exp(log.sigma)),
    mu<-a+bn*neocortex
  ),data=d,start=list(a=a.start,bn=0,log.sigma=sigma.start))

m6.13=map(
  alist(
    kcal.per.g~dnorm(mu,exp(log.sigma)),
    mu<-a+bm*log(mass)
  ),data=d,start=list(a=a.start,bm=0,log.sigma=sigma.start))

m6.14=map(
  alist(
    kcal.per.g~dnorm(mu,exp(log.sigma)),
    mu<-a+bn*neocortex+bm*log(mass)
  ),data=d,start=list(a=a.start,bn=0,bm=0,log.sigma=sigma.start))

milk.models=compare(m6.11,m6.12,m6.13,m6.14)
source("C:\\Users\\oc3512\\Documents\\StatisticalRethinking\\compare_plot_Rethinking.R")
plot(milk.models,SE=TRUE,dSE=TRUE)

coeftab_plot(coeftab(m6.11,m6.12,m6.13,m6.14))

# Counterfactual plots with m6.14
nc.seq=seq(from=0.5,to=0.8,length.out = 50)
mass.ave=mean(d$mass)
pred.data=data.frame(neocortex=nc.seq,mass=mass.ave)
mu=link(m6.14,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
sim.y=sim(m6.14,data=pred.data)
sim.y.PI=apply(sim.y,2,PI,prob=0.89)

plot(kcal.per.g~neocortex,data=d,col=rangi2)
mtext("Holding mass constant")
lines(nc.seq,mu.mean)
shade(mu.PI,nc.seq)

# compute model averaged posteior predictions
milk.ensemble=ensemble(m6.11,m6.12,m6.13,m6.14,data=pred.data)
str(milk.ensemble)
mu=apply(milk.ensemble$link,2,mean)
mu.PI=apply(milk.ensemble$link,2,PI,prob=0.89)
lines(nc.seq,mu)
shade(mu.PI,nc.seq)

#
data("rugged")
d=rugged
str(d)
d$log_gdp=log(d$rgdppc_2000)

# extract countries with GDP data
dd=d[complete.cases(d$rgdppc_2000),]

# split Africa and not-Africa
d.A1=dd[dd$cont_africa==1,] #Africa
d.A0=dd[dd$cont_africa==0,] #non-African

m7.1=map(
  alist(
    log_gdp~dnorm(mu,sigma),
    mu<-a+bR*rugged,
    a~dnorm(8,100),
    bR~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d.A1)

m7.2=map(
  alist(
    log_gdp~dnorm(mu,sigma),
    mu<-a+bR*rugged,
    a~dnorm(8,100),
    bR~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d.A0
)
plot(log_gdp~rugged,d.A1,col=rangi2)
rug.seq=seq(from=0,to=7,length.out = 50)
pred.data=data.frame(rugged=rug.seq)
mu=link(m7.1,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
lines(rug.seq,mu.mean)
shade(mu.PI,rug.seq)


# plot non-African nations
plot(log_gdp~rugged,data=d.A0,col=rangi2)
rug.seq=seq(from=0,to=7,length.out = 50)
pred.data=data.frame(rugged=rug.seq)
mu=link(m7.2,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
lines(rug.seq,mu.mean)
shade(mu.PI,rug.seq)

# Adding dummy variable
m7.3=map(
  alist(
    log_gdp~dnorm(mu,sigma),
    mu<-a+bR*rugged,
    a~dnorm(8,100),
    bR~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=dd)
  
m7.4=map(
  alist(
    log_gdp~dnorm(mu,sigma),
    mu<-a+bR*rugged+bA*cont_africa,
    a~dnorm(8,100),
    c(bR,bA)~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=dd)

plot(compare(m7.3,m7.4))

# plot m7.4
# fixing cont_africa==1
cont_fix=1
rug.seq=seq(from=0,to=7,length.out = 50)
pred.data=data.frame(rugged=rug.seq,cont_africa=cont_fix)
mu=link(m7.4,data=pred.data)
mu.mean.africa=apply(mu,2,mean)
mu.PI.africa=apply(mu,2,PI,prob=0.89)

# fixing cont_africa==0
cont_fix=0
pred.data=data.frame(rugged=rug.seq,cont_africa=cont_fix)
mu=link(m7.4,data=pred.data)
mu.mean.nafrica=apply(mu,2,mean)
mu.PI.nafrica=apply(mu,2,PI,prob=0.89)

# plot
plot(log_gdp~rugged,data=dd,type="n")
points(log_gdp~rugged,data=d.A1,col=rangi2) #African nations
points(log_gdp~rugged,data=d.A0,col=col.alpha("black",0.5)) #Non-African

# African
lines(rug.seq,mu.mean.africa,col=rangi2)
shade(mu.PI.africa,rug.seq,col=col.alpha(rangi2,0.5))

# non-African
lines(rug.seq,mu.mean.nafrica,col=col.alpha("black",0.5))
shade(mu.PI.nafrica,rug.seq,col=col.alpha("black",0.5))

# Interaction term
# We believe that the relationship between ruggedness and GDP varies by whether or not a nation is in Africa
m7.5=map(
  alist(
    log_gdp~dnorm(mu,sigma),
    mu<-a+gamma*rugged+bA*cont_africa,
    gamma<-bR+bAR*cont_africa,
    a~dnorm(8,100),
    c(bA,bR,bAR)~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=dd)
compare(m7.3,m7.4,m7.5)
plot(compare(m7.3,m7.4,m7.5))
precis(m7.5)

# fix cont_africa==1
cont_fix=1
rug.seq=seq(from=0,to=7,length.out=50)
pred.data=data.frame(rugged=rug.seq,cont_africa=cont_fix)
mu=link(m7.5,data=pred.data)
mu.mean.africa=apply(mu,2,mean)
mu.PI.africa=apply(mu,2,PI,prob=0.89)

# fix cont_africa==0
cont_fix=0
pred.data=data.frame(rugged=rug.seq,cont_africa=cont_fix)
mu=link(m7.5,data=pred.data)
mu.mean.nafrica=apply(mu,2,mean)
mu.PI.nafrica=apply(mu,2,PI,prob=0.89)

# plot African nations
plot(log_gdp~rugged,dd,col=rangi2,type="n")
mtext("African nations")
points(log_gdp~rugged,data=d.A1,col=rangi2)
lines(rug.seq,mu.mean.africa,col=rangi2)
shade(mu.PI.africa,rug.seq,col=col.alpha(rangi2,0.2))

# plot Non-African nations
plot(log_gdp~rugged,dd,col=col.alpha("black",0.5))
mtext("Non-African nations")
lines(rug.seq,mu.mean.nafrica)
shade(mu.PI.nafrica,rug.seq,col=col.alpha("black",0.2))

# Effect of ruggedness on GDP in the interaction context (i.e. gamma is a coefficient of R)
post=extract.samples(m7.5)
# note: gamma<-bR+bAR*cont_africa
gamma.Africa=post$bR+post$bAR*1
gamma.nonAfrica=post$bR+post$bAR*0

mean(gamma.Africa)
mean(gamma.nonAfrica)

dens(gamma.Africa,xlim=c(-0.5,0.6),ylim=c(0,5.5),xlab="gamma",col=rangi2)
dens(gamma.nonAfrica,add=TRUE)
diff=gamma.Africa-gamma.nonAfrica
n.diff=length(diff[diff<0])
n.diff/length(diff)

# Influence of being in Africa on GDP depends on ruggedness
m7.6=map(
  alist(
    log_gdp~dnorm(mu,sigma),
    mu<-a+bR*rugged+gamma*cont_africa,
    gamma<-bAR*rugged+bA,
    a~dnorm(8,100),
    c(bR,bAR,bA)~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=dd)

# min rugged
rugged.min=min(dd$rugged)
pred.data=data.frame(rugged=rugged.min,cont_africa=0:1)
mu.min.africa=link(m7.6,data=pred.data)
mu.min.africa.mean=apply(mu.min.africa,2,mean)
mu.min.africa.PI=apply(mu.min.africa,2,PI,prob=0.89)


# max rugged
rugged.max=max(dd$rugged)
pred.data=data.frame(rugged=rugged.max,cont_africa=0:1)
mu.max.africa=link(m7.6,data=pred.data)
mu.max.africa.mean=apply(mu.max.africa,2,mean)
mu.max.africa.PI=apply(mu.max.africa,2,PI,prob=0.89)


med.r=median(dd$rugged)
ox=ifelse(dd$rugged>med.r,0.05,-0.05)
ox
plot(dd$cont_africa+ox,log(dd$rgdppc_2000),
     col=ifelse(dd$rugged>med.r,rangi2,"black"),
     xlime=c(-0.25,1.25),xaxt="n",ylab="log GDP year 2000",
     xlab="Continent")
axis(1,at=c(0,1),labels=c("Other","Africa"))
lines(0:1,mu.min.africa.mean,col=rangi2)
shade(mu.min.africa.PI,0:1,col=col.alpha(rangi2,0.2))
lines(0:1,mu.max.africa.mean)
shade(mu.max.africa.PI,0:1,col=col.alpha("black",0.2))

# 7.3.1 Continuous interaction
library(rethinking)
data("tulips")
d=tulips
str(d)
pairs(d)

m7.6=map(
  alist(
    blooms~dnorm(mu,sigma),
    mu<-a+bW*water+bS*shade,
    a~dnorm(0.100),
    c(bW,bS)~dnorm(0,100),
    sigma~dunif(0,100)
  ),data=d,method="Nelder-Mead",control=list(maxit=1e4))

m7.7=map(
  alist(
    blooms~dnorm(mu,sigma),
    mu<-a+bW*water+bS*shade+bWS*water*shade,
    a~dnorm(0,100),
    c(bW,bS,bWS)~dnorm(0,100),
    sigma~dunif(0,100)
  ),data=d,method="Nelder-Mead",control=list(maxit=1e4))
coeftab(m7.6,m7.7)

source("C:\\Users\\oc3512\\Documents\\StatisticalRethinking\\compare_plot_Rethinking.R")

op=par(mfrow=c(2,2))
plot(compare(m7.6,m7.7))
precis_plot(precis(m7.6))
precis_plot(precis(m7.7))

# center predictor variables and re-estimate
d$shade.c=d$shade-mean(d$shade)
d$water.c=d$water-mean(d$water)

m7.8=map(
  alist(
    blooms~dnorm(mu,sigma),
    mu<-a+bW*water.c+bS*shade.c,
    a~dnorm(130,100),
    c(bW,bS)~dnorm(0,100),
    sigma~dunif(0,100)
  ),data=d,method="Nelder-Mead",start=list(a=mean(d$blooms,bW=0,bS=0,sigma=sd(d$blooms)))
)

m7.9=map(
  alist(
    blooms~dnorm(mu,sigma),
    mu<-a+bW*water.c+bS*shade.c+bWS*water.c*shade.c,
    a~dnorm(130,100),
    c(bW,bS,bWS)~dnorm(0,100),
    sigma~dunif(0,100)
  ),data=d,method="Nelder-Mead",start=list(a=mean(d$blooms),bW=0,bS=0,bWS=0,sigma=sd(d$blooms))
)

coeftab(m7.8,m7.9)
compare(m7.8,m7.9)
precis_plot(precis(m7.8))
precis_plot(precis(m7.9))
coef(m7.7)
coef(m7.9)

precis(m7.9)
precis_plot(precis(m7.9))

op=par(mfrow=c(2,3))

# No interaction: 
shade.seq=-1:1
water.seq=-1:1

for(i in -1:1){
  dn=d[d$water.c==i,]
  plot(blooms~shade.c,dn,col=rangi2,main=paste("water.c=",i,sep=""),
       xlab="shade (centered)",ylab="blooms",xaxp=c(-1,1,2),ylim=c(0,362))
  pred.data=data.frame(water.c=i,shade.c=shade.seq)
  mu=link(m7.8,data=pred.data)
  mu.mean=apply(mu,2,mean)
  mu.PI=apply(mu,2,PI,prob=0.89)
  lines(shade.seq,mu.mean)
  shade(mu.PI,shade.seq)
}


# Interaction: loop over values of water.c and plot predictions
shade.seq=-1:1
water.seq=-1:1

for(i in -1:1){
  dn=d[d$water.c==i,]
  plot(blooms~shade.c,dn,col=rangi2,main=paste("water.c=",i,sep=""),
       xlab="shade (centered)",ylab="blooms",xaxp=c(-1,1,2),ylim=c(0,362))
  pred.data=data.frame(water.c=i,shade.c=shade.seq)
  mu=link(m7.9,data=pred.data)
  mu.mean=apply(mu,2,mean)
  mu.PI=apply(mu,2,PI,prob=0.89)
  lines(shade.seq,mu.mean)
  shade(mu.PI,shade.seq)
}

# Practice
#7M1
# The expected change in the influence of water when shade increases by one unit has no effect on the size
# of blossomm under hot environment. The reverse is true: the expected change in the influence of shade
# when water level increases by one unit has also no effect on the size of blossoms under the hot environment.
# In other words, neither change in the level of  water nor shade has impacts on the size of blossoms under the hot environment.

# 7M2
# First add observations where blooms become zero when temperature is hot with other combinations of shade and water experiments identical
# fit the three-way interaction term with shade.c, water.c, and temperature.

# 7M3
# Population size of raven = outcome variable
# the presence of wolves = predictor variable
# the amount of food available = the other predictor
# In the absence of wolves, biological interaction could not be linear, as the amount of food available is uncertain
# yet, in the presence of wolves, the interaction could be linear, as food consumed becomes high with more food available.

#7H1
data("tulips")
d=tulips
head(d)

# center water and shade continuous variavbles
d$water.c=d$water-mean(d$water)
d$shade.c=d$shade-mean(d$shade)

# Dummy variables for bed
d$bed.b=ifelse(d$bed=="b",1,0)
d$bed.c=ifelse(d$bed=="c",1,0)
d$bed.i=coerce_index(d$bed)
str(d)

# fit a model
m7h1=map(
  alist(
    blooms~dnorm(mu,sigma),
    mu<-a[bed.i]+bW*water.c+bS*shade.c+bWS*water.c*shade.c,
    a~dnorm(0,100),
    c(bW,bS,bWS)~dnorm(0,100),
    a[bed.i]~dnorm(0,100),
    sigma~dunif(0,100)
  ),data=d,method="Nelder-Mead",start=list(a=mean(d$blooms),bW=0,bS=0,bWS=0,sigma=sd(d$blooms)),control=list(maxit=1e4)
)
precis(m7h1,depth=2)
precis_plot(precis(m7h1,depth=2))
compare(m7.9,m7h1)
plot(compare(m7.9,m7h1))
coeftab(m7.9,m7h1)
post=extract.samples(m7h1)
str(post)

dens(post$a[,1],xlim=c(40,200))
dens(post$a[,2],col=rangi2,add=TRUE)
dens(post$a[,3],col=col.alpha("green",0.9),add=TRUE)

#7H3

data(rugged)
d <- rugged
d <- d[complete.cases(d$rgdppc_2000),]
d$log_gdp <- log(d$rgdppc_2000)
d$rugged.c <- d$rugged - mean(d$rugged)

# (a)fit the model with and without Seychelles country
# with Seychelles
m7h3.s=map(
  alist(
    log_gdp~dnorm(mu,sigma),
    mu<-a+bRR*rugged.c+bA*cont_africa+bAR*cont_africa*rugged.c,
    a~dnorm(0,10),
    c(bRR,bA,bAR)~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d
)

# without Seychelles
m7h3=map(
  alist(
    log_gdp~dnorm(mu,sigma),
    mu<-a+bRR*rugged.c+bA*cont_africa+bAR*cont_africa*rugged.c,
    a~dnorm(0,10),
    c(bRR,bA,bAR)~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d[(d$country != "Seychelles"),]
)

# Tripletych plot
precis_plot(precis(m7h3.s))
precis_plot(precis(m7h3))

# with seychelles
par(mfrow=c(2,2))
tripletych.plot=function(model,text){
  cont.fix=0:1
  
  for(i in cont.fix){
    dt=d[d$cont_africa==i,]
    rugged.seq=seq(from=-1.5,to=5,length.out = 50)
    pred.data=data.frame(rugged.c=rugged.seq,cont_africa=i)
    mu=link(model,data=pred.data)
    mu.mean=apply(mu,2,mean)
    mu.PI=apply(mu,2,PI,prob=0.89)
    plot(log_gdp~rugged.c,dt,col=ifelse(i==0,col.alpha("black",0.9),rangi2),
         main=ifelse(i==0,"Non-Africa","Africa"),
         sub=ifelse(text=="Seychelles","Seychelles",""))
    lines(rugged.seq,mu.mean,col=ifelse(i==0,col.alpha("black",0.9),rangi2))
    shade(mu.PI,rugged.seq,col=ifelse(i==0,col.alpha("black",0.2),col.alpha(rangi2,0.2)))
  }
}

tripletych.plot(m7h3.s,"Seychelles")
tripletych.plot(m7h3,"")

#  (b)
# Yes, the effect of ruggedness depend on continent, regardless of the presence of Seychelles
# the expected relationship between ruggedness and log GDP changed little between models with and without Seychelles.

# (c)
m1=map(
  alist(
    log_gdp~dnorm(mu,sigma),
    mu<-a+bR*rugged.c,
    a~dnorm(0,5),
    bR~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d[(d$country != "Seychelles"),])

m2=map(
  alist(
    log_gdp~dnorm(mu,sigma),
    mu<-a+bR*rugged.c+bA*cont_africa,
    a~dnorm(0,5),
    c(bR,bA)~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d[(d$country != "Seychelles"),])

m3=map(
  alist(
    log_gdp~dnorm(mu,sigma),
    mu<-a+bR*rugged.c+bA*cont_africa+bAR*cont_africa*rugged.c,
    a~dnorm(0,5),
    c(bR,bA,bAR)~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d[(d$country != "Seychelles"),])

source("C:\\Users\\oc3512\\Documents\\StatisticalRethinking\\compare_plot_Rethinking.R")
compare(m1,m2,m3)
plot(compare(m1,m2,m3))

# model-averaged predictions = ensemble


cont.fix=0:1
for(i in cont.fix){
  dt=d[d$cont_africa==i,]
  rugged.ave=seq(from=-1.5,5,length.out = 50)
  pred.data=data.frame(rugged.c=rugged.ave,cont_africa=i)
  mu=ensemble(m1,m2,m3,data=pred.data)
  mu.mean=apply(mu$link,2,mean)
  mu.PI=apply(mu$link,2,PI,prob=0.89)
  plot(log_gdp~rugged.c,dt,col=ifelse(i==0,col.alpha("black",0.4),rangi2),
       main=ifelse(i==0,"Non-Africa","Africa"),
       sub="Without Seychelles")
  lines(rugged.ave,mu.mean,col=ifelse(i==0,col.alpha("black",0.9),rangi2))
  shade(mu.PI,rugged.ave,col=ifelse(i==0,col.alpha("black",0.2),col.alpha(rangi2,0.2)))
}


#7H4
# Hypothesis: language diversity is partly a product of food security.
# Outcome variable = the number of languages per capita

data("nettle")
d=nettle

str(d)
d$lang.per.cap=d$num.lang/d$k.pop
d$log_lang.per.cap=log(d$lang.per.cap)
d$log_area=log(d$area)
d$m.grow.season.c=d$mean.growing.season-mean(d$mean.growing.season)
pairs(d[,c(1,5,7,9,10,11)])

# (a)
m_a=map(
  alist(
    log_lang.per.cap~dnorm(mu,sigma),
    mu<-a+bG*m.grow.season.c+bA*log_area,
    a~dnorm(0,10),
    c(bG,bA)~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d)
precis(m_a)
precis_plot(precis(m_a))

# Counterfactual plots
# holding log_area constant at its log mean
log_area.ave=mean(d$log_area)
grow.s.seq=seq(from=-8,to=6,length.out = 50)
pred.data=data.frame(m.grow.season.c=grow.s.seq,log_area=log_area.ave)
mu=link(m_a,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
plot(log_lang.per.cap~m.grow.season.c,d,col=rangi2)
mtext("Holding log_area constant")
lines(grow.s.seq,mu.mean)
shade(mu.PI,grow.s.seq)

# holding growing season constant at its mean (i.e., mean = 0)
log_area.seq=seq(from=-12,to=17,length.out = 50)
grow.s.ave=mean(d$m.grow.season.c)
pred.data=data.frame(m.grow.season.c=grow.s.ave,log_area=log_area.seq)
mu=link(m_a,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
plot(log_lang.per.cap~log_area,d,col=rangi2)
mtext("holding m.growing.season.c constant")
lines(log_area.seq,mu.mean)
shade(mu.PI,log_area.seq)

# Prediction Residual plots
# observed vs predicted
mu=link(m_a)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
plot(d$log_lang.per.cap~mu.mean,col=rangi2,xlab="Observed",ylab="Predicted")
mtext("Log_lang.per.cap")
abline(a=0,b=1)

# residuals vs other predictors (missing variable?)
resid=d$log_lang.per.cap-mu.mean
names(d)
plot(resid~log(d$num.stations),col=rangi2);abline(h=0,lty=2)
plot(resid~d$sd.growing.season,col=rangi2);abline(h=0,lty=2)
plot(resid~mu.mean,xlab="Predicted")

#(b)
str(d)
d$sd.grow.season.c=d$sd.growing.season-mean(d$sd.growing.season)

m_b=map(
  alist(
    log_lang.per.cap~dnorm(mu,sigma),
    mu<-a+bS*sd.grow.season.c+bA*log_area,
    a~dnorm(0,5),
    bS~dnorm(0,1),
    bA~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d)

precis(m_b)
precis_plot(precis(m_b))

# Counterfactual plots
# holding log_area constant
range(d$sd.grow.season.c)
log_area.ave=mean(d$log_area)
sd.g.season.seq=seq(from=-2,to=5,length.out = 50)
pred.data=data.frame(sd.grow.season.c=sd.g.season.seq,log_area=log_area.ave)
mu=link(m_b,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
plot(log_lang.per.cap~sd.grow.season.c,d,col=rangi2);mtext("holding log_area constant")
lines(sd.g.season.seq,mu.mean)
shade(mu.PI,sd.g.season.seq)

# holding sd growing seasn constant
log_area.seq=seq(from=-12,to=17,length.out = 50)
sd.g.season.ave=mean(d$sd.grow.season.c)
pred.data=data.frame(sd.grow.season.c=sd.g.season.seq,log_area=log_area.ave)
mu=link(m_b,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
plot(log_lang.per.cap~log_area,d,col=rangi2);mtext("holding sd_growing season.c constant")
lines(log_area.seq,mu.mean)
shade(mu.PI,log_area.seq)

# (c)
m_c=map(
  alist(
    log_lang.per.cap~dnorm(mu,sigma),
    mu<-a+bG*m.grow.season.c+bS*sd.grow.season.c+bGS*m.grow.season.c*sd.grow.season.c,
    a~dnorm(0,5),
    c(bG,bS,bGS)~dnorm(0,1),
    sigma~dunif(0,10)
  ),data=d)

precis_plot(precis(m_c))

plot(d$sd.grow.season.c~d$m.grow.season.c,col=rangi2)

# holding m.grow.season constant
m.grow.s.ave=mean(d$m.grow.season.c)
sd.g.season.seq=seq(from=-2,to=5,length.out = 50)
pred.data=data.frame(m.grow.season.c=m.grow.s.ave,sd.grow.season.c=sd.g.season.seq)
mu=link(m_c,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
plot(log_lang.per.cap~sd.grow.season.c,d,col=rangi2);mtext("holding mean growing season c constant")
lines(sd.g.season.seq,mu.mean)
shade(mu.PI,sd.g.season.seq)

# holding sd growins season constant
sd.g.season.ave=mean(d$sd.grow.season.c)
m.grow.s.seq=seq(from=-8,to=6,length.out = 50)
pred.data=data.frame(m.grow.season.c=m.grow.s.seq,sd.grow.season.c=sd.g.season.ave)
mu=link(m_c,data=pred.data)
mu.mean=apply(mu,2,mean)
mu.PI=apply(mu,2,PI,prob=0.89)
plot(log_lang.per.cap~m.grow.season.c,d,col=rangi2)
mtext("holding sd growing season c constant")
lines(m.grow.s.seq,mu.mean)
shade(mu.PI,m.grow.s.seq)
