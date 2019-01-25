p=list()
p$A=c(0,0,10,0,0)
p$B=c(0,1,8,1,0)
p$C=c(0,2,6,2,0)
p$D=c(1,2,4,2,1)
p$E=c(2,2,2,2,2)

p_norm=lapply(p,function(q) q/sum(q))
H=sapply(p_norm,function(q) -sum(ifelse(q==0,0,q*log(q))))

ways=c(1,90,1260,37800,113400)
logwayspp=log(ways)/10

p=0.7
A=c((1-p)^2,p*(1-p),(1-p)*p,p^2)
entropy=-sum(A*log(A))

sim.p=function(G=1.4){
  x123=runif(3)
  x4=((G)*sum(x123-x123[2]-x123[3])/(2-G))
  z=sum(c(x123,x4))
  p=c(x123,x4)/z
  list(H=-sum(p*log(p)),p=p)
}
library(rethinking)
H=replicate(1e5,sim.p(1.4))
op=par(mfrow=c(2,2))
dens(as.numeric(H[1,]),adj=0.1)

curve(dgamma(x,5,20))
curve(dnorm(x,10,3))
?dlogis
curve(dlogis(x,5,1),xlim=c(-1,1))
