nm0<- 81; nm1<- 31; nm2<-71; nOmega=184

Treatment=c("null","a","b")
delta=15 # smallest interval between two visits
Time=c(delta,2*delta,4*delta)
nd=length(Treatment)*length(Time)

Decisions=matrix(ncol=2,nrow=nd)
Decisions[,1]<-rep(Treatment,length(Time))
Decisions[,2]<-rep(Time,each=3)


evalGauss<-function(epsilon)
{
	return( 1/sqrt(2*pi*sigma2)*exp(-epsilon^2/2/sigma2))
}


distblock<-function(Filt,Theta)
{
		T0=Theta[1:nm0]
		T1=Theta[(nm0+1):(nm0+nm1)]
		T2=Theta[(nm0+nm1+1):(nm0+nm1+nm2)]
		p0=sum(T0); p1=sum(T1); p2=sum(T2)
		return(abs(p0-Filt[nOmega])+abs(p1-Filt[nOmega+1])+abs(p2-Filt[nOmega+2])+sqrt(sum((Filt[1:(nOmega-1)]-Theta)^2)))	
}

NNeighborBlockT<-function(Theta)
{
		if (sum(Theta)==0) return(c(rep(0,nOmega-1),ngamma))

		dist<-apply(Gdist,2, distblock, Theta=Theta)
		ell=which.min(dist)
		return (ell)
}


Update_filter<-function(theta,d,y)
{
				dind=(which(Time==d[2])-1)*3+which(Treatment==d[1])
				PTemp<-t(P[1:(nOmega-1),1:(nOmega-1),dind])%*%theta
				eG<-evalGauss(y-xval)
				Num=PTemp*eG
				Den=eG%*%PTemp
				Thetanew=Num/as.vector(Den)
				return(Thetanew)
}

Get_policy<-function(theta,t)
{
	NT=NNeighborBlockT(theta)
	indvisit=t/delta
	decision<-Decint[indvisit+1,NT]
	Policy<-c(Decisions[decision,1],Decisions[decision,2])
	return(Policy)
}
