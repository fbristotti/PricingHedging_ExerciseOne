brownm <- function(n, T,x0=0){
	sigma = sqrt(T / n)
	sample = c(x0, rnorm(n-1,0,sigma))
	return(cumsum(sample))
}



brownbridge.recursive <- function(inf,sup,sample){
	if(inf+1>sup){
		return(sample)
	}
	
	mid = floor((inf+sup)/2)
	if(mid==inf | mid==sup){
		return(sample)
	}

	inf_value = sample[inf]
	sup_value = sample[sup]

	mean = ((sup-mid)*sample[inf] + (mid-inf)*sample[sup])/(sup-inf)
	variance = sqrt(((mid-inf)*(sup-mid))/(sup-inf))

	sample[mid] = rnorm(1,mean,variance)
	sample = brownbrecursive(inf,mid,sample)
	sample = brownbrecursive(mid,sup,sample)
	return(sample)
}

brownbridge <- function(n,T,x0=0){

	mean = 0
	sigma = c(rep(0,n-1),1)
	mean = rep(0,n)
	sample = c(x0,rep(0,n-2),rnorm(1,mean,sigma))
	ini = 1
	fim = n
	
	sample =  brownbridge.recursive(1,length(sample),sample)
	
	return(sample)
}

mytest <- function(n){
	return(list(rep(0,n),rep(1,n))
}

plot(brownbridge(100,1),type="l",col="blue",ylab="path")
lines(brownm(100,1),col="red")


r=0
q=0
S_0=100
K=100
H=85
t=256
vol=.2
daysInYear=256
dailyVol = vol*sqrt(1/252)
normVector = rnorm(t,0,1)
normVolVector=normVector*sqrt(dailyVol)

plot(normVector, col="red")
points(normVolVector, col="blue")
