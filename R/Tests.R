brownm <- function(n, T,x0=0){
	sigma = sqrt(T / n)
	sample = c(x0, sigma * rnorm(n-1,0,1))
	return(cumsum(sample))
}

brownbridge.recursive <- function(inf,sup,data){
	if(inf+1>sup){
		return(data)
	}
	
	mid = floor((inf+sup)/2)
	if(mid==inf | mid==sup){
		return(data)
	}

	infval = data$sample[inf]
	supval = data$sample[sup]

	mean = ((sup-mid)*infval + (mid-inf)*supval)/(sup-inf)
	data$mean[mid] = mean
	sigma = (((sup-mid)*(mid-inf))/(sup-inf)) / length(data$sample)

	data$sigma[mid] = sigma

	data$sample[mid] = mean + sqrt(sigma) * rnorm(1,0,1)
	data = brownbridge.recursive(inf,mid,data)
	data = brownbridge.recursive(mid,sup,data)
	return(data)
}

brownbridge <- function(n,T,x0=0){

	sigma = c(rep(0,n-1),1)
	mean = rep(0,n)
	sample = c(x0,rep(0,n-2),rnorm(1,0,1))
	data = list("sample"=sample,"mean"=mean,"sigma"=sigma)

	ini = 1
	fim = n
	
	data =  brownbridge.recursive(1,length(sample),data)
	
	return(data)
}

mytest <- function(n){
	return(list(rep(0,n),rep(1,n))
}

n = 1000
data=brownbridge(n,1)
plot(data$sample,type="l",col="blue",ylab="path")
lines(brownm(n,1),col="red")
data$sample
data$mean
data$sigma

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
