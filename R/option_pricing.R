brownm <- function(n, T){
  sigma = sqrt(T / n)
  sample = c(0, sigma * rnorm(n-1,0,1))
  return(cumsum(sample))
}

brownbridge.recursive <- function(inf,sup,data){
  if(inf+1>=sup){
    return(data)
  }
  
  mid = floor((inf+sup)/2)
  
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

brownbridge <- function(n,T){
  
  sigma = c(rep(0,n-1),1)
  mean = rep(0,n)
  sample = c(0,rep(0,n-2),rnorm(1,0,1))
  data = list("sample"=sample,"mean"=mean,"sigma"=sigma)
  
  ini = 1
  fim = n
  
  data =  brownbridge.recursive(1,length(sample),data)
  
  return(data)
}

brownbridge_barrier.recursive <- function(inf,sup,data,S,H,r,sigma){
  if(inf+1>=sup){
    return(data)
  }
  
  mid = floor((inf+sup)/2)
  
  infval = data$sample[inf]
  supval = data$sample[sup]
  
  mean = ((sup-mid)*infval + (mid-inf)*supval)/(sup-inf)
  sigma = (((sup-mid)*(mid-inf))/(sup-inf)) / length(data$sample)
  data$sample[mid] = mean + sqrt(sigma) * rnorm(1,0,1)
  deltaT = (mid-inf)/length(data$prices)
  S_mid = data$prices[inf] * exp((r-sigma^2/2)*(deltaT)+sigma*sqrt(deltaT)*data$sample[mid])
  data$hit = S_mid <= H
  
  if(data$hit){
    return(data)
  }
  else{
    data = brownbridge.recursive(inf,mid,data)
    if(data$hit){
      return(data)
    }
    else
    {
      data = brownbridge.recursive(mid,sup,data)
    }
  }
  return(data)
}

brownbridge_barrier <- function(n,T,S,H,r,sigma){
  
  sample = c(0,rep(0,n-2),rnorm(1,0,1))
  S_T = S * exp((r-sigma^2/2)*(T)+sigma*sqrt(T)*rnorm(1,0,T))
  prices = c(S,rep(0,n-2),S_T)
  hit = S_T <= H
  data = list("sample"=sample,"prices"=prices,"hit"=hit)
  
  if(touche = 1){
    return(data)
  }
  else{
    ini = 1
    fim = n
    data =  brownbridge.recursive(1,length(sample),data)
    return(data)
  }
}

preco_bs <- function(S, K, r, T, sigma,optionType) {
    
  sigmaSqrtT = sigma*sqrt(T)
  d1 <- (log(S/K)+(r+sigma^2/2)*T)/(sigmaSqrtT)
  d2 <- d1 - sigmaSqrtT
  
  if(optionType=="call")
    return(S*pnorm(d1) - K*exp(-r*T)*pnorm(d2))
  if(optionType=="put")
    return(K*exp(-r*T) * pnorm(-d2) - S*pnorm(-d1))
}

pricing_put_ki <- function(S,K,H,r,T,sigma,N,discret){
  
  prices = rep(0,N)
  touch = rep(0,N)
  
  for(i in 1:N){
    P = S
    hit = 0
    deltaT = T / discret
    C=0
    for(j in 1:discret){
      P = P * exp((r-sigma^2/2)*(deltaT)+sigma*sqrt(deltaT)*rnorm(1,0,1))
      
      if(P < H){
        hit = 1
      }
    }
    if(hit == 1){
      C = max(K-P,0)
    }
    prices[i] = C
    touch[i] = hit
  }
  price = mean(prices*exp(-r*T))
  
  return(list("prices"=prices,"touches"=touch,"price"=price))
}

pricing_put_ko <- function(S,K,H,r,T,sigma,N,discret){
  
  prices = rep(0,N)
  touch = rep(0,N)
  
  for(i in 1:N){
    P = S
    hit = 0
    deltaT = T / discret
    C=0
    for(j in 1:discret){
      P = P * exp((r-sigma^2/2)*(deltaT)+sigma*sqrt(deltaT)*rnorm(1,0,1))
      
      if(P < H){
        hit = 1
        break
      }
    }
    if(hit == 0){
      C = max(K-P,0)
    }
    prices[i] = C
    touch[i] = hit
  }
  price = mean(prices*exp(-r*T))
  
  return(list("prices"=prices,"touches"=touch,"price"=price))
}

pricing_put_ko_brownianb <- function(S,K,H,r,T,sigma,N,discret){
  
  prices = rep(0,N)
  touch = rep(0,N)
  underlyng_prices = c(S,rep(0,N-1))
  
  for(i in 1:N){
    P = S
    hit = 0
    C=0
    brownbridge_barrier(discret,T,S,H,r,sigma)
    if(data$hit)
      prices=
    
    P = P * exp((r-sigma^2/2)*(T)+sigma*sqrt(T)*rnorm(1,0,T))
    prob_no_touch = 1 - exp(-(2*log(H/S)*log(H/P))/(sigma^2*T))
    
    if(P<H){
      C=max(K-P,0)*prob_no_touch*exp(-r*T)  
    }
    
    touch = brownbridge
    
    for(j in 1:discret){
      if(P < H){
        hit = 1
        break
      }
    }
    if(hit == 0){
      C = max(K-P,0)
    }
    prices[i] = C
    touch[i] = hit
  }
  price = mean(prices*exp(-r*T))
  
  return(list("prices"=prices,"touches"=touch,"price"=price,"underlying_prices"=underlyng_prices))
}


r=0
q=0
S_0=100
K=100
H=85
T=0.5
vol=.2
N=50000
discret=1000

# plot(brownbridge_barrier(1000,1)$sample,type="l")

ko = pricing_put_ko(S_0,K,H,r,T,vol,N,discret)
ko$price
ki = pricing_put_ki(S_0,K,H,r,T,vol,N,discret)

ver = 1 < 2
