#The number of samples from the mixture distribution
N = 100000                 

#Sample N random uniforms U
U =runif(N)

#Variable to store the samples from the mixture distribution                                             
rand.samples = rep(NA,N)

#Sampling from the mixture
for(i in 1:N){
  if(U[i]<.3){
    rand.samples[i] = rnorm(1,0,1)
  }else if(U[i]<.8){
    rand.samples[i] = rnorm(1,10,1)
  }else{
    rand.samples[i] = rnorm(1,3,.1)
  }
}


r__gaussmix_2c <- function(N, mu1, mu2, sigma1, sigma2, pi1, pi2){
  
  if(pi1 + pi2 < 0.99999){stop("Error")}
  
  U = runif(N)
  
  samples = rep(NA,N)
  
  

  
  
  for(i in 1:N){
    if(U[i] < pi1){
      samples[i] = rnorm(1,mu1,sigma1)
      }else{
      samples[i] = rnorm(1,mu2,sigma2)
    }
  }
  return(samples)
}


mu1 = 0.0001686 
mu2 = -0.00199134

sig1 = 0.02856135 
sig2 = 0.01024246

pi1 = 0.11219124
pi2 = 0.88780874


sample = r__gaussmix_2c(10000, mu1,mu2,sig1,sig2, pi1, pi2)


quantile(sample, probs = 0.99) * Pf
