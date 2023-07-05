# Simulated Data

# setting working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))


set.seed(0911)
simdata_path = paste0(getwd(),"/data_files/sim_data.csv")


sim_data = AAPL
sim_data$R = NA



#The number of samples from the mixture distribution
N = nrow(sim_data)                 

#Sample N random uniforms U
# from 0 to 1
U =runif(N)

#Variable to store the samples from the mixture distribution                                             
rand_samples = rep(NA,N)


# defining parameters for distribution
mu1 = AAPL$R %>% max(na.rm = T)
mu2 = AAPL$R %>% min(na.rm = T)

sd = (AAPL$R %>% sd(na.rm = T)) / 2



#Sampling from the mixture
# first half: lower probability for bull market
for(i in 1: round(N/2)){
  if(U[i]<.1){
    rand_samples[i] = rnorm(1,mu1,sd)
  }else{
    rand_samples[i] = rnorm(1,mu2,sd)
  }
}

# second half: higher probability for bull market
for(i in (round(N/2)+1): N){
  if(U[i]<.8){
    rand_samples[i] = rnorm(1,mu1,sd)
  }else{
    rand_samples[i] = rnorm(1,mu2,sd)
  }
}

sim_data$R = rand_samples

write.csv(x = sim_data,
          file = simdata_path)

