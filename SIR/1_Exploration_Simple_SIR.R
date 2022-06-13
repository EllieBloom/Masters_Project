# Playing around with SIR model in R

# Date started: 7th June 2022

# Following basic tutorial:
# https://towardsdatascience.com/extending-the-basic-sir-model-b6b32b833d76#:~:text=SIR%20Models%20in%20R,package%20provides%20functions%20for%20visualizations.&text=Initially%20we%20consider%20a%20simple,force%20of%20infection(%CE%BB).




# Setup -------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(deSolve) #SIR model package in R
library(reshape2)




# Basic SIR model ---------------------------------------------------------


# Model inputs
initial_state_values=c(S=999999,I=1,R=0)
parameters=c(gamma=0.20,beta=0.5)


# Time points

time=seq(from=1,to=100,by=1)

# SIR model function 

sir_model <- function(time,state,parameters){
  with(as.list(c(state,parameters)),{
    N=S+I+R
    lambda=beta*(I/N) 
    dS=-lambda*S
    dI=lambda*S-gamma*I
    dR=gamma*I
    
    return(list(c(dS,dI,dR)))
  }
  )
}

#Solving the differential equations
output<-as.data.frame(ode(y=initial_state_values,func = sir_model,parms=parameters,times = time))

out_long=melt(output,id="time")

# To plot the proportion of susceptible, infected and recovered individuals over time
ggplot(data = out_long,          
       aes(x = time, y = value/1000000, colour = variable, group = variable)) +  
  geom_line() +xlab("Time (days)")+ylab("Proportion of the population")+scale_color_discrete(name="State")

# Can see that infection peaks at around 48 days


# SIR model extended to account for population turnover -------------------


# Model inputs

parameters=c(gamma=0.2*365,beta=0.4*365,mu=1/70,b=1/70)
# mu is the mortality rate (i.e rate of removal of population)
# b is the rate at which newborns are entering the susceptible compartment (i.e. rate of entry to S)

# Time points

time=seq(from=1,to=400,by=1/365)

# SIR model function

sir_model2 <- function(time,state,parameters){
  with(as.list(c(state,parameters)),{
    N=S+I+R
    lambda=beta*(I/N)
    dS=-lambda*S-mu*S+b*N
    dI=lambda*S-gamma*I-mu*I
    dR=gamma*I-mu*R
    
    return(list(c(dS,dI,dR)))
  }
  )
}

# Solving the differential equations:
output<-as.data.frame(ode(y=initial_state_values,func = sir_model2,parms=parameters,times = time))

out_long=melt(output,id="time")

#Plotting the prevelance over time
ggplot(data = out_long,                                               
       aes(x = time, y = value/1000000, colour = variable, group = variable)) +  
  geom_line() +                                                          
  xlab("Time (years)")+                                                  
  ylab("Prevalence") + scale_color_discrete(name="State")    



# SIR model accounting for impact of vaccination --------------------------

# Model inputs

initial_state_values=c(S=0.60*999999,I=1,R=0.40*999999)
parameters=c(gamma=0.1,beta=0.4)
# 40% vaccinated so start in recovered cateogory

# Time points

time=seq(from=1,to=3*365,by=1)

#SIR Model function
sir_model3 <- function(time,state,parameters){
  with(as.list(c(state,parameters)),{
    N=S+I+R
    lambda=beta*(I/N)
    dS=-lambda*S
    dI=lambda*S-gamma*I
    dR=gamma*I
    
    return(list(c(dS,dI,dR)))
  }
  )
}

# Solving the differential equations:
output<-as.data.frame(ode(y=initial_state_values,func = sir_model3,parms=parameters,times = time))

out_long=melt(output,id="time")
#Plot of prevalance
ggplot(data = out_long,         
       aes(x = time, y = value/1000000, colour = variable, group = variable)) +  
  geom_line() +                                                          
  xlab("Time (days)")+                          
  ylab("Prevalance") +scale_color_discrete(name="State")

# Can still that the epidemic still occurs, infecting ~12.5% of the population, even with 40% vaccinated
# Actually the critical vaccination threshold is 75%...
initial_state_values=c(S=0.25*999999,I=1,R=0.75*999999)


#SIR Model function

# Solving the differential equations:
output<-as.data.frame(ode(y=initial_state_values,func = sir_model3,parms=parameters,times = time))

out_long=melt(output,id="time")
#Plot of prevalance
ggplot(data = out_long,         
       aes(x = time, y = value/1000000, colour = variable, group = variable)) +  
  geom_line() +                                                          
  xlab("Time (days)")+                          
  ylab("Prevalance") +scale_color_discrete(name="State")








# SIR accounting for waning immunity --------------------------------------

# Model input

initial_values=c(S=999999,I=1,R=0)
parameters=c(gamma=0.2*365,beta=0.4*365,sigma=1/(10))
# New parameter sigma -> accounting for recovered back to susceptible
# 1/10 per year -> immunity period is 10 years


# Time points

time=seq(from=1,to=50,by=1/365)

#SIR model function
sir_model4 <- function(time,state,parameters){
  with(as.list(c(state,parameters)),{
    N=S+I+R
    lambda=beta*(I/N)
    dS=-lambda*S+sigma*R
    dI=lambda*S-gamma*I
    dR=gamma*I-sigma*R
    
    return(list(c(dS,dI,dR)))
  }
  )
}



# Solving the differential equations:
output<-as.data.frame(ode(y=initial_values,func = sir_model4,parms=parameters,times = time))

out_long=melt(output,id="time")
#Prevalence plot
ggplot(data = out_long,          
       aes(x = time, y = value/1000000, colour = variable, group = variable)) +  
  geom_line() +                                                          
  xlab("Time (years)")+                         
  ylab("Prevalence") +scale_color_discrete(name="State")

# The prevalence plot appears similar to the situation with population turnover. 
# While in the population turnover scenario, the source of new susceptibles were births, in this case, the source turns out to be individuals losing their immunity.
