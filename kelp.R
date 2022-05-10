# ECOLOGICAL MODDELING REPORT 

# THE TOP DOWN CONTROL OF SEA URCHINS IN KELP FORESTS.


##############################################################################################################################################
##############################################################################################################################################

  
# 2) Graph showing effect of density dependent predation on population density.
    # Showing otter and urchin population 

install.packages("deSolve")
library(deSolve)

# Defining the model
otter_urchin_competition_1 <- function(times, N0, params){
N1 <- N0[1]
N2  <- N0[2]
with(as.list(params), {
  dN1.dt <- N1*(r1-alpha12*N2)
  dN2.dt <- N2*(alpha21*N1-d2)
  return(list(c(dN1.dt, dN2.dt)))
  })
}

# Setting values of parameters that are used in the equations
params <- c(r1 = 0.5, 
            alpha12 = 1, 
            alpha21= 0.2,
            d2 = 0.1)

# Initial conditions
NO <- c(1,1)

# Defining time
t.values <- seq(0,100)

# Simulating the continuous time dynamic using the ode
otter_urchin_out_1 <- ode(NO, t.values, otter_urchin_competition_1, params)

# Plot results
# First column of N.out shows the time values for the x axis
# Second column of N.out shows the population sizes for species 1.
# Third column of N.out shows the population sizes for species 2.

par(mfrow=c(1,2))
plot(otter_urchin_out_1[,1], otter_urchin_out_1[,2], type = 'l', col = 'purple', xlab = 'Time', ylab = 'Abundance', main = 'Otter, Urchin Normal Competition')

lines(otter_urchin_out_1[,1], otter_urchin_out_1[,3], type = 'l', col = 'grey')

#######################################################################################

# Definition of the model 
otter_urchin_competition_2 <- function(times, N0, params){ 
  N1 <- N0[1] 
  N2 <- N0[2] 
  with(as.list(params), { 
    dN1.dt <- r1*N1*((K1-N1-(alpha12*N2))/K1) 
    dN2.dt <- r2*N2*((K2-N2-(alpha21*N1))/K2) 
    return(list(c(dN1.dt, dN2.dt))) 
  })
}

# setting parameters.
# r1 and r2 are intrinsic growth rates for N1 and N2.
# K1 and K2 are the density dependence parameters.
# alpha12 and alpha 21 is how species 1 affects species 2 and vice versa, if

params <- c(r1 = 0.5, 
            r2 = 0.1, 
            K1 = 30, 
            K2 = 10,
            alpha12 = 1.0, 
            alpha21 = 0.2)
# initial conditions 
N0 <- c(1,1) 

# definition of time 
t.values <- seq(100, 300)

# Simulating the continuous time dynamic using the ode
otter_urchin_out_2 <- ode(NO, t.values, otter_urchin_competition_2, params)

# plot results
par(mfrow=c(1,2)) 
plot(otter_urchin_out_2[,1], otter_urchin_out_2[,2], type='l',col='purple', xlab = 'Time', ylab = 'Abundance', main = 'Otter, Urchin Competition after Otter population decline') # prey

lines(otter_urchin_out_2[,1], otter_urchin_out_2[,3],  type='l',col='grey') # predator

##############################################################################################################################################
##############################################################################################################################################



# 3) Graph showing effect of otter removal from ecosystem on urchin populations.
# exponential growth of urchin population.

# Defining the model
otter_urchin_competition_1 <- function(times, N0, params){
  N1 <- N0[1]
  N2  <- N0[2]
  with(as.list(params), {
    dN1.dt <- N1*(r1-alpha12*N2)
    dN2.dt <- N2*(alpha21*N1-d2)
    return(list(c(dN1.dt, dN2.dt)))
  })
}

# Setting values of parameters that are used in the equations
params <- c(r1 = 0.5, 
            alpha12 = 1, 
            alpha21= 0.05,
            d2 = 0.1)

# Initial conditions
NO <- c(1,1)

# Defining time
t.values <- seq(0,100)

# Simulating the continuous time dynamic using the ode
otter_urchin_out_1 <- ode(NO, t.values, otter_urchin_competition_1, params)

# Plot results
# First column of N.out shows the time values for the x axis
# Second column of N.out shows the population sizes for species 1.
# Third column of N.out shows the population sizes for species 2.

par(mfrow=c(1,2))
plot(otter_urchin_out_1[,1], otter_urchin_out_1[,2], type = 'l', col = 'purple', xlab = 'Time', ylab = 'Abundance', main = 'Otter, Urchin Normal Competition')

lines(otter_urchin_out_1[,1], otter_urchin_out_1[,3], type = 'l', col = 'grey')

##########################################################################################


# Definition of the model 
otter_urchin_competition_2 <- function(times, N0, params){ 
  N1 <- N0[1] 
  N2 <- N0[2] 
  with(as.list(params), { 
    dN1.dt <- r1*N1*((K1-N1-(alpha12*N2))/K1) 
    dN2.dt <- r2*N2*((K2-N2-(alpha21*N1))/K2) 
    return(list(c(dN1.dt, dN2.dt))) 
  })
}

# setting parameters.
# r1 and r2 are intrinsic growth rates for N1 and N2.
# K1 and K2 are the density dependence parameters.
# alpha12 and alpha 21 is how species 1 affects species 2 and vice versa, if

params <- c(r1 = 0.5, 
            r2 = 0.1, 
            K1 = 30, 
            K2 = 10,
            alpha12 = 0.1, 
            alpha21 = 0.15)
# initial conditions 
N0 <- c(1,1) 

# definition of time 
t.values <- seq(100, 300)

# Simulating the continuous time dynamic using the ode
otter_urchin_out_2 <- ode(NO, t.values, otter_urchin_competition_2, params)

# plot results
par(mfrow=c(1,2)) 
plot(otter_urchin_out_2[,1], otter_urchin_out_2[,2], type='l',col='purple', xlab = 'Time', ylab = 'Abundance', main = 'Otter, Urchin Competition after Otter population decline') # prey

lines(otter_urchin_out_2[,1], otter_urchin_out_2[,3],  type='l',col='grey') # predator

############################################################################################
############################################################################################


# 4) Graph showing normal predator prey population curve with urchins and kelp.


# Defining the model
urchin_kelp_competition_1 <- function(times, N0, params){
  N1 <- N0[1]
  N2  <- N0[2]
  with(as.list(params), {
    dN1.dt <- N1*(r1-alpha12*N2)
    dN2.dt <- N2*(alpha21*N1-d2)
    return(list(c(dN1.dt, dN2.dt)))
  })
}

# Setting values of parameters that are used in the equations
params <- c(r1 = 0.5, 
            alpha12 = 0.7, 
            alpha21= 0.1,
            d2 = 0.1)

# Initial conditions
NO <- c(1,1)

# Defining time
t.values <- seq(0,100)

# Simulating the continuous time dynamic using the ode
urchin_kelp_out_1 <- ode(NO, t.values, urchin_kelp_competition_1, params)

# Plot results
# First column of N.out shows the time values for the x axis
# Second column of N.out shows the population sizes for species 1.
# Third column of N.out shows the population sizes for species 2.

par(mfrow=c(1,2))
plot(urchin_kelp_out_1[,1], urchin_kelp_out_1[,2], type = 'l', col = 'dark green', xlab = 'Time', ylab = 'abundance', main = 'Urchin, Kelp Normal Competition')

lines(urchin_kelp_out_1[,1], urchin_kelp_out_1[,3], type = 'l', col = 'purple')


##########################################################################

urchin_kelp_competition_2 <- function(times, N0, params){ 
  N1 <- N0[1] 
  N2 <- N0[2] 
  with(as.list(params), { 
    dN1.dt <- r1*N1*((K1-N1-(alpha12*N2))/K1) 
    dN2.dt <- r2*N2*((K2-N2-(alpha21*N1))/K2) 
    return(list(c(dN1.dt, dN2.dt))) 
  })
}

# setting parameters.
# r1 and r2 are intrinsic growth rates for N1 and N2.
# K1 and K2 are the density dependence parameters.
# alpha12 and alpha 21 is how species 1 affects species 2 and vice versa, if

params <- c(r1 = 0.5, 
            r2 = 0.5, 
            K1 = 50, 
            K2 = 30,
            alpha12 = 0.7, 
            alpha21 = 0.1)
# initial conditions 
N0 <- c(1,1) 

# definition of time 
t.values <- seq(100, 150)

# Simulating the continuous time dynamic using the ode
urchin_kelp_out_2 <- ode(NO, t.values, urchin_kelp_competition_2, params)

# plot results
par(mfrow=c(1,2)) 
plot(urchin_kelp_out_2[,1], urchin_kelp_out_2[,2], type='l',col='dark green', xlab = 'Time', ylab = 'Abundance', main = 'Urchin, Kelp Competition after Otter population decline') # prey

lines(urchin_kelp_out_2[,1], urchin_kelp_out_2[,3],  type='l',col='purple') # predator





# 5) Graph showing effect of density independent predation on population density.
# when urchin predators population declines.
# exponential growth of sea urchins 
# exponential decay of kelp

# Definition of the model 
urchin_kelp_competition_2 <- function(times, N0, params){ 
  N1 <- N0[1] 
  N2 <- N0[2] 
  with(as.list(params), { 
    dN1.dt <- r1*N1*((K1-N1-(alpha12*N2))/K1) 
    dN2.dt <- r2*N2*((K2-N2-(alpha21*N1))/K2) 
    return(list(c(dN1.dt, dN2.dt))) 
  })
}

# setting parameters.
# r1 and r2 are intrinsic growth rates for N1 and N2.
# K1 and K2 are the density dependence parameters.
# alpha12 and alpha 21 is how species 1 affects species 2 and vice versa, if

params <- c(r1 = 0.5, 
            r2 = 0.5, 
            K1 = 50, 
            K2 = 30,
            alpha12 = 2.0, 
            alpha21 = 0.1)
# initial conditions 
N0 <- c(1,1) 

# definition of time 
t.values <- seq(100, 150)

# Simulating the continuous time dynamic using the ode
urchin_kelp_out_2 <- ode(NO, t.values, urchin_kelp_competition_2, params)

# plot results
par(mfrow=c(1,2)) 
plot(urchin_kelp_out_2[,1], urchin_kelp_out_2[,2], type='l',col='dark green', xlab = 'Time', ylab = 'Abundance', main = 'Urchin, Kelp Competition after Otter population decline') # prey

lines(urchin_kelp_out_2[,1], urchin_kelp_out_2[,3],  type='l',col='purple') # predator

