# - James Collerton
# - Student Number 46114
# - Source Code for MSc Thesis

# This is the source code for the optimisation of the parameter values over the
# entire model, attempting to induce some stability.

################################################################################
# Libraries

library(ggplot2) 	    # Plotting
library(reshape2) 	  # Reshaping data (tall-narrow <-> short-wide)
library(deSolve) 	    # Solving differential equations
library(minpack.lm)   # Least squares fit using levenberg-marquart algorithm
library(plyr)         # Renaming the dataframes

################################################################################
# Reading in the data for comparison

setwd("~/Documents/Programming/Git_Folders/MSc-Thesis-Code/Submission/3_Experiments_Stabilising_AHHA_SPACE/1_ODE_Parameter_Stabilisation/Data")

comparison_df = read.csv("unstable_data.txt")

df = read.csv("Old_Training_Set.txt")
time_step = df$t
df <- data.frame(df, time_step)
df <- rename(df, replace=c("t" = "time"))
df$N <- NULL
df$Q <- NULL

################################################################################
# This function represents the ODE model as declared by Eureqa. We need to
# format it in this way in order to feed it to the LM optimisation.

rxnrate=function(t,c,parms){
 
 # Parameters for the model. They're formatted like this to make the actual
 # differential functions more readable.
 k1=parms$k1
 k2=parms$k2
 k3=parms$k3
 k4=parms$k4
 k5=parms$k5
 k6=parms$k6
 k7=parms$k7
 k8=parms$k8
 k9=parms$k9
 k10=parms$k10
 k11=parms$k11
 k12=parms$k12
 k13=parms$k13
 k14=parms$k14
 k15=parms$k15
 k16=parms$k16
 k17=parms$k17
 k18=parms$k18
 k19=parms$k19
 k20=parms$k20
 k21=parms$k21
 k22=parms$k22
 k23=parms$k23
 k24=parms$k24
 k25=parms$k25
 k26=parms$k26
 k27=parms$k27
 k28=parms$k28
 k29=parms$k29
 k30=parms$k30
 k31=parms$k31
 k32=parms$k32
 k33=parms$k33

 # Derivatives as declared by Eureqa

 r=rep(0,length(c))

 r[1] = k1*c["R1"] + k2*c["time_step"] + k3 + k4*c["R2"] + k5 * c["A1"] 				            #dS/dt
 r[2] = k6*c["S"] + k7*c["S"]*c["R2"] + k8*(c["A1"] > k9) - k10						   		            #dA1/dt
 r[3] = k11 + k12*c["A2"] + k13*c["time_step"] + k14*c["S"] + k15*c["R1"] + k16*c["A1"]     #dA2/dt
 r[4] = k17 * c["A2"] - k18 * c["time_step"] 										   		                      #dP0/dt
 r[5] = k19 * exp(k20 * c["S"] - k21 * c["A2"])									   			                    #dP1/dt
 r[6] = k22 * c["time_step"] + k23 * c["A2"]											                          #dP2/dt
 r[7] = k24 * c["A2"] + k25 * c["S"] + k26 * c["P2"] + k27 + k28 * c["P1"]		   			      #dR1/dt
 r[8] = k29 * c["time_step"] - k30 * c["P2"] - k31 * c["R1"] * c["time_step"]               #dR2/dt
 r[9] = 1 		                                                                              #dt/dt
 
 # Computed derivatives are returned as a list
 return(list(r))
 
}
 
################################################################################
# This function is to calculate what needs to be minimised by the parameter
# optimisation. 

ssq=function(parms){
 
 # Initial conditions
 cinit=c(S=df$S[1],A1=df$A1[1],A2=df$A2[1],P0=df$P0[1],P1=df$P1[1],P2=df$P2[1],R1=df$R1[1],R2=df$R2[1],time_step=0)

 # Time
 t=df$time

 # Parameters from the parameter estimation
 k1=parms[1]
 k2=parms[2]
 k3=parms[3]
 k4=parms[4]
 k5=parms[5]
 k6=parms[6]
 k7=parms[7]
 k8=parms[8]
 k9=parms[9]
 k10=parms[10]
 k11=parms[11]
 k12=parms[12]
 k13=parms[13]
 k14=parms[14]
 k15=parms[15]
 k16=parms[16]
 k17=parms[17]
 k18=parms[18]
 k19=parms[19]
 k20=parms[20]
 k21=parms[21]
 k22=parms[22]
 k23=parms[23]
 k24=parms[24]
 k25=parms[25]
 k26=parms[26]
 k27=parms[27]
 k28=parms[28]
 k29=parms[29]
 k30=parms[30]
 k31=parms[31]
 k32=parms[32]
 k33=parms[33]

 # Solve ODE for a given set of parameters
 out=ode(y=cinit,times=t,func=rxnrate,
	 	 parms=list(k1=k1,k2=k2,k3=k3,k4=k4,k5=k5,k6=k6,k7=k7,k8=k8,k9=k9,
	 			    k10=k10,k11=k11,k12=k12,k13=k13,k14=k14,k15=k15,k16=k16,
	 			    k17=k17,k18=k18,k19=k19,k20=k20,k21=k21,k22=k22,k23=k23,
	 			    k24=k24,k25=k25,k26=k26,k27=k27,k28=k28,k29=k29,k30=k30,
	 			    k31=k31,k32=k32,k33=k33))
 
 # Change the output ODE to a dataframe and combine the results in order to
 # minimise the difference between the predicted and original data.
 outdf=data.frame(out)
 preddf=melt(outdf,id.var="time",variable.name="species",value.name="conc")
 expdf=melt(df,id.var="time",variable.name="species",value.name="conc")
 ssqres=preddf$conc-expdf$conc

 # Return residuals
 return(ssqres)
}

################################################################################
# Function for plotting the results. Plots the underlying, unstabilised and
# stabilised models to compare.

plot_result <- function(out_ind, dataframe_vals, comparison_vals, y_label, y_limit, leg_x, leg_y){

  par(mfrow = c(1, 2))

  plot(out[,out_ind], type = "l", col = "firebrick1", 
                      xlab = "Time", ylab = y_label,
                      lwd = 2, ylim = y_limit)
  lines(dataframe_vals, col = "dodgerblue", lwd = 2)
  lines(comparison_vals, col = "lightgreen", lwd = 2)

  legend(leg_x, leg_y,
         c("Underlying", "Stabilised", "Unstabilised"),
         lty=c(1, 1, 1),
         lwd=c(2, 2, 2),
         col=c("dodgerblue", "firebrick1", "lightgreen"),
         cex = 0.8)

  plot(out[,out_ind] - dataframe_vals, type = "l", col = "red",
       xlab = "Time", ylab = "Difference in Functions", lwd = 2.5)

}

# Calculates the R squared value.
R_squared <- function(estimate, real_values){
  
  real_values = real_values[!is.na(real_values)]

  estimate_seq = seq(from = 1, to = length(estimate), by = length(estimate) / 100)
  real_values_seq = seq(from = 1, to = length(real_values), by = length(real_values) / 100)

  estimate = estimate[estimate_seq]
  real_values = real_values[real_values_seq]

  obs_mean = mean(estimate)
  
  SS_res = sum((estimate - real_values)^2)
  SS_tot = sum((real_values - obs_mean)^2)

  return(1 - SS_res/SS_tot)

}

# Mean squared error
MSE <- function(estimate, real_values){

  real_values = real_values[!is.na(real_values)]

  estimate_seq = seq(from = 1, to = length(estimate), by = length(estimate) / 100)
  real_values_seq = seq(from = 1, to = length(real_values), by = length(real_values) / 100)

  estimate = estimate[estimate_seq]
  real_values = real_values[real_values_seq]

  residuals = real_values - estimate

  return(sum(residuals^2) / length(residuals))

}

# Mean Absolute Error
MAE <- function(estimate, real_values){

  real_values = real_values[!is.na(real_values)]

  estimate_seq = seq(from = 1, to = length(estimate), by = length(estimate) / 100)
  real_values_seq = seq(from = 1, to = length(real_values), by = length(real_values) / 100)

  estimate = estimate[estimate_seq]
  real_values = real_values[real_values_seq]

  abs_residuals = abs(real_values - estimate)

  return(sum(abs_residuals) / length(abs_residuals))

}

# Creates a vector of R squared values for easy reading.
create_RSq_vec <- function(){

  c(S_Rsq = R_squared(out[,2], df$S),
    A1_Rsq = R_squared(out[,3], df$A1),
    A2_Rsq = R_squared(out[,4], df$A2),
    P1_Rsq = R_squared(out[,6], df$P1),
    P2_Rsq = R_squared(out[,7], df$P2),
    R1_Rsq = R_squared(out[,8], df$R1),
    R2_Rsq = R_squared(out[,9], df$R2))

}

# Creates a vector of MSE values for easy reading.
create_MSE_vec <- function(){

  c(S_MSE = MSE(out[,2], df$S),
    A1_MSE = MSE(out[,3], df$A1),
    A2_MSE = MSE(out[,4], df$A2),
    P1_MSE = MSE(out[,6], df$P1),
    P2_MSE = MSE(out[,7], df$P2),
    R1_MSE = MSE(out[,8], df$R1),
    R2_MSE = MSE(out[,9], df$R2))

}

# Creates a vector of MAE values for easy reading.
create_MAE_vec <- function(){

  c(S_MAE = MAE(out[,2], df$S),
    S_MAE = MAE(out[,3], df$A1),
    S_MAE = MAE(out[,4], df$A2),
    S_MAE = MAE(out[,6], df$P1),
    S_MAE = MAE(out[,7], df$P2),
    S_MAE = MAE(out[,8], df$R1),
    S_MAE = MAE(out[,9], df$R2))

}

################################################################################
# Running the parameter optimisation.

# Initial parameter values were chosen to be zero, as this seemed the most sensible
# possible unbiased option.

parms=c(k1=0,k2=0,k3=0,k4=0,k5=0,k6=0,k7=0,k8=0,k9=0,k10=0,k11=0,k12=0,
        k13=0,k14=0,k15=0,k16=0,k17=0,k18=0,k19=0,k20=0,k21=0,k22=0,k23=0,
        k24=0,k25=0,k26=0,k27=0,k28=0,k29=0,k30=0,k31=0,k32=0,k33=0) # fitting

# Running the fitting of values. Need to have the max iterations set high
# in order to ensure the result converges.

fitval=nls.lm(par=parms,fn=ssq, control = c(maxiter = 1000, maxfev = 20000))

parest=as.list(coef(fitval))

# Creating and running the output ODE model with the new parameters.

cinit=c(S=df$S[1],A1=df$A1[1],A2=df$A2[1],P0=df$P0[1],P1=df$P1[1],P2=df$P2[1],R1=df$R1[1],R2=df$R2[1],time_step=0)
t=df$time
parms=as.list(parest)
out=ode(y=cinit,times=t,func=rxnrate,parms=parms)

# Plotting the results for comparison, and calculating the R square values.

plot_result(2, df$S, comparison_df$S, "Number of Scouts", c(5,43), 0, 13)
plot_result(3, df$A1, comparison_df$A1, "Number of Assessors (Site One)", c(0, 5.9), 0, 5.8)
plot_result(4, df$A2, comparison_df$A2, "Number of Assessors (Site Two)", c(0, 4), 0, 4)
plot_result(6, df$P1, comparison_df$P1, "Number of Passive (Site One)", c(0, 1.5), 0, 1.5)
plot_result(7, df$P2, comparison_df$P2, "Number of Passive (Site Two)", c(0, 140), 0, 140)
plot_result(8, df$R1, comparison_df$R1, "Number of Recruiters (Site One)", c(0, 11), 0, 11)
plot_result(9, df$R2, comparison_df$R2, "Number of Recruiters (Site Two)", c(0, 29), 0, 29)

create_RSq_vec()
create_MSE_vec()
create_MAE_vec()
