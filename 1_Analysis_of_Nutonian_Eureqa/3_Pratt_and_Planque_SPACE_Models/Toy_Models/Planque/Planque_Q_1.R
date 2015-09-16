# - James Collerton
# - Student Number 46114
# - Source Code for MSc Thesis

# This is the source code for the experiments done with the Planque model. The
# model is built and run with the given quorum size and then compared to the
# original model

# REMEMBER YOU WILL HAVE TO CHANGE THE DIRECTORY LOCATION

###############################################################################
# Global Variables as defined in the original paper.

N = 250 					  # Colony size as defined in original paper.
F = 0.1447 					# Fraction of active ants, range: [0.05, 0.5]
f = 0.1 					  # Fraction of post-quorum RTR time.
mu = 0.05 					# Rate active ants at old nest become scouts
lmda = 0.1 					# Rate at which ants following TR become recruiters
phi = 0.2 					# Rate at which passive ants carried to new nest.
kappa = 0.001 			# Rate at which scouts indep become recruiters.
epsilon = 0.01 			# Used to avoid singularities in denominators

###############################################################################
# Initialising the vectors for storing the results.

S_vec = c()
A_vec = c()
R_vec = c()
P_vec = c()
C_vec = c()

###############################################################################
# Function definitions

# R squared measurement for the goodness of fit.
R_squared <- function(estimate, real_values){
  
  obs_mean = mean(estimate)
  
  SS_res = sum((estimate - real_values)^2)
  SS_tot = sum((real_values - obs_mean)^2)

  return(1 - SS_res/SS_tot)
}

# Mean Squared Error
MSE <- function(estimate, real_values){
  residuals = real_values - estimate
  return(sum(residuals^2) / length(residuals))
}

# Mean Absolute Error
MAE <- function(estimate, real_values){
  abs_residuals = abs(real_values - estimate)
  return(sum(abs_residuals) / length(abs_residuals))
}

# This is used to scale the two emigrations in order to compare them on the 
# the same timescale. Functions such as the MAE, MSE and R^2 demand the same
# number of data points 
scale_for_comparison <- function(data){

  sampling = length(data) / 100
  sampled_vec = c()

  for( i in 1: 100 ){ sampled_vec[i] = data[ i * sampling ] }

  return(sampled_vec)
}

# Creates a vector of R squared values from the outputs.
create_R_squared_error <- function(){

  R_squared_errors = c(S_rsq = R_squared(S_vec_scaled, S_scaled),
                       A_rsq = R_squared(A_vec_scaled, A_scaled),
                       R_rsq = R_squared(R_vec_scaled, R_scaled),
                       C_rsq = R_squared(C_vec_scaled, C_scaled),
                       P_rsq = R_squared(P_vec_scaled, P_scaled))

  return(R_squared_errors)

}

# Creates a vector of MAE values from the outputs.
create_MAE_error <- function(){

  MAE_vec = c(S_MAE = MAE(S_vec_scaled, S_scaled),
              A_MAE = MAE(A_vec_scaled, A_scaled),
              R_MAE = MAE(R_vec_scaled, R_scaled),
              C_MAE = MAE(C_vec_scaled, C_scaled),
              P_MAE = MAE(P_vec_scaled, P_scaled))

  return(MAE_vec)

}

# Creates a vector of MSE values from the outputs.
create_MSE_error <- function(){

  MSE_vec = c(S_MSE = MSE(S_vec_scaled, S_scaled),
              A_MSE = MSE(A_vec_scaled, A_scaled),
              R_MSE = MSE(R_vec_scaled, R_scaled),
              C_MSE = MSE(C_vec_scaled, C_scaled),
              P_MSE = MSE(P_vec_scaled, P_scaled))

  return(MSE_vec)

}

# Used to plot the results of the model alongside the training data.
plot_result <- function(vec_scaled, scaled, ylabel, leg_x, leg_y){

    plot(vec_scaled, type = "l", col = "midnightblue", lwd = 2,
         xlab = "Percentage through Emigration", ylab = ylabel)
    lines(scaled, type = "l", col = "chartreuse", lwd = 2)

    legend(leg_x, leg_y,
       c("Eureqa", "Original"),
       lty=c(1, 1),
       lwd=c(2, 2),
       col=c("midnightblue", "chartreuse"),
       cex = 0.8 )

    plot(vec_scaled - scaled, type = "l", col = "indianred1", lwd = 2,
     xlab = "Percentage through Emigration", ylab = "Difference between Functions")

}

###############################################################################
# Differential functions

dS <- function(S, A, R, P, C){

	result = 0.047576782448518*A - 0.000983510375027726*S - 
           0.000291869888948359*S*R - 0.000319044772597488*A*R

	return(result)
}

dA <- function(S, A, R, P, C){

  result = -0.0514193243836146*A

	return(result)
}

dR <- function(S, A, R, P, C){

	result = 0.00101600348105209 * S +
             0.000288520709947242 * R *
             (
              (C > 0) * 1.05*S +
              (C <= 0) * 315.771406293459
             )

	return(result)
}

dP <- function(S, A, R, P, C){

	result = R * P/(-5.35671645627978 * R - 5.53741799404282 * P)

	return(result)
}

dC <- function(S, A, R, P, C){

	result = R*P*(0.861861631862385 < R)/(5.82057309225481*R + 5.50662212786997*P)

	return(result)
}

###############################################################################

###############################################################################
# Comparing results.

# Training data.

data_dir = "~/Documents/Programming/Git_Folders/MSc-Thesis-Code/Submission/1_Analysis_of_Nutonian_Eureqa/3_Pratt_and_Planque_SPACE_Models/Data/Planque"

setwd(data_dir)

mydata = read.csv("Planque_Q1.txt")

###############################################################################

###############################################################################
# Running model

# Initial conditions set to be the same as the underlying data.
t = 0

A = mydata$A[1]
S = mydata$S[1]
R = mydata$R[1]
P = mydata$P[1]
C = mydata$C[1]

# This is the time at which carrying begins. We manually have to seed this for
# the reasons explained in the thesis concerning complexity and accuracy as
# defined by Eureqa.
carry_time = which(mydata$C > 0)[1]

# While we have not reached the end of the underlying model, continue running the
# new model.
while(t <= nrow(mydata)){

  S_vec = c(S_vec, S)
  A_vec = c(A_vec, A)
  R_vec = c(R_vec, R)
  P_vec = c(P_vec, P)
  C_vec = c(C_vec, C)

  # Update the different components of the model. If we are at the carry point
  # then we need to seed the model.
  S = S + dS(S, A, R, P, C)
  A = A + dA(S, A, R, P, C)
  P = P + dP(S, A, R, P, C)
  
  if(t + 1 == carry_time){
    C = C + dC(S, A, R, P, mydata$C[ t + 1 ])
    R = R + dR(S, A, R, P, mydata$R[ t + 1 ])
  } else{
    C = C + dC(S, A, R, P, C)
    R = R + dR(S, A, R, P, C)
  }

	t = t + 1
}

###############################################################################
# Scaling results.

S_vec_scaled = scale_for_comparison(S_vec)
S_scaled = scale_for_comparison(mydata$S)

A_vec_scaled = scale_for_comparison(A_vec)
A_scaled = scale_for_comparison(mydata$A)

R_vec_scaled = scale_for_comparison(R_vec)
R_scaled = scale_for_comparison(mydata$R)

P_vec_scaled = scale_for_comparison(P_vec)
P_scaled = scale_for_comparison(mydata$P)

C_vec_scaled = scale_for_comparison(C_vec)
C_scaled = scale_for_comparison(mydata$C)

###############################################################################

###############################################################################
# Plotting Results

par(mfrow = c(1, 2))

plot_result(S_vec_scaled, S_scaled, "Number of Scouts", 50, 10)
plot_result(A_vec_scaled, A_scaled, "Number of Assessors", 60, 10)
plot_result(R_vec_scaled, R_scaled, "Number of Recruiters", 60, 5)
plot_result(P_vec_scaled, P_scaled, "Number of Passive", 10, 50)
plot_result(C_vec_scaled, C_scaled, "Number of Carried", 10, 200)

###############################################################################

###############################################################################
# Calculating errors.

create_R_squared_error()
create_MSE_error()
create_MAE_error()
