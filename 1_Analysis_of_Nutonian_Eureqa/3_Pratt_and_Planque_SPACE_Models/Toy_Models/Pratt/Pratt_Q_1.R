# - James Collerton
# - Student Number 46114
# - Source Code for MSc Thesis

# This is the source code for the experiments done with the Pratt model. The
# model is built and run with the given quorum size and then compared to the
# original model.

###############################################################################
# Global Variables

N = 208
p = 0.25
lmbda = 0.033
phi = 0.099
mu = 0.013
k_one = 0.015
k_two = 0.02
p_onetwo = 0.008
c = 4.6
total_passive = N * (1 - p)

###############################################################################
# Functions

# Calculating the R squared value. 
R_squared <- function(estimate, real_values){
  
  obs_mean = mean(estimate)
  
  SS_res = sum((estimate - real_values)^2)
  SS_tot = sum((real_values - obs_mean)^2)

  return(1 - SS_res/SS_tot)
}

# Calculating the Mean Squared Error value. 
MSE <- function(estimate, real_values){
  residuals = real_values - estimate
  return(sum(residuals^2) / length(residuals))
}

# Calculating the Mean Absolute Error squared value. 
MAE <- function(estimate, real_values){
  abs_residuals = abs(real_values - estimate)
  return(sum(abs_residuals) / length(abs_residuals))
}

# This is used to scale the two emigrations in order to compare them on the 
# the same timescale. Functions such as the MAE, MSE and R^2 demand the same
# number of data points. 
scale_for_comparison <- function(data){

  sampling = length(data) / 100
  sampled_vec = c()

  for( i in 1: 100 ){ sampled_vec[i] = data[ i * sampling ] }

  return(sampled_vec)
}

# Note, we ignore the last three points in the passive model, as for low
# quorums this is where the direct carrying began. This is covered in the
# thesis. Creates a vector of the R squared values to view easily.
create_R_squared_errors <- function(){

  R_squared_errors = c(S_rsq = R_squared(S_vec_scaled, S_scaled),
                       A1_rsq = R_squared(A1_vec_scaled, A1_scaled),
                       A2_rsq = R_squared(A2_vec_scaled, A2_scaled),
                       R1_rsq = R_squared(R1_vec_scaled, R1_scaled),
                       R2_rsq = R_squared(R2_vec_scaled, R2_scaled),
                       P1_rsq = R_squared(P1_vec_scaled[1 : (length(P1_vec_scaled) - 3)], P1_scaled[1 : (length(P1_scaled) - 3)]),
                       P2_rsq = R_squared(P2_vec_scaled[1 : (length(P2_vec_scaled) - 3)], P2_scaled[1 : (length(P2_scaled) - 3)]))

  return(R_squared_errors)

}

# Creates a vector of the MAE values to view easily.
create_MAE_errors <- function(){

  MAE_vec = c(S_MAE = MAE(S_vec_scaled, S_scaled),
              A1_MAE = MAE(A1_vec_scaled, A1_scaled),
              A2_MAE = MAE(A2_vec_scaled, A2_scaled),
              R1_MAE = MAE(R1_vec_scaled, R1_scaled),
              R2_MAE = MAE(R2_vec_scaled, R2_scaled),
              P1_MAE = MAE(P1_vec_scaled[1 : (length(P1_vec_scaled) - 3)], P1_scaled[1 : (length(P1_scaled) - 3)]),
              P2_MAE = MAE(P2_vec_scaled[1 : (length(P2_vec_scaled) - 3)], P2_scaled[1 : (length(P2_scaled) - 3)]))  

  return(MAE_vec)
}

# Creates a vector of the MSE values to view easily.
create_MSE_errors <- function(){

  MSE_vec = c(S_MSE = MSE(S_vec_scaled, S_scaled),
              A1_MSE = MSE(A1_vec_scaled, A1_scaled),
              A2_MSE = MSE(A2_vec_scaled, A2_scaled),
              R1_MSE = MSE(R1_vec_scaled, R1_scaled),
              R2_MSE = MSE(R2_vec_scaled, R2_scaled),
              P1_MSE = MSE(P1_vec_scaled[1 : (length(P1_vec_scaled) - 3)], P1_scaled[1 : (length(P1_scaled) - 3)]),
              P2_MSE = MSE(P2_vec_scaled[1 : (length(P2_vec_scaled) - 3)], P2_scaled[1 : (length(P2_scaled) - 3)]))

  return(MSE_vec)

}

# Used to plot the results of the model alongside the training data.
plot_result <- function(vec_scaled, scaled, ylabel, leg_x, leg_y){

    plot(vec_scaled, type = "l", col = "midnightblue", lwd = 2,
         xlab = "Percentage through Emigration", ylab = ylabel)
    lines(scaled, type = "l", col = "chartreuse", lwd = 2)

    legend(leg_x, leg_y,
       c("Eureqa Solution", "Original Function"),
       lty=c(1, 1),
       lwd=c(2, 2),
       col=c("midnightblue", "chartreuse"),
       cex = 0.5 )

    plot(vec_scaled - scaled, type = "l", col = "indianred1", lwd = 2,
     xlab = "Percentage through Emigration", ylab = "Difference between Functions")

}

###############################################################################
# Initialising the vectors for storing the results.

S_vec = c()
A_one_vec = c()
A_two_vec =c()
R_one_vec = c()
R_two_vec = c()
P_zero_vec = c()
P_one_vec = c()
P_two_vec = c()

###############################################################################
# Differential functions

dS <- function(S, A_one, A_two, R_one, R_two, P_one, P_two, P_nought){

	result = R_two*cos(R_two)*
           (
              (P_one > 0) * 0 +
              (P_one <= 0) * -0.0723939851687596
           ) - 0.0260453824363613*S

	return(result)
}

dA_one <- function(S, A_one, A_two, R_one, R_two, P_one, P_two, P_nought){

	result = 0.0133298359626687*S + 0.6*R_one*(P_two < 0.313135626571411)*
           sin(0.0368013153228601 - 0.315477428247599*P_two^2) - 
           0.0234809808975181*A_one

	return(result)
}

dA_two <- function(S, A_one, A_two, R_one, R_two, P_one, P_two, P_nought){

	result = 0.0144116390606646 * S - 0.0163386497316581 * A_two

	return(result)
}

dR_one <- function(S, A_one, A_two, R_one, R_two, P_one, P_two, P_nought){

	result = 0.0151191182966451 * A_one - 0.00801814969222646 * R_one

	return(result)
}

dR_two <- function(S, A_one, A_two, R_one, R_two, P_one, P_two, P_nought){

	result = 0.0200788534521445 * A_two + 0.00800735524448586 * R_one

	return(result)
}

dP_one <- function(S, A_one, A_two, R_one, R_two, P_one, P_two, P_nought){

	result = 0.0978114723772282 * R_one * (P_nought > 0) - 4.59 *
             ( abs(P_nought + P_one + P_two + R_two - 208 ) < 0.01 )

	return(result)
	
}

dP_two <- function(S, A_one, A_two, R_one, R_two, P_one, P_two, P_nought){

    result = 0.0981981137620581 * R_two * (P_nought > 0) +
             4.59 * ( abs(P_nought + P_one + P_two + R_two - 208 ) < 0.01 )

	return(result)
	
}

###############################################################################

###############################################################################
# Comparing results.

# Training data.

data_dir = "~/Documents/Programming/Git_Folders/MSc-Thesis-Code/Submission/1_Analysis_of_Nutonian_Eureqa/3_Pratt_and_Planque_SPACE_Models/Data/Pratt"

setwd(data_dir)

mydata = read.csv("Pratt_Q1.txt")

###############################################################################

###############################################################################
# Running model

# Initial conditions set to be the same as the underlying data.
t = 0

S = mydata$S[1]
A_one = mydata$A_one[1]
A_two = mydata$A_two[1]
R_one = mydata$R_one[1]
R_two = mydata$R_two[1]
P_zero = mydata$P_nought[1]
P_one = mydata$P_one[1]
P_two = mydata$P_two[1]

# While we have not reached the end of the underlying model, continue running the
# new model.
while(t < 1035){

  S_vec = c(S_vec, S)
  A_one_vec = c(A_one_vec, A_one)
  A_two_vec = c(A_two_vec, A_two)
  R_one_vec = c(R_one_vec, R_one)
  R_two_vec = c(R_two_vec, R_two)
  P_zero_vec = c(P_zero_vec, P_zero)
  P_one_vec = c(P_one_vec, P_one)
  P_two_vec = c(P_two_vec, P_two)

  # Update the different components of the model.
  S = S + dS(S, A_one, A_two, R_one, R_two, P_one, P_two, P_zero)
  A_one = A_one + dA_one(S, A_one, A_two, R_one, R_two, P_one, P_two, P_zero) 
  A_two = A_two + dA_two(S, A_one, A_two, R_one, R_two, P_one, P_two, P_zero)
  R_one = R_one + dR_one(S, A_one, A_two, R_one, R_two, P_one, P_two, P_zero)
  R_two = R_two + dR_two(S, A_one, A_two, R_one, R_two, P_one, P_two, P_zero)
  P_one = P_one + dP_one(S, A_one, A_two, R_one, R_two, P_one, P_two, P_zero)
  P_two = P_two + dP_two(S, A_one, A_two, R_one, R_two, P_one, P_two, P_zero)

	P_zero = total_passive - P_one - P_two

	t = t + 1
}

###############################################################################
# Scaling results.

S_vec_scaled = scale_for_comparison(S_vec)
S_scaled = scale_for_comparison(mydata$S)

A1_vec_scaled = scale_for_comparison(A_one_vec)
A1_scaled = scale_for_comparison(mydata$A_one)

A2_vec_scaled = scale_for_comparison(A_two_vec)
A2_scaled = scale_for_comparison(mydata$A_two)

R1_vec_scaled = scale_for_comparison(R_one_vec)
R1_scaled = scale_for_comparison(mydata$R_one)

R2_vec_scaled = scale_for_comparison(R_two_vec)
R2_scaled = scale_for_comparison(mydata$R_two)

P0_vec_scaled = scale_for_comparison(P_zero_vec)
P0_scaled = scale_for_comparison(mydata$P_nought)

P1_vec_scaled = scale_for_comparison(P_one_vec)
P1_scaled = scale_for_comparison(mydata$P_one)

P2_vec_scaled = scale_for_comparison(P_two_vec)
P2_scaled = scale_for_comparison(mydata$P_two)

###############################################################################

###############################################################################
# Plotting Results

par(mfrow = c(1, 2))

plot_result(S_vec_scaled, S_scaled, "Number of Scouts", 50, 40)
plot_result(A1_vec_scaled, A1_scaled, "Number of Assessors (Site One)", 50, 10.2)
plot_result(A2_vec_scaled, A2_scaled, "Number of Assessors (Site Two)", 50, 13)
plot_result(R1_vec_scaled, R1_scaled, "Number of Recruiters (Site One)", 50, 8.4)
plot_result(R2_vec_scaled, R2_scaled, "Number of Recruiters (Site Two)", 50, 8)
plot_result(P1_vec_scaled[1 : (length(P1_vec_scaled) - 3)], P1_scaled[1 : (length(P1_scaled) - 3)], "Number of Passive (Site One)", 49, 7)
plot_result(P2_vec_scaled[1 : (length(P2_vec_scaled) - 3)], P2_scaled[1 : (length(P2_scaled) - 3)], "Number of Passive (Site Two)", 49, 17)

###############################################################################

###############################################################################
# Errors

create_R_squared_errors()
create_MAE_errors()
create_MSE_errors()

###############################################################################
