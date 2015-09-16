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

R_squared <- function(estimate, real_values){
  
  obs_mean = mean(estimate)
  
  SS_res = sum((estimate - real_values)^2)
  SS_tot = sum((real_values - obs_mean)^2)

  return(1 - SS_res/SS_tot)
}

MSE <- function(estimate, real_values){
  residuals = real_values - estimate
  return(sum(residuals^2) / length(residuals))
}

MAE <- function(estimate, real_values){
  abs_residuals = abs(real_values - estimate)
  return(sum(abs_residuals) / length(abs_residuals))
}

scale_for_comparison <- function(data){

  sampling = length(data) / 100
  sampled_vec = c()

  for( i in 1: 100 ){ sampled_vec[i] = data[ i * sampling ] }

  return(sampled_vec)
}

create_R_squared_errors <- function(){

  R_squared_errors = c(S_rsq = R_squared(S_vec_scaled, S_scaled),
                       A1_rsq = R_squared(A1_vec_scaled, A1_scaled),
                       A2_rsq = R_squared(A2_vec_scaled, A2_scaled),
                       R1_rsq = R_squared(R1_vec_scaled, R1_scaled),
                       R2_rsq = R_squared(R2_vec_scaled, R2_scaled),
                       P1_rsq = R_squared(P1_vec_scaled, P1_scaled),
                       P2_rsq = R_squared(P2_vec_scaled, P2_scaled))

  return(R_squared_errors)

}

create_MAE_errors <- function(){

  MAE_vec = c(S_MAE = MAE(S_vec_scaled, S_scaled),
              A1_MAE = MAE(A1_vec_scaled, A1_scaled),
              A2_MAE = MAE(A2_vec_scaled, A2_scaled),
              R1_MAE = MAE(R1_vec_scaled, R1_scaled),
              R2_MAE = MAE(R2_vec_scaled, R2_scaled),
              P1_MAE = MAE(P1_vec_scaled, P1_scaled),
              P2_MAE = MAE(P2_vec_scaled, P2_scaled))  

  return(MAE_vec)
}

create_MSE_errors <- function(){

  MSE_vec = c(S_MSE = MSE(S_vec_scaled, S_scaled),
              A1_MSE = MSE(A1_vec_scaled, A1_scaled),
              A2_MSE = MSE(A2_vec_scaled, A2_scaled),
              R1_MSE = MSE(R1_vec_scaled, R1_scaled),
              R2_MSE = MSE(R2_vec_scaled, R2_scaled),
              P1_MSE = MSE(P1_vec_scaled, P1_scaled),
              P2_MSE = MSE(P2_vec_scaled, P2_scaled))

  return(MSE_vec)

}

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
# Initialising

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

	result = -0.0251182972780439 * S - 0.0230955293171324 * ( A_one * (S > 0) + (P_one * (S <= 0) ) )

	return(result)
}

dA_one <- function(S, A_one, A_two, R_one, R_two, P_one, P_two, P_nought){

	result = 0.000227254735768916 * S^2 - 0.0229818482798014 * 
			 (
			 	(S > 0) * -0.0455813694747499 * S +
			 	(S <= 0) * A_one

			 )

	return(result)
}

dA_two <- function(S, A_one, A_two, R_one, R_two, P_one, P_two, P_nought){

	result = 0.000786245787890816 + 0.010569867897222 * S +
             0.00036133957676203 * P_nought - 
             0.0164374355367147 * A_two * (S <= R_two)

	return(result)
}

dR_one <- function(S, A_one, A_two, R_one, R_two, P_one, P_two, P_nought){

	result = 0.0150444834198657 * A_one - 0.00801099463153149 * R_one

	return(result)
}

dR_two <- function(S, A_one, A_two, R_one, R_two, P_one, P_two, P_nought){

	result = 0.0201543867686343 * A_two + 0.00797996201542356 * R_one

	return(result)
}

dP_one <- function(S, A_one, A_two, R_one, R_two, P_one, P_two, P_nought){

	result = 0

	return(result)
	
}

dP_two <- function(S, A_one, A_two, R_one, R_two, P_one, P_two, P_nought){

  result = 0.0995249856445131 * R_two * 
           (P_two > 0) * (P_nought > 0)

	return(result)
	
}

###############################################################################

###############################################################################
# Comparing results.

# Training data.

data_dir = "~/Documents/Programming/Git_Folders/MSc-Thesis-Code/Submission/1_Analysis_of_Nutonian_Eureqa/3_Pratt_and_Planque_SPACE_Models/Data/Pratt"

setwd(data_dir)

mydata = read.csv("Pratt_Q10.txt")

###############################################################################

###############################################################################
# Running model

t = 0

S = mydata$S[1]
A_one = mydata$A_one[1]
A_two = mydata$A_two[1]
R_one = mydata$R_one[1]
R_two = mydata$R_two[1]
P_zero = mydata$P_nought[1]
P_one = mydata$P_one[1]
P_two = mydata$P_two[1]

# This is the time at which carrying begins. We manually have to seed this for
# the reasons explained in the thesis concerning complexity and accuracy as
# defined by Eureqa.
carry_time = which(mydata$P_two > 0)[1]

while(t < nrow(mydata)){

  S_vec = c(S_vec, S)
  A_one_vec = c(A_one_vec, A_one)
  A_two_vec = c(A_two_vec, A_two)
  R_one_vec = c(R_one_vec, R_one)
  R_two_vec = c(R_two_vec, R_two)
  P_zero_vec = c(P_zero_vec, P_zero)
  P_one_vec = c(P_one_vec, P_one)
  P_two_vec = c(P_two_vec, P_two)

  S = S + dS(S, A_one, A_two, R_one, R_two, P_one, P_two, P_zero)
  A_one = A_one + dA_one(S, A_one, A_two, R_one, R_two, P_one, P_two, P_zero)
  A_two = A_two + dA_two(S, A_one, A_two, R_one, R_two, P_one, P_two, P_zero)
  R_one = R_one + dR_one(S, A_one, A_two, R_one, R_two, P_one, P_two, P_zero)
  R_two = R_two + dR_two(S, A_one, A_two, R_one, R_two, P_one, P_two, P_zero)
  P_one = P_one + dP_one(S, A_one, A_two, R_one, R_two, P_one, P_two, P_zero)

  # Update the different components of the model. If we are at the carry point
  # then we need to seed the model.
  if(t + 1 == carry_time){
    P_two = P_two + dP_two(S, A_one, A_two, R_one, R_two, P_one, mydata$P_two[t + 1], P_zero)
  } else{
    P_two = P_two + dP_two(S, A_one, A_two, R_one, R_two, P_one, P_two, P_zero)
  }

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
plot_result(A1_vec_scaled, A1_scaled, "Number of Assessors (Site One)", 50, 11)
plot_result(A2_vec_scaled, A2_scaled, "Number of Assessors (Site Two)", 50, 16)
plot_result(R1_vec_scaled, R1_scaled, "Number of Recruiters (Site One)", 50, 9.5)
plot_result(R2_vec_scaled, R2_scaled, "Number of Recruiters (Site Two)", 50, 11)
plot_result(P1_vec_scaled, P1_scaled, "Number of Passive (Site One)", 70, 10)
plot_result(P2_vec_scaled, P2_scaled, "Number of Passive (Site Two)", 50, 33)

###############################################################################

###############################################################################
# Errors

create_R_squared_errors()
create_MAE_errors()
create_MSE_errors()
