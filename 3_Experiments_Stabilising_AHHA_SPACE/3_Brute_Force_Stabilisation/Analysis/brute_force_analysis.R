# - James Collerton
# - Student Number 46114
# - Source Code for MSc Thesis

# This script runs the quantitative analysis on the output of the SPACE model
# stabilised using brute force. It reads in everything and then calculates
# R squared, MAE and MSE.

###############################################################################
# Reading in the data.

setwd("~/Documents/Programming/Git_Folders/MSc-Thesis-Code/Submission/3_Experiments_Stabilising_AHHA_SPACE/3_Brute_Force_Stabilisation/Analysis/Stable_Data")

first_data = read.csv("final_combination.csv")

setwd("~/Documents/Programming/Git_Folders/MSc-Thesis-Code/Submission/3_Experiments_Stabilising_AHHA_SPACE/3_Brute_Force_Stabilisation/Analysis/Unstable_SPACE_Data")

comparison_data = read.csv("Comparison_Data.txt")
stable_data = read.csv("stable_data.txt")
unstable_data = read.csv("unstable_data.txt")

compare_seq = seq(from = 0, to = length(comparison_data$S), by = length(comparison_data$S) / 100)
stable_unstable_compare_seq = seq(from = 0, to = length(stable_data$S), by = length(stable_data$S) / 100)

###############################################################################
# Functions for analysis

# Calculating the R squared
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

# Creates a vector of R squared values so it's easy to read.
create_R_sq_vec <- function(){

	c(S_Rsq = R_squared(first_data$S.Underlying, first_data$S.Approximation),
	  A1_Rsq = R_squared(first_data$A1.Underlying, first_data$A1.Approximation),
	  A2_Rsq = R_squared(first_data$A2.Underlying, first_data$A2.Approximation),
	  R1_Rsq = R_squared(first_data$R1.Underlying, first_data$R1.Approximation),
	  R2_Rsq = R_squared(first_data$R2.Underlying, first_data$R2.Approximation),
	  P1_Rsq = R_squared(first_data$P1.Underlying, first_data$P1.Approximation),
	  P2_Rsq = R_squared(first_data$P2.Underlying, first_data$P2.Approximation))

}

# Creates a vector of Mean Absolute Error values so it's easy to read.
create_MAE_vec <- function(){

	c(S_MAE = MAE(first_data$S.Underlying, first_data$S.Approximation),
	  A1_MAE = MAE(first_data$A1.Underlying, first_data$A1.Approximation),
	  A2_MAE = MAE(first_data$A2.Underlying, first_data$A2.Approximation),
	  R1_MAE = MAE(first_data$R1.Underlying, first_data$R2.Approximation),
	  R2_MAE = MAE(first_data$R2.Underlying, first_data$R2.Approximation), 
	  P1_MAE = MAE(first_data$P1.Underlying, first_data$P1.Approximation),
	  P2_MAE = MAE(first_data$P2.Underlying, first_data$P2.Approximation))

}

# Creates a vector of Mean Squared Error values so it's easy to read.
create_MSE_vec <- function(){

	c(S_MSE = MSE(first_data$S.Underlying, first_data$S.Approximation),
	  A1_MSE = MSE(first_data$A1.Underlying, first_data$A1.Approximation),
	  A2_MSE = MSE(first_data$A2.Underlying, first_data$A2.Approximation),
	  R1_MSE = MSE(first_data$R1.Underlying, first_data$R2.Approximation),
	  R2_MSE = MSE(first_data$R2.Underlying, first_data$R2.Approximation), 
	  P1_MSE = MSE(first_data$P1.Underlying, first_data$P1.Approximation),
	  P2_MSE = MSE(first_data$P2.Underlying, first_data$P2.Approximation))
	
}

# Plotting the results to give a visual demonstration.
plot_result <- function(underlying, ylabel, ylimit, approximation, leg_x, leg_y, unstable_data, perfect_info){

	i = 0

	par(mfrow = c(1,2))

	plot(underlying, type = "l", col = "firebrick1", lwd = 2, ylim = ylimit,
	     xlab= "Percentage through emigration", ylab = ylabel)
	lines(approximation, col = "dodgerblue", lwd = 2)
	lines(unstable_data, col = "seagreen1", lwd = 2)

	legend(leg_x, leg_y,
	   	   c("Underlying", "Stabilised", "Unstabilised"),
	       lty=c(1, 1, 1),
	       lwd=c(2, 2, 2),
	       col=c("dodgerblue", "firebrick1", "lightgreen"),
	       cex = 0.8)

	plot(underlying - approximation, type = "l", col = "red", lwd = 2,
		 xlab = "Time", ylab = "Difference in Functions")
}

###############################################################################
# Running the functions. First plots them then calculates the errors and fit.

plot_result(first_data$S.Underlying, "Number of Scouts", c(4, 43), first_data$S.Approximation, 0, 12, unstable_data$S[stable_unstable_compare_seq], stable_data$S[stable_unstable_compare_seq])
plot_result(first_data$A1.Underlying, "Number of Assessors (Site One)", c(0, 6), first_data$A1.Approximation, 0, 6, unstable_data$A1[stable_unstable_compare_seq], stable_data$A1[stable_unstable_compare_seq])
plot_result(first_data$A2.Underlying, "Number of Assessors (Site Two)", c(0, 5.5), first_data$A2.Approximation, 0, 5.5, unstable_data$A2[stable_unstable_compare_seq], stable_data$A2[stable_unstable_compare_seq])
plot_result(first_data$R1.Underlying, "Number of Recruiters (Site One)", c(0, 11.5), first_data$R1.Approximation, 0, 11.5, unstable_data$R1[stable_unstable_compare_seq], stable_data$R1[stable_unstable_compare_seq])
plot_result(first_data$R2.Underlying, "Number of Recruiters (Site Two)", c(0, 30), first_data$R2.Approximation, 0, 30, unstable_data$R2[stable_unstable_compare_seq], stable_data$R2[stable_unstable_compare_seq])
plot_result(first_data$P1.Underlying, "Number of Passive (Site One)", c(-0.2, 1.5), first_data$P1.Approximation, 0, 1.5, unstable_data$P1[stable_unstable_compare_seq], stable_data$P1[stable_unstable_compare_seq])
plot_result(first_data$P2.Underlying, "Number of Passive (Site Two)", c(0, 125), first_data$P2.Approximation, 0, 125, unstable_data$P2[stable_unstable_compare_seq], stable_data$P2[stable_unstable_compare_seq])

###############################################################################
# Calculating the fit and error.

create_R_sq_vec()
create_MAE_vec()
create_MSE_vec()

###############################################################################
