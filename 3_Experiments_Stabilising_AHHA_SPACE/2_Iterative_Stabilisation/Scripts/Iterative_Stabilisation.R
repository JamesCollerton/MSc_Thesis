# - James Collerton
# - Student Number 46114
# - Source Code for MSc Thesis

# Stabilising the SPACE model by iteration. This script requires some manual
# work as it is quicker to manually alter this script and use the results with
# the Eureqa desktop application than to try and automate the whole pipeline
# using the Eureqa API. Also, the Eureqa API is fairly limited in what it can
# be applied to as it is limited in the number of variables/ amount of data
# it can take in.

###############################################################################
# Global Variables and settings.

# Setting the current directory to where we have stored our data.

setwd("~/Documents/Programming/Git_Folders/MSc-Thesis-Code/Submission/3_Experiments_Stabilising_AHHA_SPACE/2_Iterative_Stabilisation/Data")

# Later this will be filled with the new, stable components.

new_data = list()

# Read these in either for comparison or to form part of the model. 
# We need unstable_df and data as the same thing, as one will be overwritten
# throughout the program, whilst the other will not.

unstable_df = read.csv("unstable_data.txt")
comparison_data = read.csv("Training_Data.txt")
data = read.csv("unstable_data.txt")

# Initial conditions.

N = 200
Q = data$Q[1]
initial_scouts = data$S[1]
total_passive = N - initial_scouts

# Indicators for the data vectors.

T_ind = 1
S_ind = 2 
P0_ind = 3
P1_ind = 4
P2_ind = 5
A1_ind = 6
A2_ind = 7
R1_ind = 8
R2_ind = 9

###############################################################################
# Initialising

# Initial conditions. These have been hardcoded for the single trial, but
# could easily be changed to data$S[1], data$A1[1] etc. to allow flexibility.

S = initial_scouts
A_one = 0
A_two = 0
R_one = 0
R_two = 0
P_zero = total_passive
P_one = 0
P_two = 0

# Initialising empty vectors to store the results in.

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

dS <- function(S, t, Q, N){

	result = 1.06669377213794e-9*t^3 - 0.0130383120110502 - 
			 0.000168544171939409*t - 6.65198441305169e-13*t^4 - 
			 2.20232908623747e-7*t^2

	return(result)
}

dA_one <- function(S, P1, P2, R2, t, Q, N){

	result = 0.00231744609341487*S + 0.942990934721937*R2*
			 (
			 	(R2 > 0) * 0.00157400238903202 +
			 	(R2 <= 0) * cos(0.855282848706793*S)
			 ) - 
			 0.0434850619886643 - 2.33968166891488e-5*S^2 - 
			 0.00563945347163448*sin(5.19729925857688 + 0.364203873387924*S)

	return(result)
}

dA_two <- function(S, P1, P2, A1, t, Q, N){

	result = 0.000282667076235126*A1 + 0.0950374171430637/S +
			 0.000297132846101472*A1*P2 + 0.030200635796381*
			 (10.6088331903667 < S) - 0.0249260226030619 - 
			 0.000370254067916185*P2 - 0.0042652733456114*P2*
			 (10.6088331903667 < S) - 7.45572148229618e-6*S*P2*A1^2

	return(result)
}

dR_one <- function(S, A1, A2, P1, P2, t, Q, N){

	result = 0.0266282174935782 + 0.009329126372239*S + 
			 0.0231123108033275*(P1 > 0) - 6.99557347486849e-5*t - 
			 0.00535770880365551*A1 - 0.000343308507165349*S*A1 - 
			 0.000231772856346287*S^2

	return(result)
}

dR_two <- function(S, A1, A2, P1, P2, R1, t, Q, N){

	result = 0.00410346545735221*S - 0.0110700090049996 - 
			 9.0219126132179e-5*
			 (
			 	(S <= 6.52877096324257) * (-75.5959114648096) +
			 	(S > 6.52877096324257) * S
			 )*
			 (	
			 	(0.425348476983269 < P1) * 1.92739381263045 +
			 	(0.425348476983269 >= P1) * (S - 9.6421558706767*P1)
			 )

	return(result)
}

dP_one <- function(S, P2, t, Q, N){

	result = 0.00011585819289194*P2*
			 (72.3159612664517 <= P2)*cos(-0.0123745627255911*t)


	return(result)
	
}

dP_two <- function(S, t, Q, N){

	result = 0.00235796547198259*t*exp(-0.255561049906936*S) - 
			 cos(S)*exp(-0.377989973882601*S)

	return(result)
	
}

###############################################################################
# Updating functions.

# These functions are used to update the data vector to be written back in.
# The idea is that each time we run the model, we stabilise a different component
# (ex the number of scouts) and then use that stable part in declaring the
# next function in Eureqa.

update_S <- function(S_vec, data){

	data$S <- S_vec

	return(data)
}

update_P2 <- function(P2_vec, data){

	data$P2 <- P2_vec

	return(data)
}

update_P1 <- function(P1_vec, data){

	data$P1 <- P1_vec

	return(data)
}

update_A1 <- function(A1_vec, data){

	data$A1 <- A1_vec

	return(data)
}

update_A2 <- function(A2_vec, data){

	data$A2 <- A2_vec

	return(data)
}

update_R1 <- function(R1_vec, data){

	data$R1 <- R1_vec

	return(data)
}

update_R2 <- function(R2_vec, data){

	data$R2 <- R2_vec

	return(data)
}

# This updates the training data file. For example, lets say that at the start
# we have the variable S, and we define a function dS/dt = g(t, Q, N), then 
# we know that each time we run the simulation the S values won't change. We then
# update the training data with these new stable S values, and build the next term
# on top of that. So dA1/dt = h(S, t, Q, N).

write_new_file <- function(new_data){

	setwd("~/Documents/Programming/Git_Folders/MSc-Thesis-Code/Submission/3_Experiments_Stabilising_AHHA_SPACE/2_Iterative_Stabilisation/Data")

	S = c()
	P0 = c()
	P1 = c()
	P2 = c()
	A1 = c()
	A2 = c()
	R1 = c()
	R2 = c()
	N = c()
	Q = c()
	t = c()

	for(single_file in new_data){

		S = c(S, single_file$S)
		P0 = c(P0, single_file$P0)
		P1 = c(P1, single_file$P1)
		P2 = c(P2, single_file$P2)
		A1 = c(A1, single_file$A1)
		A2 = c(A2, single_file$A2)
		R1 = c(R1, single_file$R1)
		R2 = c(R2, single_file$R2)
		N = c(N, single_file$N)
		Q = c(Q, single_file$Q)
		t = c(t, single_file$t)

	}

	write_df <- data.frame(S, A1, A2, P0, P1, P2, R1, R2, Q, t, N)

	write.csv(write_df, "new_data.txt", row.names = FALSE)

}

###############################################################################
# Running model with the underlying data included

t = 1

while(t <= nrow(data)){

	S_vec = c(S_vec, S)
	A_one_vec = c(A_one_vec, A_one)
	A_two_vec = c(A_two_vec, A_two)
	R_one_vec = c(R_one_vec, R_one)
	R_two_vec = c(R_two_vec, R_two)
	P_one_vec = c(P_one_vec, P_one)
	P_two_vec = c(P_two_vec, P_two)

	S = S + dS(S, t, Q, N)
	A_one = A_one + dA_one(S, P_one, P_two, R_two, t, Q, N)
	A_two = A_two + dA_two(S, P_one, P_two, A_one, t, Q, N)
	R_one = R_one + dR_one(S, A_one, A_two, P_one, P_two, t, Q, N)
	R_two = R_two + dR_two(S, A_one, A_two, P_one, P_two, R_one, t, Q, N)
	P_one = P_one + dP_one(S, P_two, t, Q, N)
	P_two = P_two + dP_two(S, t, Q, N)
  
    t = t + 1
}

# Plotting the results

plot_result <- function(vec, compare_data, unstable_data, y_label, leg_x, leg_y){

	par(mfrow = c(1,2))

	plot(compare_data, type = "l", col = "firebrick1", xlab = "Time", 
			  ylab = y_label, lwd = 2)
	lines(vec, col = "dodgerblue", lwd = 2)
	lines(unstable_data, col = "lightgreen", lwd = 2)

	legend(leg_x, leg_y,
	   	   c("Underlying", "Stabilised", "Unstabilised"),
	       lty=c(1, 1, 1),
	       lwd=c(2, 2, 2),
	       col=c("dodgerblue", "firebrick1", "lightgreen"),
	       cex = 0.8)

	plot(compare_data - vec, type = "l", col = "red", lwd = 2.5,
		 xlab = "Time", ylab = "Difference in Functions")

}

# Calculating the R squared value.

R_squared <- function(estimate, real_values){
  
  obs_mean = mean(estimate)
  
  SS_res = sum((estimate - real_values)^2)
  SS_tot = sum((real_values - obs_mean)^2)

  return(1 - SS_res/SS_tot)

}

# Mean squared error
MSE <- function(estimate, real_values){

  residuals = real_values - estimate
  return(sum(residuals^2) / length(residuals))

}

# Mean Absolute Error
MAE <- function(estimate, real_values){

  abs_residuals = abs(real_values - estimate)
  return(sum(abs_residuals) / length(abs_residuals))

}

# Creating vector of the R squared values.
create_Rsq_vec <- function(){

  c(S_rsq = R_squared(S_vec, comparison_data$S),
  A1_rsq = R_squared(A_one_vec, comparison_data$A1),
  A2_rsq = R_squared(A_two_vec, comparison_data$A2),
  R1_rsq = R_squared(R_one_vec, comparison_data$R1),
  R2_rsq = R_squared(R_two_vec, comparison_data$R2),
  P1_rsq = R_squared(P_one_vec, comparison_data$P1),
  P2_rsq = R_squared(P_two_vec, comparison_data$P2))

}

# Creating vector of the MAE values.
create_MAE_vec <- function(){

  c(S_MAE = MAE(S_vec, comparison_data$S),
  A1_MAE = MAE(A_one_vec, comparison_data$A1),
  A2_MAE = MAE(A_two_vec, comparison_data$A2),
  R1_MAE = MAE(R_one_vec, comparison_data$R1),
  R2_MAE = MAE(R_two_vec, comparison_data$R2),
  P1_MAE = MAE(P_one_vec, comparison_data$P1),
  P2_MAE = MAE(P_two_vec, comparison_data$P2))
	
}

# Creating vector of the MSE values.
create_MSE_vec <- function(){

  c(S_MSE = MSE(S_vec, comparison_data$S),
  A1_MSE = MSE(A_one_vec, comparison_data$A1),
  A2_MSE = MSE(A_two_vec, comparison_data$A2),
  R1_MSE = MSE(R_one_vec, comparison_data$R1),
  R2_MSE = MSE(R_two_vec, comparison_data$R2),
  P1_MSE = MSE(P_one_vec, comparison_data$P1),
  P2_MSE = MSE(P_two_vec, comparison_data$P2))
	
}

###############################################################################
# Running the plotting of the results.

plot_result(S_vec, comparison_data$S, unstable_df$S, "Number of Scouts", 0, 14)
plot_result(A_one_vec, comparison_data$A1, unstable_df$A1, "Number of Assessors (Site One)", 0, 5.6)
plot_result(A_two_vec, comparison_data$A2, unstable_df$A2, "Number of Assessors (Site Two)", 0, 4.6)
plot_result(R_one_vec, comparison_data$R1, unstable_df$R1, "Number of Recruiters (Site One)", 0, 10.4)
plot_result(R_two_vec, comparison_data$R2, unstable_df$R2, "Number of Recruiters (Site Two)", 0, 27.5)
plot_result(P_one_vec, comparison_data$P1, unstable_df$P1, "Number of Passive (Site One)", 0, 1.5)
plot_result(P_two_vec, comparison_data$P2, unstable_df$P2, "Number of Passive (Site Two)", 0, 133)

###############################################################################
# Updating the values and preparing the new data vector to be written.

data = update_S(S_vec, data)
data = update_P2(P_two_vec, data)
data = update_P1(P_one_vec, data)
data = update_A1(A_one_vec, data)
data = update_A2(A_two_vec, data)
data = update_R1(R_one_vec, data)
data = update_R2(R_two_vec, data)

new_data[[length(new_data) + 1]] <- data

###############################################################################
# Calculating the R squared values, MAE and MSE.

create_Rsq_vec()
create_MAE_vec()
create_MSE_vec()

###############################################################################

###############################################################################
# Write new files

# write_new_file(new_data)

###############################################################################
