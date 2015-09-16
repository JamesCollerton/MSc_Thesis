# - James Collerton
# - Student Number 46114
# - Source Code for MSc Thesis

# This is the source code for the experiments done with the Pratt model. The
# model is built and run with the given quorum size and then compared to the
# original model.

###############################################################################
# Setting directories, libraries and reading in files.

library(plyr)

# String to set the directory.
dir_string = "~/Documents/Programming/Git_Folders/MSc-Thesis-Code/Submission/1_Analysis_of_Nutonian_Eureqa/3_Pratt_and_Planque_SPACE_Models/Data/Pratt/Var_Parameters"

setwd(dir_string)

###############################################################################
# Functions

# Calculate R squared values.
R_squared <- function(estimate, real_values){
  
  obs_mean = mean(estimate)
  
  SS_res = sum((estimate - real_values)^2)
  SS_tot = sum((real_values - obs_mean)^2)

  result = 1 - SS_res/SS_tot

  return(result)
}

# Calculate mean squared error values.
MSE <- function(estimate, real_values){
  residuals = real_values - estimate
  return(sum(residuals^2) / length(residuals))
}

# Calculate mean absolute error values.
MAE <- function(estimate, real_values){
  abs_residuals = abs(real_values - estimate)
  return(sum(abs_residuals) / length(abs_residuals))
}

# Scaling vectors so they can be compared.
scale_for_comparison <- function(data){

	sampling = length(data) / 100
	sampled_vec = c()

	for( i in 1: 100 ){ sampled_vec[i] = data[ i * sampling ] }

	return(sampled_vec)
}

# Returns a list of filenames in the set folder.
get_filenames <- function(){

	filenames <- list.files(path = ".", pattern = NULL, all.files = FALSE,
			            full.names = FALSE, recursive = FALSE,
			            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

	return(filenames)
}

# Calculates the R squared vectors for each of the roles in the particular
# simulation. These are later added to a matrix to find the average/ standard
# deviation per role.
add_to_R_sq <- function(){

	vec_vector = data.frame(P_one_vec, P_two_vec, R_one_vec, R_two_vec, A_one_vec, A_two_vec, S_vec)

	for(i in 1: length(vec_vector)){ 
		R_sq_errors = c(R_sq_errors, R_squared(mydata[[i + 1]], vec_vector[[i]] ) ) 
	}

	return(R_sq_errors)

}

# Same as the above but for the MAE.
add_to_MAE <- function(){

	vec_vector = data.frame(P_one_vec, P_two_vec, R_one_vec, R_two_vec, A_one_vec, A_two_vec, S_vec)

	for(i in 1: length(vec_vector)){ 
		MAE_errors = c(MAE_errors, MAE(mydata[[i + 1]], vec_vector[[i]] ) ) 
	}

	return(MAE_errors)

}

# Same as the above but for the MSE.
add_to_MSE <- function(){

	vec_vector = data.frame(P_one_vec, P_two_vec, R_one_vec, R_two_vec, A_one_vec, A_two_vec, S_vec)

	for(i in 1: length(vec_vector)){ 
		MSE_errors = c(MSE_errors, MSE(mydata[[i + 1]], vec_vector[[i]] ) ) 
	}

	return(MSE_errors)

}

# This takes the vector of errors/ fits and turns it into a data frame for
# all of the roles in order to aggregate values.
create_error_df <- function(error_vec){

	error_mat = matrix(error_vec, ncol = 7, byrow = TRUE)
	error_df = data.frame(error_mat)
	new_names <- c("X1" = "P1", "X2" = "P2", "X3" = "R1", "X4" = "R2", "X5" = "A1",
				   "X6" = "A2", "X7" = "S")
	error_df <- rename(error_df, new_names)	

	return(error_df)
}

# The valid terms are used to discount some values for P1 and P2 where
# no ants are taken to the respective sites, and so the values remain at zero.
# This then means that the R squared value comes back as undefined.

create_ave_err <- function(error_mat){

  valid_P1 = which(R_sq_mat$P1 > 0)
  valid_P2 = which(R_sq_mat$P2 > 0)

	ave_err = c( S = mean(error_mat$S),
				 A1 = mean(error_mat$A1),
				 A2 = mean(error_mat$A2),
				 R1 = mean(error_mat$R1),
				 R2 = mean(error_mat$R2),
				 P1 = mean(error_mat$P1[valid_P1]),
				 P2 = mean(error_mat$P2[valid_P2]) )

	return(ave_err)

}

# Creates the variance of error/ fit per role and returns it as a vector.
create_var_mat <- function(error_mat){
  
  valid_P1 = which(R_sq_mat$P1 > 0)
  valid_P2 = which(R_sq_mat$P2 > 0)
  
  ave_err = c( S = var(error_mat$S),
               A1 = var(error_mat$A1),
               A2 = var(error_mat$A2),
               R1 = var(error_mat$R1),
               R2 = var(error_mat$R2),
               P1 = var(error_mat$P1[valid_P1]),
               P2 = var(error_mat$P2[valid_P2]) )
  
  return(ave_err)
  
}

###############################################################################

###############################################################################
# Differential functions

dS <- function(S, A1, A2, R1, R2, P1, P2, P0){

	result = -2.00429489353722*S*mu - 1.02063315327971*R1*lmbda*
			 (S > 0)*(R1 < Q) - 1.06883601580764*R2*lmbda*(S > 0)*
			 (1.04193165476242*R2 <= Q)

	return(result)
}

dA_one <- function(S, A1, A2, R1, R2, P1, P2, P0){

	result = S*mu - A1*k_one - A1*
			 (
			 	((Q + k_one - A1*k_one) < R1) * p_onetwo +
			 	((Q + k_one - A1*k_one) >= R1) * (p_onetwo - S*mu*(P1 > 0) - 0.625651652906761*k_one*lmbda*t*(S>0))
			 )

	return(result)
}

dA_two <- function(S, A1, A2, R1, R2, P1, P2, P0){

	result = S*mu + A1*p_onetwo + S*mu*p_onetwo*(P2 < p_onetwo) + 
			 1.04024588951255*R2*lmbda*(P2 < 0.0011177663799438*S) + 
			 lmbda^2*(P2 < 0.0011177663799438*S) - 1.00307174147254*A2*k_two
  
	return(result)
}

dR_one <- function(S, A1, A2, R1, R2, P1, P2, P0){

	result = A1*k_one - R1*p_onetwo

	return(result)
}

dR_two <- function(S, A1, A2, R1, R2, P1, P2, P0){

	result = R1*p_onetwo + A2*k_two

	return(result)
}

dP_one <- function(S, A1, A2, R1, R2, P1, P2, P0){

	result = (P0 > 0) * (R1 * phi * (Q <= R1)) 

	return(result)
	
}

dP_two <- function(S, A1, A2, R1, R2, P1, P2, P0){

    result = (P0 > 0) * (R2 * phi * (Q <= R2))

	return(result)
	
}

###############################################################################
# Global Vars.

filenames = get_filenames()

# Initialising the error vectors in order to store values in them.
R_sq_errors = c()
MAE_errors = c()
MSE_errors = c()

###############################################################################
# Running models

for(single_file in filenames){

	# Check to make sure we're not trying to scan in an invalid file for a training
	# data file.
	if(single_file != "rearrange_output.R" && single_file != "var_simulation_results.txt"){

		###############################################################################
		# Global Variables

		mydata = read.csv(single_file)

		# Set the parameters as in the training data.

		N = mydata$N[1]
		p = mydata$p[1]
		Q = mydata$Q[1]
		lmbda = mydata$lmbda[1]
		phi = mydata$phi[1]
		mu = mydata$mu[1]
		k_one = mydata$k_one[1]
		k_two = mydata$k_two[1]
		p_onetwo = mydata$p_onetwo[1]
		c = mydata$c[1]
		total_passive = mydata$total_passive[1]

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
		# Comparing results.

		# Training data.

		mydata = read.csv(single_file)

		###############################################################################

		###############################################################################
		# Running model

		# Set initial conditions the same as in the underlying model.
		t = 0

		S = mydata$S[1]
		A_one = mydata$A_one[1]
		A_two = mydata$A_two[1]
		R_one = mydata$R_one[1]
		R_two = mydata$R_two[1]
		P_zero = mydata$P_nought[1]
		P_one = mydata$P_one[1]
		P_two = mydata$P_two[1]

		while(t < nrow(mydata)){

			S_vec = c(S_vec, S)
			A_one_vec = c(A_one_vec, A_one)
			A_two_vec = c(A_two_vec, A_two)
			R_one_vec = c(R_one_vec, R_one)
			R_two_vec = c(R_two_vec, R_two)
			P_one_vec = c(P_one_vec, P_one)
			P_two_vec = c(P_two_vec, P_two)

			S = S + dS(S, A_one, A_two, R_one, R_two, P_one, P_two, P_zero)
			A_one = A_one + dA_one(S, A_one, A_two, R_one, R_two, P_one, P_two, P_zero) 
			A_two = A_two + dA_two(S, A_one, A_two, R_one, R_two, P_one, P_two, P_zero)
			R_one = R_one + dR_one(S, A_one, A_two, R_one, R_two, P_one, P_two, P_zero)
			R_two = R_two + dR_two(S, A_one, A_two, R_one, R_two, P_one, P_two, P_zero)
			P_one = P_one + dP_one(S, A_one, A_two, R_one, R_two, P_one, P_two, P_zero)
			P_two = P_two + dP_two(S, A_one, A_two, R_one, R_two, P_one, P_two, P_zero)

			t = t + 1
		}

		# For every run through we do we add the R squared, MAE and MSE to vectors
		# in order to later average them.
		R_sq_errors = add_to_R_sq()
		MAE_errors = add_to_MAE()
		MSE_errors = add_to_MSE()

	}

}

################################################################################
# Create matrices out of the error values and print them to screen.

R_sq_mat = create_error_df(R_sq_errors)
MAE_mat = create_error_df(MAE_errors)
MSE_mat = create_error_df(MSE_errors)

create_ave_err(R_sq_mat)
create_ave_err(MAE_mat)
create_ave_err(MSE_mat)

R_sq_var = create_var_mat(R_sq_mat)
MAE_var = create_var_mat(MAE_mat)
MSE_var = create_var_mat(MSE_mat)

# Square root the variances to find the standard deviations.
sqrt(R_sq_var)
sqrt(MAE_var)
sqrt(MSE_var)

###############################################################################