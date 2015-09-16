# - James Collerton
# - Student Number 46114
# - Source Code for MSc Thesis

# This is the code for the Planque model with randomly generated parameters.
# The model is built and run for each of the original simulations and then
# compared to the training data.

###############################################################################
# Setting directories, libraries and reading in files.

library("plyr")

# String to set the directory.
dir_string = "~/Documents/Programming/Git_Folders/MSc-Thesis-Code/Submission/1_Analysis_of_Nutonian_Eureqa/3_Pratt_and_Planque_SPACE_Models/Data/Planque/Var_Parameters"

setwd(dir_string)

# Creates a list of all filenames in the directory.
filenames <- list.files(path = ".", pattern = NULL, all.files = FALSE,
			            full.names = FALSE, recursive = FALSE,
			            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

###############################################################################
# Functions

# Calculating the R squared value between two vectors.
R_squared <- function(estimate, real_values){
  
  obs_mean = mean(estimate)
  
  SS_res = sum((estimate - real_values)^2)
  SS_tot = sum((real_values - obs_mean)^2)

  result = 1 - SS_res/SS_tot

  return(result)
}

# Calculating the mean squared error.
MSE <- function(estimate, real_values){

  residuals = real_values - estimate
  return(sum(residuals^2) / length(residuals))

}

# Calculating the mean absolute error.
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

# Calculates the R squared vectors for each of the roles in the particular
# simulation. These are later added to a matrix to find the average/ standard
# deviation per role.
add_to_R_sq <- function(){

	vec_vector = data.frame(S_vec, A_vec, R_vec, P_vec, C_vec)

	for(i in 1: length(vec_vector)){ 
		R_sq_errors = c(R_sq_errors, R_squared(mydata[[i]], vec_vector[[i]] ) ) 
	}

	return(R_sq_errors)

}

# Same as the above but for the MAE.
add_to_MAE <- function(){

	vec_vector = data.frame(S_vec, A_vec, R_vec, P_vec, C_vec)

	for(i in 1: length(vec_vector)){ 
		MAE_errors = c(MAE_errors, MAE(mydata[[i]], vec_vector[[i]] ) ) 
	}

	return(MAE_errors)

}

# Same as the above but for the MSE.
add_to_MSE <- function(){

	vec_vector = data.frame(S_vec, A_vec, R_vec, P_vec, C_vec)

	for(i in 1: length(vec_vector)){ 
		MSE_errors = c(MSE_errors, MSE(mydata[[i]], vec_vector[[i]] ) ) 
	}

	return(MSE_errors)

}

# This takes the vector of errors/ fits and turns it into a data frame for
# all of the roles in order to aggregate values.
create_error_df <- function(error_vec){

	error_mat = matrix(error_vec, ncol = 5, byrow = TRUE)
	error_df = data.frame(error_mat)
	new_names <- c("X1" = "S", "X2" = "A", "X3" = "R", "X4" = "P", "X5" = "C")
	error_df <- rename(error_df, new_names)	

	return(error_df)
}

# Takes the average error for each of the simulations. As parameter values are
# sampled randomly, some of the combinations lead to undefined behaviours. 
# therefore we discount the 8th trial.
create_ave_err <- function(error_mat){

	ave_err = c( S = mean(error_mat$S[-8]),
				 A = mean(error_mat$A[-8]),
				 R = mean(error_mat$R[-8]),
				 P = mean(error_mat$P[-8]),
				 C = mean(error_mat$C[-8]) )

	return(ave_err)

}

# Creates the variance of error/ fit per role and returns it as a vector.
create_var_mat <- function(error_mat){
  
  ave_err = c( S = var(error_mat$S[-8]),
               A = var(error_mat$A[-8]),
               R = var(error_mat$R[-8]),
               P = var(error_mat$P[-8]),
               C = var(error_mat$C[-8]) )
  
  return(ave_err)
  
}

###############################################################################

###############################################################################
# Differential functions

dS <- function(S, A, R, P, C){

	result = A*mu - 8.08569237336037e-5 - S*kappa - 
			 A*mu^2 - R*uc_F*lc_f*lmda*(C > 0) - 
			 0.361422636566558*lmda*(C > 0) - 
			 2.91548952513461*R*kappa*
			 (
			 	(C > 0) +
			 	(C <= 0) * A * mu
			 )

	return(result)
}

dA <- function(S, A, R, P, C){

	result = A*uc_F*mu/
			 (0.0436378475320125*R*
			 (
			 	(C > 0) * -8.01766063390002e-5 + 
			 	(C <= 0) * (0.658664991264159 - 0.092306569709023*A*mu)
			 ) - uc_F)

	return(result)
}

dR <- function(S, A, R, P, C){

	result = S*kappa + 0.000348930918408604*
			 (
			 	((
			 		(C > 0) * lc_f +
			 		(C <= 0) * (2.12068695907581*R <= A)
			 	) > 0) * -0.561000833593849 +
			 	((
			 		(C > 0) * lc_f +
			 		(C <= 0) * (2.12068695907581*R <= A)
			 	) <= 0) * 112.193237655697*A) + S*R*lmda*
			 	(
			 		(C > 0) * lc_f +
			 		(C <= 0) * (2.12068695907581*R <= A)
			 	)/(S + R)

	
	return(result)
}

dP <- function(S, A, R, P, C){

	result = 1.00635793458105*R*P*
			 (
			 	(C > 0) * (0.00112430078384966 - phi) +
			 	(C <= 0) * C
			 )/
			 (R + P + R*lc_f + P*lc_f + R*lc_f^2 + 1.61964735100976*R*P*lc_f^4)

	return(result)
}

dC <- function(S, A, R, P, C){

	result = 0.88036819563391*S*R*P*phi*(C > 0)/(S*P + 1.10949091025353*S*R - P)

	return(result)
}

###############################################################################

# Initialising the error vectors in order to store values in them.
R_sq_errors = c()
MAE_errors = c()
MSE_errors = c()

################################################################################
# Running the program

# We loop through all of the files in the training data and test the derived model
# against the training data.

for(single_file in filenames){

	# Check to make sure we're not trying to scan in an invalid file for a training
	# data file.
	if(single_file != "rearrange_output.R" && single_file != "var_simulation_results.txt"){

	###############################################################################
	# Global Variables

	mydata = read.csv(single_file)

	# Set the parameters as in the training data.

	N = mydata$N[1]
	uc_F = mydata$uc_F[1]					# Fraction of active ants, range: [0.05, 0.5]
	lc_f = mydata$lc_f[1] 					# Fraction of post-quorum RTR time.
	mu = mydata$mu[1] 						# Rate active ants at old nest become scouts
	lmda = mydata$lmda[1]					# Rate at which ants following TR become recruiters
	phi = mydata$phi[1] 					# Rate at which passive ants carried to new nest.
	kappa = mydata$kappa[1] 				# Rate at which scouts indep become recruiters.
	epsilon = mydata$epsilon[1]				# Used to avoid singularities in denominators

	###############################################################################
	# Initialising the vectors for storing the results.

	S_vec = c()
	A_vec = c()
	R_vec = c()
	P_vec = c()
	C_vec = c()

	###############################################################################

	###############################################################################
	# Running model

	# Set initial conditions the same as in the underlying model.
	t = 0

	A = mydata$A[1]
	S = mydata$S[1]
	R = mydata$R[1]
	P = mydata$P[1]
	C = mydata$C[1]

	# Have to seed the carry time for the reasons as explained in the thesis.
	carry_time = which(mydata$C > 0)[1]

	while(t < nrow(mydata)){

		S_vec = c(S_vec, S)
		A_vec = c(A_vec, A)
		R_vec = c(R_vec, R)
		P_vec = c(P_vec, P)
		C_vec = c(C_vec, C)

	    S = S + dS(S, A, R, P, C)
	    A = A + dA(S, A, R, P, C)
	    R = R + dR(S, A, R, P, C)
	    P = P + dP(S, A, R, P, C)

		if(t + 1 == carry_time){
			C = C + dC(S, A, R, P, mydata$C[ t + 1 ])
		} else{
			C = C + dC(S, A, R, P, C)
		}

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

################################################################################