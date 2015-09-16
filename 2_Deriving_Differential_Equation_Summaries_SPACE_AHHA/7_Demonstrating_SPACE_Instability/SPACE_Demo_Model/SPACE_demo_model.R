# - James Collerton
# - Student Number 46114
# - Source Code for MSc Thesis

# This is the source code for demonstrating the instability of the Eureqa
# derived SPACE model. The model is built and run as defined by Eureqa,
# and we see that, due to the higher complexity of the model, that interactions
# between variables means that small errors propagate over the whole model
# and it becomes unstable.

###############################################################################
# Reading in data

# Training data for comparison.

setwd("~/Documents/Programming/Git_Folders/MSc-Thesis-Code/Submission/2_Deriving_Differential_Equation_Summaries_SPACE_AHHA/7_Demonstrating_SPACE_Instability/SPACE_Training_Set")

data = read.csv("training_data.txt")

###############################################################################
# Global Variables

# Assigning the values of the variables to the same values as in the training
# data.

N = data$N[1]
Q = data$Q[1]
initial_scouts = data$S[1]

# Indicators for positions in the input array.

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

# S, A_one, A_two... are the initial conditions for the unstable model entirely
# built from scratch. The 'perf' versions are then the ones with perfect info
# to demonstrate that the error is indeed caused by the interaction between vars.

S = initial_scouts
A_one = data$A1[1]
A_two = data$A2[1]
R_one = data$R1[1]
R_two = data$R2[1]
P_zero = data$P0[1]
P_one = data$P1[1]
P_two = data$P2[1]

S_perf = initial_scouts
A_one_perf = data$A1[1]
A_two_perf = data$A2[1]
R_one_perf = data$R1[1]
R_two_perf = data$R2[1]
P_one_perf = data$P1[1]
P_two_perf = data$P2[1]

# Empty vectors being initialised for storing the results from the model. Again
# the perf versions are for the models with perfect information.

S_vec = c()
A_one_vec = c()
A_two_vec = c()
R_one_vec = c()
R_two_vec = c()
P_zero_vec = c()
P_one_vec = c()
P_two_vec = c()

S_vec_perf = c()
A_one_vec_perf = c()
A_two_vec_perf = c()
R_one_vec_perf = c()
R_two_vec_perf = c()
P_zero_vec_perf = c()
P_one_vec_perf = c()
P_two_vec_perf = c()

###############################################################################
# Differential functions

dS <- function(S, A1, A2, R1, R2, P0, P1, P2){

	result = 11.5227583929433 + 0.0437724807782926*S - 
			 0.000187222875738185*P2 - 0.000324995337728485*P0 - 
			 0.0145130055327213*A1 - 0.0882596154864457*R1 - 
			 22.7947102803363*cos(1.04568095806253 + 0.00458171465753293*R1 - 0.00237192591526553*S)


	return(result)
}

dA_one <- function(S, A1, A2, R1, R2, P0, P1, P2){

	result = 0.00118687744325149*A2 + 0.000148619789374766*S +
			 (
			 	(P1 > 0) * 
			 		(
			 			(R2 > 0) * (0.00208910615567274*P1 + 0.000202824715649984*S) +
			 			(R2 <= 0) * -0.00637010844307502
			 		) +
			 	(P1 <= 0) * 0.000961952772237083*A2*sin(P0)
			 ) - 0.000319104604375733*A2*P2 - 0.000148619789374766*S*sin(P0)*(P2 == P1)



	return(result)
}

dA_two <- function(S, A1, A2, R1, R2, P0, P1, P2){

	result = 0.00777766418054526*P2 + 0.003407467603301*P0 + 0.00260838557334383*R1 - 
			 0.533023206147934 - 0.0144795263306038*P1 - 0.00064356871906768*S*
			 (
				(R1 < P1) * 0.191764355422804 +
				(R1 >= P1) * 
					(
						(S*P2*R1 > (0.00777766418054526 - 0.00260838557334383*R1)) * P2 +
						(S*P2*R1 <= (0.00777766418054526 - 0.00260838557334383*R1)) * -0.100946810821107
					)
			 )

	return(result)
}

dR_one <- function(S, A1, A2, R1, R2, P0, P1, P2){

	result = 0.0108898637923752*P0 + 0.00486825323206245*P2 + 
			 0.000352521033964462*S*R2 + 3.72373018280612e-5*P2^2 + 
			 0.000147231869850825*P2*S^2 - 1.72047666699306 - 
			 0.000806308073424104*R2 - 0.230258848867597*P1

	return(result)
}

dR_two <- function(S, A1, A2, R1, R2, P0, P1, P2){

	result = 0.313532904150143 + 0.00723112662728436*R2 + 
			 7.20928970085809e-5*P2 + 0.00299309738161579*A2^2 - 
			 0.0074014806769761*S - 0.0111233157702349*A1 - 
			 0.0217589693245477*R1 - 0.000337744062906282*R2^2

	return(result)
}

dP_zero <- function(S, A1, A2, R1, R2, P0, P1, P2){

	result = 0.00803730157443149*cos(R2) + 2.65842891611729e-5*P0^2 - 0.462867022360692 - 0.00131687503107756*P0

	return(result)
	
}

dP_one <- function(S, A1, A2, R1, R2, P0, P1, P2){

	result = (S - 6.12201621478115 < 0.00178571423269827)*sin(0.381315259268983 - 0.0620016774175446*S)

	return(result)
	
}

dP_two <- function(S, A1, A2, R1, R2, P0, P1, P2){

	result = 1.21178374921546*R2*
			 (
			 	(9.43091364362484 > S) * 0.0159480588943798 +
			 	(9.43091364362484 <= S) * 3.73495173912728*P1
			 ) - 
			 0.0159480588943798*A1*(9.43091364362484 >= S)

	return(result)
	
}

###############################################################################

###############################################################################
# Scaling results.

scale_for_comparison <- function(data){

	sampling = length(data) / 100
	sampled_vec = c()

	for( i in 1: 100 ){ sampled_vec[i] = data[ i * sampling ] }

	return(sampled_vec)

}

R_squared <- function(estimate, real_values){
  
  obs_mean = mean(estimate)
  
  SS_res = sum((estimate - real_values)^2)
  SS_tot = sum((real_values - obs_mean)^2)

  return(1 - SS_res/SS_tot)

}

create_R_squared_error <- function(S_vec_scaled, S_scaled,
								   A1_vec_scaled, A1_scaled,
								   A2_vec_scaled, A2_scaled,
								   R1_vec_scaled, R1_scaled,
								   R2_vec_scaled, R2_scaled,
								   P1_vec_scaled, P1_scaled,
								   P2_vec_scaled, P2_scaled){

  R_squared_errors = c(S_rsq = R_squared(S_vec_scaled[!is.nan(S_vec_scaled)], S_scaled[!is.nan(S_vec_scaled)]),
                       A1_rsq = R_squared(A1_vec_scaled[!is.nan(A1_vec_scaled)], A1_scaled[!is.nan(A1_vec_scaled)]),
                       A2_rsq = R_squared(A1_vec_scaled[!is.nan(A2_vec_scaled)], A1_scaled[!is.nan(A2_vec_scaled)]),
                       R1_rsq = R_squared(R1_vec_scaled[!is.nan(R1_vec_scaled)], R1_scaled[!is.nan(R1_vec_scaled)]),
                       R2_rsq = R_squared(R2_vec_scaled[!is.nan(R2_vec_scaled)], R2_scaled[!is.nan(R2_vec_scaled)]),
                       P1_rsq = R_squared(P1_vec_scaled[!is.na(P1_vec_scaled)], P1_scaled[!is.na(P1_vec_scaled)]),
                       P2_rsq = R_squared(P2_vec_scaled[!is.nan(P2_vec_scaled)], P2_scaled[!is.nan(P2_vec_scaled)]))

  return(R_squared_errors)

}

# We scale and plot the results in order for them to be compared on the same
# scale. We also calculate the R squared value between the unstabilised and
# underlying data.

scaling_plotting_R_sq <- function(){

	S_vec_scaled = scale_for_comparison(S_vec)
	S_scaled = scale_for_comparison(data$S)
	S_scaled_perf = scale_for_comparison(S_vec_perf)

	A1_vec_scaled = scale_for_comparison(A_one_vec)
	A1_scaled = scale_for_comparison(data$A1)
	A1_scaled_perf = scale_for_comparison(A_one_vec_perf)

	A2_vec_scaled = scale_for_comparison(A_two_vec)
	A2_scaled = scale_for_comparison(data$A2)
	A2_scaled_perf = scale_for_comparison(A_two_vec_perf)

	R1_vec_scaled = scale_for_comparison(R_one_vec)
	R1_scaled = scale_for_comparison(data$R1)
	R1_scaled_perf = scale_for_comparison(R_one_vec_perf)

	R2_vec_scaled = scale_for_comparison(R_two_vec)
	R2_scaled = scale_for_comparison(data$R2)
	R2_scaled_perf = scale_for_comparison(R_two_vec_perf)

	P1_vec_scaled = scale_for_comparison(P_one_vec)
	P1_scaled = scale_for_comparison(data$P1)
	P1_scaled_perf = scale_for_comparison(P_one_vec_perf)

	P2_vec_scaled = scale_for_comparison(P_two_vec)
	P2_scaled = scale_for_comparison(data$P2)
	P2_scaled_perf = scale_for_comparison(P_two_vec_perf)

	plot_result(S_vec_scaled, "Number of Scouts", c(5, 42), S_scaled, 0, 11, S_ind, S_scaled_perf)
	plot_result(A1_vec_scaled, "Number of Assessors (Site One)", c(0, 7), A1_scaled, 0, 7, A1_ind, A1_scaled_perf)
	plot_result(A2_vec_scaled, "Number of Assessors (Site Two)", c(0, 5), A2_scaled, 0, 5, A2_ind, A2_scaled_perf)
	plot_result(R1_vec_scaled, "Number of Recruiters (Site One)", c(0, 14), R1_scaled, 0, 14, R1_ind, R1_scaled_perf)
	plot_result(R2_vec_scaled, "Number of Recruiters (Site Two)", c(0, 30), R2_scaled, 0, 30, R2_ind, R2_scaled_perf)
	plot_result(P1_vec_scaled, "Number of Passive (Site One)", c(0, 1.7), P1_scaled, 0, 1.7, P1_ind, P1_scaled_perf)
	plot_result(P2_vec_scaled, "Number of Passive (Site Two)", c(0, 135), P2_scaled, 0, 135, P2_ind, P2_scaled_perf)

	R_sq_vec <- create_R_squared_error(S_vec_scaled, S_scaled,
						   A1_vec_scaled, A1_scaled,
						   A2_vec_scaled, A2_scaled,
						   R1_vec_scaled, R1_scaled,
						   R2_vec_scaled, R2_scaled,
						   P1_vec_scaled, P1_scaled,
						   P2_vec_scaled, P2_scaled)

	return(R_sq_vec)
}

# Plots the data, with lines for the underlying data, the perfect info model
# and the unstable model. Comment/ uncomment the part under unstable/ stable to
# see the relevant plot.

plot_result <- function(vec_scaled, ylabel, ylimit, scaled, leg_x, leg_y, role_ind, perf_vec){

	# Unstable

	par(mfrow = c(1, 1))

	plot(vec_scaled, type = "l", col = "blueviolet", lwd = 3, ylim = ylimit,
	     xlab= "Percentage through emigration", ylab = ylabel)
	lines(scaled, type = "l", col = "brown1", lwd = 4)

	legend(leg_x, leg_y,
	       c("General Behaviour", "Eureqa Derived Model"),
	       lty=c(1, 1, 1),
	       lwd=c(2, 2, 2),
	       col=c("brown1", "blueviolet"),
	       cex = 0.6 )

	# Stable

	# par(mfrow = c(1, 2))

	# plot(scaled, type = "l", col = "blueviolet", lwd = 4, ylim = ylimit,
	#      xlab= "Percentage through emigration", ylab = ylabel)
	# 	lines(perf_vec, type = "l", col = "mediumspringgreen", lwd = 3)

	# legend(leg_x, leg_y,
	#        c("General Behaviour", "Perfect Information Model"),
	#        lty=c(1, 1, 1),
	#        lwd=c(2, 2, 2),
	#        col=c("blueviolet", "mediumspringgreen"),
	#        cex = 0.6 )

	# plot(scaled - perf_vec, type = "l", col = "red", lwd = 3,
	# 	 xlab = "Percentage through emigration", ylab = "Difference in functions")

}

###############################################################################

###############################################################################
# Running model with the underlying data included.

# The model is run for the same number of steps as the underlying model.
# Note, we need a new equation for P0 as P0 does not necessarily equal 
# initial_passive - P1 - P2 because of smoothing.

t = 0

while(t < nrow(data)){

	S_perf = S_perf + dS(S_perf, data$A1[t + 1], data$A2[t + 1], data$R1[t + 1], data$R2[t + 1], data$P0[t + 1], data$P1[t + 1], data$P2[t + 1])
	A_one_perf = A_one_perf + dA_one(data$S[t + 1], A_one_perf, data$A2[t + 1], data$R1[t + 1], data$R2[t + 1], data$P0[t + 1], data$P1[t + 1], data$P2[t + 1])
	A_two_perf = A_two_perf + dA_two(data$S[t + 1], data$A1[t + 1], A_two_perf, data$R1[t + 1], data$R2[t + 1], data$P0[t + 1], data$P1[t + 1], data$P2[t + 1])
	R_one_perf = R_one_perf + dR_one(data$S[t + 1], data$A1[t + 1], data$A2[t + 1], R_one_perf, data$R2[t + 1], data$P0[t + 1], data$P1[t + 1], data$P2[t + 1])
	R_two_perf = R_two_perf + dR_two(data$S[t + 1], data$A1[t + 1], data$A2[t + 1], data$R1[t + 1], R_two_perf, data$P0[t + 1], data$P1[t + 1], data$P2[t + 1])
	P_one_perf = P_one_perf + dP_one(data$S[t + 1], data$A1[t + 1], data$A2[t + 1], data$R1[t + 1], data$R2[t + 1], data$P0[t + 1], P_one_perf, data$P2[t + 1])
	P_two_perf = P_two_perf + dP_two(data$S[t + 1], data$A1[t + 1], data$A2[t + 1], data$R1[t + 1], data$R2[t + 1], data$P0[t + 1], data$P1[t + 1], P_two_perf)

	S = S + dS(S, A_one, A_two, R_one, R_two, P_zero, P_one, P_two)
	A_one = A_one + dA_one(S, A_one, A_two, R_one, R_two, P_zero, P_one, P_two)
	A_two = A_two + dA_two(S, A_one, A_two, R_one, R_two, P_zero, P_one, P_two)
	R_one = R_one + dR_one(S, A_one, A_two, R_one, R_two, P_zero, P_one, P_two)
	R_two = R_two + dR_two(S, A_one, A_two, R_one, R_two, P_zero, P_one, P_two)
	P_one = P_one + dP_one(S, A_one, A_two, R_one, R_two, P_zero, P_one, P_two)
	P_two = P_two + dP_two(S, A_one, A_two, R_one, R_two, P_zero, P_one, P_two)
	P_zero = P_zero + dP_zero(S, A_one, A_two, R_one, R_two, P_zero, P_one, P_two)

	S_vec = c(S_vec, S)
	A_one_vec = c(A_one_vec, A_one)
	A_two_vec = c(A_two_vec, A_two)
	R_one_vec = c(R_one_vec, R_one)
	R_two_vec = c(R_two_vec, R_two)
	P_one_vec = c(P_one_vec, P_one)
	P_two_vec = c(P_two_vec, P_two)

	S_vec_perf = c(S_vec_perf, S_perf)
	A_one_vec_perf = c(A_one_vec_perf, A_one_perf)
	A_two_vec_perf = c(A_two_vec_perf, A_two_perf)
	R_one_vec_perf = c(R_one_vec_perf, R_one_perf)
	R_two_vec_perf = c(R_two_vec_perf, R_two_perf)
	P_one_vec_perf = c(P_one_vec_perf, P_one_perf)
	P_two_vec_perf = c(P_two_vec_perf, P_two_perf)
  
    t = t + 1
}

scaling_plotting_R_sq()

###############################################################################