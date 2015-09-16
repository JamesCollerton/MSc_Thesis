# - James Collerton
# - Student Number 46114
# - Source Code for MSc Thesis

# This is the source code for the Pratt model with both fixed and variable
# parameters. It will create directories and then output the necessary data
# to those directories.

###############################################################################
# Imported libraries

import os.path
import numpy as np
from random import randint

###############################################################################
# Step Functions

def I(R_i, S, T):
	if(R_i < T and S > 0): return(R_i)
	else: return(0)

# Note: Must make an approximation here with < sign. In the original paper it
# uses an equals rather than a less than.
def J(R_i, P_nought, T):
	if(R_i < T or P_nought < 0): return(0)
	else: return(R_i)

###############################################################################

###############################################################################
# Differential Functions

def dS(S, R_one, R_two, T, parameters):
	result = -2 * parameters['mu'] * S - parameters['lmbda'] * 				\
			 ( I(R_one, S, T) + I(R_two, S, T) )
	return(result)

def dA(S, R, A_i, A_ii, k, T, parameters):
	result = parameters['mu'] * S + parameters['lmbda'] * I(R, S, T) + 		\
			 parameters['p_onetwo'] * A_i - k * A_ii
	return(result)

def dP(R, P_nought, T, parameters):
	result = parameters['phi'] * J(R, P_nought, T)
	return(result)

def dR(k_i, A_i, R_i, parameters):
	result = k_i * A_i + parameters['p_onetwo'] * R_i
	return(result)

###############################################################################

# Range of quorums considered
T = [1, 5, 10, 15, 20, 25, 40]

###############################################################################
# Runs the main part of the model.

def main():

	fixed_parameters_run()
	var_parameters_run()

# Used for running the fixed parameter trials, with the parameter values as in
# the original paper. Runs trials with quorums 1, 5, 10, 15, 20, 25 and 40.

def fixed_parameters_run():

	create_folder("pratt_results")
	parameters = create_parameters(208, 0.25, 0.033, 0.099, 0.013, 0.015,	\
								   0.02, 0.008, 4.6)

	model_run(parameters, True, True, None)

# Used for running trials with variable values of parameters. Quorum set 
# randomly between 1 and 12.

def var_parameters_run():

	create_folder("pratt_var_results")

	for i in range(0, 10):

		threshold = randint(1,12)
		if(i == 0): create_var_trial(threshold, True)
		else: create_var_trial(threshold, False)

# Used to generate runs of the model. If it detects that parameters are fixed
# it iterates through the thresholds in T with the fixed parameters. If not, 
# it is only run once with that random threshold.

def model_run(parameters, fixed, header_needed, rand_threshold):

	#Initial conditions
	P_nought = parameters['total_passive']
	P_one = 0
	P_two = 0
	R_one = 0
	R_two = 0
	A_one = 0
	A_two = 0
	S = parameters['N'] - parameters['total_passive']

	if(fixed == True):
		for threshold in T:
			run(P_nought, P_one, P_two, R_one, R_two, A_one, A_two, S, threshold,
				parameters, "pratt_results", True, header_needed)

	else: 
		run(P_nought, P_one, P_two, R_one, R_two, A_one, A_two, S, rand_threshold,
			parameters, "pratt_var_results", False, header_needed)


# Used to create a single random trial. Generaters random parameters according
# to the values in the original paper, then feeds them on to create a random
# trial.

def create_var_trial(threshold, header_needed):

	parameters = random_parameters()
	model_run(parameters, False, header_needed, threshold)


# Used to create the folders for results.

def create_folder(folder_name):

	if not os.path.exists(folder_name):
		os.makedirs(folder_name)

# Creates the list of parameters for the simulation. These are sampled randomly
# from the distributions given in the original paper.

def random_parameters():

	parameters = create_parameters( normal_dist_par(208, 99), 			# N
									normal_dist_par(0.25, 0.1),			# p
									normal_dist_par(0.033, 0.016), 		# lambda
									normal_dist_par(0.099, 0.02), 		# phi
									normal_dist_par(0.013, 0.006), 		# mu 
									normal_dist_par(0.015, 0.06),		# k_one
									normal_dist_par(0.02, 0.008), 		# k_two
									normal_dist_par(0.008, 0.004),		# p_onetwo
									4.6 )								# c

	return(parameters)

# This samples from the normal distribution to get values for the tests. It
# also contains a clause that prevents the values becoming negative.

def normal_dist_par(mu, sigma):

	result = -1

	while(result < 0):
		result = np.random.normal(mu, sigma, 1)[0]

	return(result)

# Used to set the parameters vector depending on some input.

def create_parameters(N, p, lmbda, phi, mu, k_one, k_two, p_onetwo, c):

	parameters = {}

	parameters['N'] = N
	parameters['p'] = p
	parameters['lmbda'] = lmbda
	parameters['phi'] = phi
	parameters['mu'] = mu
	parameters['k_one'] = k_one
	parameters['k_two'] = k_two
	parameters['p_onetwo'] = p_onetwo
	parameters['c'] = c
	parameters['total_passive'] = parameters['N'] * (1 - parameters['p'])

	return(parameters)

# Actually runs the model for the parameter values. Creates a file to write to
# then runs the model, writing to the file after each time step.

def run(P_nought, P_one, P_two, R_one, R_two, A_one, A_two, S, threshold,
		parameters, folder_name, fixed, header_needed):

	write_file = write_header(fixed, folder_name, threshold, header_needed)
	t = 0

	while(P_nought > 0.1):
	# Can also be used as an end point depending what context you consider the model in.
	# while ( abs(P_nought + P_two + R_two - parameters['N']) > 0.01 and P_one >= 0):

		if( abs(P_nought + P_one + P_two + R_two - parameters['N']) > 0.01 ):

			P_one += dP(R_one, P_nought, threshold, parameters)
			P_two += dP(R_two, P_nought, threshold, parameters)
			R_one += dR(parameters['k_one'], A_one, - R_one, parameters)
			R_two += dR(parameters['k_two'], A_two, R_one, parameters)
			A_one += dA(S, R_one, - A_one, A_one, parameters['k_one'], threshold, parameters)
			A_two += dA(S, R_two, A_one, A_two, parameters['k_two'], threshold, parameters)
			S += dS(S, R_one, R_two, threshold, parameters)
			t += 1

		else:

			P_two += parameters['c']
			P_one -= parameters['c']

		P_nought = parameters['total_passive'] - ( P_one + P_two )

		values = [P_nought, P_one, P_two, R_one, R_two, A_one, A_two, S, t]

		write_values(values, write_file, fixed, parameters, threshold)

# This writes the header for the file. If we are writing for a run with parameters
# in then we also need headers for those parameters.

def write_header(fixed, folder_name, threshold, header_needed):

	if(fixed == True):
		filename = folder_name + "/" + str(threshold) + "_threshold_results.txt"
	else: filename = folder_name + "/var_simulation_results.txt"

	if(header_needed == True):

		write_file = open(filename, 'w')
		write_file.write("P_nought, P_one, P_two, R_one, R_two, A_one, A_two, S, t, ")

		if(fixed == False):
			write_file.write("phi, p_onetwo, total_passive, lmbda, c, N, mu, p, k_one, k_two, Q,\n")
		else: write_file.write("\n")

	else: write_file = open(filename, 'a')

	return(write_file)

# Writes the values to file depending on whether or not the values are fixed
# or not. If not also provides the parameter values.

def write_values(values, write_file, fixed, parameters, threshold):

	for val in values:
		write_val = str(round(val, 5)).ljust(9)
		write_file.write( str(write_val) + " , " )

	if(fixed == False):
		for key, value in parameters.iteritems():
			write_par = str(round(value, 5)).ljust(9)
			write_file.write( str(write_par) + " , " )

		write_threshold = str(round(threshold, 5)).ljust(9)
		write_file.write( str(write_threshold) + " , " )

	write_file.write("\n")

# Runs main.

if __name__ == "__main__":
    main()