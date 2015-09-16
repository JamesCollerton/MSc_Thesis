# - James Collerton
# - Student Number 46114
# - Source Code for MSc Thesis

# This is the source code for the Planque model with both fixed and variable
# parameters. It will create directories and then output the necessary data
# to those directories.

###############################################################################
# Imported libraries

import os.path
import numpy as np
from random import randint

###############################################################################

###############################################################################
# Step Functions as defined in the original paper.

def l(lmda, R, Q, A):
	if R < Q:
		result = ( lmda * R * A ) / ( R + A )
	else:
		result = 0
	return(result)

def c(phi, R, Q, P):
	if R >= Q:
		result = ( phi * R * P ) / ( R + P )
	else:
		result = 0
	return(result)

def r(lmda, R, Q, B):
	if R >= Q:
		result = ( lmda * R * B ) / ( R + B )
	else:
		result = 0
	return(result)

###############################################################################

###############################################################################
# Differential increments

def dA(mu, A, lmda, R, Q):
	result = -mu * A - l(lmda, R, Q, A)
	return(result)

def dS(mu, A, kappa, S, f, lmda, R, Q):
	result = mu * A - kappa * S - f * r(lmda, R, Q, S)
	return(result)

def dR(kappa, S, lmda, R, Q, A, f):
	result = kappa * S + l(lmda, R, Q, A) + f * r(lmda, R, Q, S)
	return(result)

def dP(f, phi, R, Q, P):
	result = -(1 - f) * c(phi, R, Q, P)
	return(result)

def dC(f, phi, R, Q, P):
	result = (1 - f) * c(phi, R, Q, P)
	return(result)

###############################################################################

###############################################################################
# Parameters

Q = [1, 5, 10, 15, 20, 25] 	# Quorum thresholds to be examined in fixed trials.

###############################################################################

###############################################################################
# Program functions

# Main, ran from the bottom.

def main():

	run_fixed()
	run_var_parameters()

# This is used to run the fixed parameter trials. Sets the parameter values
# according to the original paper.

def run_fixed():

	create_folder("planque_results")
	parameters = create_parameters(250, 0.1447, 0.1, 0.05, 0.1, 0.2, 0.001, 0.01)
	run_trial(parameters, True, None, True)

# Used to run the variable parameter trials. Generates ten trials with random
# parameter values. The quorum is set between 1 and 12 for a reasonable value.

def run_var_parameters():

	create_folder("planque_var_results")

	for i in range(0, 10):
		quorum = randint(1,12)
		parameters = random_parameters()
		if(i == 0): run_trial(parameters, False, quorum, True)
		else: run_trial(parameters, False, quorum, False)

# Used to generate random parameters for the model. They are sampled uniformly
# within an appropriate range.

def random_parameters():

	parameters = create_parameters(uniform_sample(240, 260),       	# N
								   uniform_sample(0.05, 0.5),      	# F
								   uniform_sample(0.05, 0.15), 	   	# f
								   uniform_sample(0.01, 0.2), 	   	# mu 
								   uniform_sample(0.05, 0.15), 	  	# lmda
								   uniform_sample(0.1, 0.3),   		# phi
								   uniform_sample(0.0001, 0.001), 	# kappa
								   0.01) 							# epsilon

	return(parameters)

# Used to generate a single sample from the uniform distribution.

def uniform_sample(lower_bound, upper_bound):

	result = np.random.uniform(lower_bound, upper_bound, 1)[0]

	return(result)

# Runs the trials. Declares initial starting conditions and then if the
# trial is fixed, iterates through the given quorums. If not it runs a
# single trial with the given quorum.

def run_trial(parameters, fixed, quorum, header_needed):

	A = parameters['F'] * parameters['N'] - ( 2 * parameters['epsilon'] )
	S = parameters['epsilon']
	R = parameters['epsilon']
	P = ( 1 - parameters['F'] ) * parameters['N']
	C = 0

	if(fixed == True): 
		for quorum in Q: 
			run(A, S, R, P, C, quorum, fixed, header_needed, parameters)

	else:
		run(A, S, R, P, C, quorum, fixed, header_needed, parameters)

# Creates folder to put the results in.

def create_folder(folder_name):

	if not os.path.exists(folder_name):
		os.makedirs(folder_name)

# Creates a dictionary of parameter values to be used depending on the input.

def create_parameters(N, F, f, mu, lmda, phi, kappa, epsilon):

	parameters = {}

	parameters['N'] = N 		
	parameters['F'] = F 
	parameters['f'] = f 	
	parameters['mu'] = mu 
	parameters['lmda'] = lmda 
	parameters['phi'] = phi
	parameters['kappa'] = kappa 
	parameters['epsilon'] = epsilon 

	return(parameters)

# Runs the model with given parameter values.

def run(A, S, R, P, C, quorum, fixed, header_needed, parameters):

	write_file = create_file(quorum, fixed, header_needed)
	t = 0

	while (P > 0.01):
		A += dA(parameters['mu'], A, parameters['lmda'], R, quorum)
		S += dS(parameters['mu'], A, parameters['kappa'], S, parameters['f'], parameters['lmda'], R, quorum)
		R += dR(parameters['kappa'], S, parameters['lmda'], R, quorum, A, parameters['f'])
		P += dP(parameters['f'], parameters['phi'], R, quorum, P)
		C += dC(parameters['f'], parameters['phi'], R, quorum, P)
		t += 1
		values = [A, S, R, P, C, t]
		write_values(values, write_file, parameters, fixed, quorum)

# Creates a file for writing the results to. If it is a fixed trial creates
# its own file. If not then it appends the result to the other variable parameter
# trials, unless it is the first one, when it creates a new file and gives
# headers.

def create_file(quorum, fixed, header_needed):

	if(fixed == True):
		filename = "planque_results/" + str(quorum) + "_quorum_results.txt"
		write_file = open(filename, 'w')
		write_file.write("A, S, R, P, C,\n")

	else:
		filename = "planque_var_results/var_simulation_results.txt"
		if(header_needed == True):
			write_file = open(filename, 'w')
			write_file.write("A, S, R, P, C, t, phi, kappa, uc_F, epsilon, lc_f, N, mu, lmda, Q,\n")
		else:
			write_file = open(filename, 'a')

	return(write_file)

# Used for writing the values to a file.

def write_values(values, write_file, parameters, fixed, quorum):

	for val in values:
		write_val = str(round(val, 5)).ljust(9)
		write_file.write( str(write_val) + " , " )

	if(fixed == False):

		for key, value in parameters.iteritems():
			write_par = str(round(value, 5)).ljust(9)
			write_file.write( str(write_par) + " , " )

		write_quorum = str(round(quorum, 5)).ljust(9)
		write_file.write( str(write_quorum) + " , " )


	write_file.write("\n")

###############################################################################

# Runs main.

if __name__ == "__main__":
    main()