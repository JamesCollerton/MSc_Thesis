import os.path
import sys
import math
import re
import csv
import matplotlib.pyplot as plt

# - James Collerton
# - Student Number 46114
# - Source Code for MSc Thesis

# This is the source code for the brute force approach to parameter stabilisation.
# Within this approach we take in the results.txt file from the Eureqa API and
# parse it as python code. All combinations of the functions as defined in the
# results.txt file are then built into models, the measurement of error between
# that and the training data is taken and the model with the smallest error is
# chosen as our model, plotted and a file written of the results.

# Needs three command line arguments:
# 	Colony size
# 	Quorum
# 	The training data.

###############################################################################
# Array indexes

# These are the array indexes for the read in files, used to access the different
# roles.

S_role = 0
P1_role = 1
P2_role = 2
A1_role = 3
A2_role = 4
R1_role = 5
R2_role = 6
t_role = 7

S_role_file = 3
P1_role_file = 5
P2_role_file = 6
R1_role_file = 7
R2_role_file = 8
A1_role_file = 9
A2_role_file = 10

###############################################################################

# Main wrapper for the get_functions function.

def main():

	all_results_list = {}

	get_functions()

# Takes the results file, reads it in, rearranges the function strings, turns
# them into usable Python code and then makes the model.

def get_functions():

	fname = "../../Results/results.txt"

	with open(fname) as f:
	    content = f.readlines()

	functions_vec = rearrange_funcs(content)

	list_of_solutions = split_solutions(functions_vec)

	make_model(list_of_solutions)

	return(functions_vec)

# This rearranges the contents of the results.txt file to be usable. It splits at
# 'END OF SOLUTION' to get the solutions for each of the roles. A special case
# is the first line where no 'END OF SOLUTION' exists. It then further splits 
# these into their respective function parts and removes the complexities and
# errors from the fronts of the string.

def rearrange_funcs(content):

	counter = 0
	functions_vec = []
	first_pass = True

	functions_vec.append("SPLIT_POINT")

	for row in content:
		temp_row = []
		counter = counter + 1
		if("END OF SOLUTION" in row):
			functions_vec.append("SPLIT_POINT")
			first_pass = False
			counter = 0
		if (counter >= 5 and counter < 9 and first_pass == False) or \
		   (first_pass == True and (counter >= 4 and counter < 8)):
			spl_string = row.split(' ')
			for entry in spl_string:
				new_entry = entry.replace('\n', '')
				temp_row.append(new_entry.strip())
			str_list = filter(None, temp_row)
			str_list = str_list[2:]
			func = ' '.join(str_list)
			functions_vec.append(func)

	return(functions_vec)

# This is used to split the functions vector whenever there is a 'SPLIT POINT'
# entry. These new vectors are put into an array, so we have an array of arrays
# each containing a list of functions for a particular variable.

def split_solutions(functions_vec):

	index_pairs = []
	all_solutions_list = []
	all_solutions_list_trim = []

	split_indexes = [i for i,x in enumerate(functions_vec) if x == 'SPLIT_POINT']

	for i in range(len(split_indexes) - 1):
		index_pairs.append([split_indexes[i], split_indexes[i + 1]])

	for pair in index_pairs:
		all_solutions_list.append(functions_vec[pair[0]:pair[1]])

	for item in all_solutions_list:
		removed = filter(lambda a: a != 'SPLIT_POINT', item)
		removed = filter(lambda a: a != '', removed)
		all_solutions_list_trim.append(removed)

	return(all_solutions_list_trim)

# This combines all possible models and creates them. It saves each model in
# a dictionary with its R squared value as its key. We can later then select
# the model according to the one with the highest R squared key.

def make_model(list_of_solutions):

	R_sq_dict = {}	

	for S_exp in list_of_solutions[S_role]:
		for P1_exp in list_of_solutions[P1_role]:
			for P2_exp in list_of_solutions[P2_role]:
				for A1_exp in list_of_solutions[A1_role]:
					for A2_exp in list_of_solutions[A2_role]:
						for R1_exp in list_of_solutions[R1_role]:
							for R2_exp in list_of_solutions[R2_role]:
								expression_vec = [S_exp,
												  P1_exp,
												  P2_exp,
												  A1_exp,
												  A2_exp,
												  R1_exp,
												  R2_exp]

								R_sq_ave = create_model_inst(expression_vec, False)
								R_sq_key = round(R_sq_ave, 6)
								R_sq_dict[R_sq_key] = expression_vec

	plot_best_model(R_sq_dict)

	write_model_csv(R_sq_dict)

# This is used to create a model according to a vector of strings representing
# the functions within the vector. It organises the functions and creates Python
# code from them. It then sets the colony size and the quorum. Finally it gets
# the training data to compare to the new model and find the R squared. The
# model is plotted if we provide an argument telling the function to.

def create_model_inst(expression_vec, plot_bool):

	organised = organise_functions(expression_vec)
	func_strings = create_functions(organised)

	N = set_CS(sys.argv)
	Q = set_Q(sys.argv)

	filenames = get_filenames(sys.argv)
	file_contents = read_files(filenames)

	model = create_model(N, Q, func_strings)
	role_pairs = create_role_pairs()

	if(plot_bool == True):
		make_plot(model, role_pairs, file_contents)

	R_sq_ave = calc_R_squared(model, role_pairs, file_contents)

	return(R_sq_ave)

# This organises the functions, parsing the powers and the trig functions, changing
# them to Python code (^ goes to math.pow, sin goes to math.sin etc.).

def organise_functions(functions):

	new_functions = []

	for fnc in functions:
		new_func = replace_power(fnc, 't')
		new_func = replace_power(new_func, 'Q')

		new_func = replace_power(new_func, 'S')
		new_func = replace_power(new_func, 'P1')
		new_func = replace_power(new_func, 'P2')
		new_func = replace_power(new_func, 'A1')
		new_func = replace_power(new_func, 'A2')
		new_func = replace_power(new_func, 'R1')
		new_func = replace_power(new_func, 'R2')

		new_func = replace_trig(new_func)
		new_functions.append(new_func)

	new_functions = filter(None, new_functions)

	return(new_functions)

# This is used to parse powers of different variables. It splits the string at the
# power symbol, then finds the first integer on the right of the split and puts
# a bracket round it. It then joins the string using the math.pow term and recreates
# the original string. This process essentiall replaces ^x with math.pow(.,x)

def replace_power(func, char):

	powers = []

	spl_string = func.split(char + '^')
	for i in range(1, len(spl_string)):
		if i != 0:
			numbers = re.findall(r"[-+]?\d*\.\d+|\d+", spl_string[i])
			powers.append(numbers[0])

	for i in range(0, len(powers)):
		spl_string[i + 1] = spl_string[i + 1].replace(powers[i], powers[i] + ")", 1)

	join_str = 'math.pow(' + char + ','
	joined_func = join_str.join(spl_string)
	
	return(joined_func)

# This function is used to replace the trig components of the model.

def replace_trig(new_func):

	new_func = new_func.replace("sin", "math.sin")
	new_func = new_func.replace("cos", "math.cos")

	return(new_func)

# Within this function we create valid Python code from each of the function
# strings to be executed as part of the model.

def create_functions(organised):

	func_names = ["dS", "dP1", "dP2", "dA1", "dA2", "dR1", "dR2"]
	func_strings = []

	for i in range(0, len(func_names)):
		f_string = make_func_string(func_names[i], organised[i])
		func_strings.append(f_string)

	return(func_strings)

# Formats the strings for Python syntax with the correct spacing etc.

def make_func_string(func_name, func):

	func_string = "def " + func_name + "(S, P1, P2, A1, A2, R1, R2):\n" +	\
				  "\tresult = " + func + "\n" +			\
				  "\treturn(result)"

	return(func_string)

# Sets quorum according to command line arguments.

def set_Q(command_line_args):

	if(len(command_line_args) < 3):
		print("Too few command line arguments.")
		exit(1)
	else:
	    Q = parse_int(command_line_args[2], "Invalid quorum size.")	

	return(Q)

# Sets the colony size according to the command line arguments.

def set_CS(command_line_args):

	if(len(command_line_args) < 2):
		print("Too few command line arguments.")
		exit(1)
	else:
	    N = parse_int(command_line_args[1], "Invalid colony size.")

	return(N)

# Function used to parse integers throughout the program.

def parse_int(poss_int, error_mess):

    try:
        return int(poss_int)
    except ValueError:
        print(error_mess)
        exit(1)

# Function used to parse floats throughout the program.

def parse_float(poss_float, error_mess):

    try:
        return float(poss_float)
    except ValueError:
        print(error_mess)
        exit(1)

# This runs the model. The initial conditions for P2 are set to slightly above
# zero to avoid the problems with complexity and initial conditions as documented
# in the body of the thesis.

def create_model(N, Q, func_strings):

	P0 = 0
	P1 = 0
	P2 = 0.000000000000164
	R1 = 0
	R2 = 0
	A1 = 0
	A2 = 0
	S = 42.83879
	t = 0

	vars_mat = []
	vars_vec = [S, P1, P2, A1, A2, R1, R2, t]
	vars_mat.append(vars_vec)

	for timestep in range(100):
		vars_vec = run_model(vars_vec, timestep, N, Q, func_strings)
		vars_mat.append((list(vars_vec)))

	return(vars_mat)

# This actually runs the steps of the model. It executes strings provided to
# it as Python code, then fills in the elements of the vector as needed.

def run_model(vars_vec, t, N, Q, func_strings):

	for f_string in func_strings:
		exec(f_string)

	S = vars_vec[S_role]
	P1 = vars_vec[P1_role]
	P2 = vars_vec[P2_role]
	A1 = vars_vec[A1_role]
	A2 = vars_vec[A2_role]
	R1 = vars_vec[R1_role]
	R2 = vars_vec[R2_role]

	vars_vec[P1_role] = vars_vec[P1_role] + dP1(S, P1, P2, A1, A2, R1, R2)
	vars_vec[P2_role] = vars_vec[P2_role] + dP2(S, P1, P2, A1, A2, R1, R2)
	vars_vec[R1_role] = vars_vec[R1_role] + dR1(S, P1, P2, A1, A2, R1, R2)
	vars_vec[R2_role] = vars_vec[R2_role] + dR2(S, P1, P2, A1, A2, R1, R2)
	vars_vec[A1_role] = vars_vec[A1_role] + dA1(S, P1, P2, A1, A2, R1, R2)
	vars_vec[A2_role] = vars_vec[A2_role] + dA2(S, P1, P2, A1, A2, R1, R2)
	vars_vec[S_role] = vars_vec[S_role] + dS(S, P1, P2, A1, A2, R1, R2)

	vars_vec[t_role] = t 

	return(vars_vec)

# Gets the filename of the training data from the command line.

def get_filenames(command_line_args):

	filenames = []

	if(len(command_line_args) < 4):
		print("Too few command line arguments")
	else:
		for i in range(3, len(command_line_args)):
			filenames.append(command_line_args[i])

	return(filenames)

# Reads in the file contents from the training data and arranges them for 
# plotting.

def read_files(filenames):

	content = read_file_contents(filenames)
	arranged = arrange_content(content)

	return(arranged)

# Reads in all file contents.

def read_file_contents(filenames):

	try:
		fname = filenames[0]
		with open(fname) as f:
		    content = f.readlines()
	except Exception:
		print("Couldn't open file!")
		exit(1)

	return(content)

# Splits the content at ',' like a .csv, then make a list of the results.

def arrange_content(content):

	arranged = []

	for row in content:
		split_row = row.split(',')
		arranged.append((list(split_row)))

	return(arranged)

# This creates role pairs. The aim of this is to pair up indices from the training
# data and the newly created model. For example in the training data the third
# column may be S, but the first vector in the newly created model is our new S values.
# The role pairs just provides a convenient way of pairing the two.

def create_role_pairs():

	role_pairs = [ [S_role, S_role_file],
				   [P1_role, P1_role_file],
				   [P2_role, P2_role_file],
   				   [A1_role, A1_role_file],
				   [A2_role, A2_role_file],
				   [R1_role, R1_role_file],
				   [R2_role, R2_role_file]
					]
	return(role_pairs)

# This makes the plot using matplotlib. It puts all of the plots in a single
# figure, hence the subplot.

def make_plot(model, role_pairs, file_contents):

	i = 0

	fig1 = plt.figure(1)
	rect = fig1.patch
	rect.set_facecolor('white')

	y_labs = ['Scouts', 'Passive (Site One)', 'Passive (Site Two)',
			  'Recruiters (Site One)', 'Recruiters (Site Two)', 
			  'Assessors (Site One)', 'Assessors (Site Two)']

	for pair in role_pairs:
		plt.subplot(4, 2, i + 1)
		plot_role(model, pair[0], file_contents, pair[1], y_labs[i])
		i = i + 1

	plt.show()

# This is used to plot roles using mat plot lib. It goes through the directory
# of roles and appends them to vectors in order to plot them through matplotlib.

def plot_role(model, role, sing_file, file_role, y_label):

	role_vec = []
	file_role_vec = []

	fin_y_label = "Number of " + y_label

	for i in range(1, 99):
		role_vec.append(model[i][role])
		sing_file_role = sing_file[i][0].split(" ")
		file_role_vec.append(sing_file_role[file_role])

	plt.plot(role_vec)
	plt.plot(file_role_vec, 'r')
	plt.ylabel(fin_y_label, fontsize = 7)
	plt.xlabel('Time', fontsize = 7)

# Calculates the R squared value for different roles in the model. Then averages
# them over all roles to give an overall impression of the fit of the model.

def calc_R_squared(model, role_pairs, file_contents):

	R_sq_vec = []

	for pair in role_pairs:
		R_sq_val = R_sq(model, pair[0], file_contents, pair[1])
		R_sq_val = max(0, R_sq_val)
		R_sq_vec.append(R_sq_val)

	R_sq_ave = reduce(lambda x, y: x + y, R_sq_vec) / len(R_sq_vec)

	return(R_sq_ave)

# Defines the R squared value. Rearranges the data to create two vectors of 
# equal length for the comparison, then calculates the result. If we divide by
# zero we just say the fit is zero.

def R_sq(model, role, sing_file, file_role):

	role_vec = []
	file_role_vec = []

	for i in range(1, 99):
		role_vec.append(model[i][role])
		sing_file_role = sing_file[i][0].split(" ")
		file_role_vec.append(sing_file_role[file_role])

	SS_res = calc_SS_res(role_vec, file_role_vec)
	SS_tot = calc_SS_tot(role_vec, file_role_vec)

	try:
		R_sq_val = 1 - SS_res/SS_tot
	except Exception, e:
		R_sq_val = 0

	return(R_sq_val)

# Calculates the SS residual component of R squared.

def calc_SS_res(role_vec, file_role_vec):

	SS_res = 0

	for i in range(len(role_vec)):
		role_vec_float = parse_float(file_role_vec[i], "File integer not parsed")
		SS_res = SS_res + math.pow((role_vec[i] - role_vec_float), 2)

	return(SS_res)

# Calculates the SS total component of R squared.

def calc_SS_tot(role_vec, file_role_vec):

	SS_tot = 0

	y_bar = reduce(lambda x, y: x + y, role_vec) / len(role_vec)

	for i in range(len(role_vec)):
		SS_tot = SS_tot + math.pow((role_vec[i] - y_bar), 2)

	return(SS_tot)

# This is used to plot the best model according to the minimal R squared value.
# Looks up in the dictionary of models by the R squared val.

def plot_best_model(R_sq_dict):

	R_sq_vals = []

	for key in R_sq_dict:
		R_sq_vals.append(key)

	max_R_sq_val = max(R_sq_vals)

	create_model_inst(R_sq_dict[max_R_sq_val], True)

# This is used to write the best model to a .csv. It creates the model according
# to the one with the maximum R squared value, and then writes it to a .csv

def write_model_csv(R_sq_dict):

	R_sq_vals = []

	for key in R_sq_dict:
		R_sq_vals.append(key)

	max_R_sq_val = max(R_sq_vals)

	role_pairs = create_role_pairs()

	filenames = get_filenames(sys.argv)
	file_contents = read_files(filenames)

	organised = organise_functions(R_sq_dict[max_R_sq_val])
	func_strings = create_functions(organised)

	N = set_CS(sys.argv)
	Q = set_Q(sys.argv)

	model = create_model(N, Q, func_strings)

	write_file(model, role_pairs, file_contents)

# Function thatdeals with going through each of the roles and writing them to
# .csv. For each role attaches the underlying and approximation values, so they
# can be accessed to compare the training data and the result.

def write_file(model, role_pairs, file_contents):

	role_dict = {}

	role_names = ['S', 'P1', 'P2', 'A1', 'A2', 'R1', 'R2']

	j = 0

	for pair in role_pairs:
		role_vec = []
		file_role_vec = []
		for i in range(1, 99):
			role_vec.append(model[i][pair[0]])
			sing_file_role = file_contents[i][0].split(" ")
			file_role_vec.append(sing_file_role[pair[1]].rstrip('\n'))
		role_dict[role_names[j] + ' Underlying'] = role_vec
		role_dict[role_names[j] + ' Approximation'] = file_role_vec
		j = j + 1

	write_csv(role_dict)

# Function that deals with physically writing the .csv from the role dictionary.

def write_csv(role_dict):

	headers = ['S Underlying', 'S Approximation', 
			   'P1 Underlying', 'P1 Approximation',
			   'P2 Underlying', 'P2 Approximation',
			   'A1 Underlying', 'A1 Approximation',
			   'A2 Underlying', 'A2 Approximation',
			   'R1 Underlying', 'R1 Approximation',
			   'R2 Underlying','R2 Approximation']

	rows = zip(role_dict[headers[0]],
			   role_dict[headers[1]],
			   role_dict[headers[2]],
			   role_dict[headers[3]],
			   role_dict[headers[4]],
			   role_dict[headers[5]],
			   role_dict[headers[6]],
			   role_dict[headers[7]],
			   role_dict[headers[8]],
			   role_dict[headers[9]],
			   role_dict[headers[10]],
			   role_dict[headers[11]],
			   role_dict[headers[12]],
			   role_dict[headers[13]])

	with open('../../Output_Files/final_combination.csv', 'wb') as f:
		writer = csv.writer(f)
		writer.writerow(headers)
		for row in rows:
			writer.writerow(row)

# Runs main.

if __name__ == "__main__":
    main()