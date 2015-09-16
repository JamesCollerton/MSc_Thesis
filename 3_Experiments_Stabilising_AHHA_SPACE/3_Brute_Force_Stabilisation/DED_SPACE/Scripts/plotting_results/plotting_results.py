import os.path
import sys
import math
import re
import csv
import matplotlib.pyplot as plt

# - James Collerton
# - Student Number 46114
# - Source Code for MSc Thesis

# This is the source code for what was a prototype differential equation
# model parser and builder from the Eureqa API program. As opposed to the
# the system talked about within the body of the thesis, this does not
# take the combination of all of the solutions on the Pareto front, but only
# those at the optimal point in terms of complexity and error are used to make
# the model. It acts to show us what solution we would have generated previously,
# when we did not use the brute force approach.

# Needs three command line arguments:
# 	Colony size
# 	Quorum
# 	The training data.

###############################################################################

###############################################################################
# Global vars

# These act as array indices in the read in data .csv files.

S_role = 7
P1_role = 1
P2_role = 2
A1_role = 3
A2_role = 4
R1_role = 5
R2_role = 6
t_role = 7

S_func = 0
P1_func = 1
P2_func = 2
A1_func = 3
A2_func = 4
R1_func = 5
R2_func = 6

S_role_file = 3
P1_role_file = 5
P2_role_file = 6
R1_role_file = 7
R2_role_file = 8
A1_role_file = 9
A2_role_file = 10

###############################################################################

# Main function, prints a message to tell us we have arrived into this bit of
# the parser (which is useful when we boot straight into it from the Eureqa API).
# We then get all of the functions from the results.txt file, organise them,
# parse them as python code and then execute them from strings into functions.
# Finally we run the model and plot the results.

def main():

	N = 0
	Q = 0
	aggregated_file = ""
	filenames = []
	file_contents = []
	functions = []

	print_welcome()

	functions = get_functions()

	organised = organise_functions(functions)

	func_strings = create_functions(organised)

	N = set_CS(sys.argv)

	Q = set_Q(sys.argv)

	filenames = get_filenames(sys.argv)

	file_contents = read_files(filenames)

	model = create_model(N, Q, func_strings)

	role_pairs = create_role_pairs()

	make_plot(model, role_pairs, file_contents)

# Prints the welcome for the program.

def print_welcome():

	print("\n\n")
	print("SPACE differential equation command line tool.")
	print("\n")

# Sets the colony size for the model.

def set_CS(command_line_args):

	if(len(command_line_args) < 2):
		print("Too few command line arguments.")
		exit(1)
	else:
		N = parse_int(command_line_args[1], "Invalid colony size.")

	return(N)

# Function used for parsing ints from .txt file.

def parse_int(poss_int, error_mess):

	try:
		return int(poss_int)
	except ValueError:
		print(error_mess)
		exit(1)

# Sets the quorum size for the emigrations.

def set_Q(command_line_args):

	if(len(command_line_args) < 3):
		print("Too few command line arguments.")
		exit(1)
	else:
		Q = parse_int(command_line_args[2], "Invalid quorum size.")	

	return(Q)

# Gets the filenames

def get_filenames(command_line_args):

	filenames = []

	if(len(command_line_args) < 4):
		print("Too few command line arguments")
	else:
		for i in range(3, len(command_line_args)):
			filenames.append(command_line_args[i])

	return(filenames)

# Creates the model from initial conditions. The initial conditions are
# hardcoded for the case we are interested in, but this could easily be
# extended to include more flexible arguments.

def create_model(N, Q, func_strings):

	P0 = 0
	P1 = 0
	P2 = 0.0001
	R1 = 0
	R2 = 0
	A1 = 0
	A2 = 0
	S = 42.83879
	t = 0

	vars_mat = []
	vars_vec = [P0, P1, P2, R1, R2, A1, A2, S, t]
	vars_mat.append(vars_vec)

	for timestep in range(100):
		vars_vec = run_model(vars_vec, timestep, N, Q, func_strings)
		vars_mat.append((list(vars_vec)))

	return(vars_mat)

# This runs the model steps. Each of the strings which represents a function read in
# from results.txt is executed as python code within the program to create the
# model.

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

	vars_vec[S_role] = vars_vec[S_role] + dS(S, P1, P2, A1, A2, R1, R2)
	vars_vec[P1_role] = vars_vec[P1_role] + dP1(S, P1, P2, A1, A2, R1, R2)
	vars_vec[P2_role] = vars_vec[P2_role] + dP2(S, P1, P2, A1, A2, R1, R2)
	vars_vec[A1_role] = vars_vec[A1_role] + dA1(S, P1, P2, A1, A2, R1, R2)
	vars_vec[A2_role] = vars_vec[A2_role] + dA2(S, P1, P2, A1, A2, R1, R2)
	vars_vec[R1_role] = vars_vec[R1_role] + dR1(S, P1, P2, A1, A2, R1, R2)
	vars_vec[R2_role] = vars_vec[R2_role] + dR2(S, P1, P2, A1, A2, R1, R2)

	return(vars_vec)

# This is used to plot roles using mat plot lib. It goes through the directory
# of roles and appends them to vectors in order to plot them through matplotlib.

def plot_role(model, role, sing_file, file_role, y_label):

	role_vec = []
	file_role_vec = []

	fin_y_label = "Number of " + y_label

	for i in range(1, 100):
		role_vec.append(model[i][role])
		sing_file_role = sing_file[i][0].split(" ")
		file_role_vec.append(sing_file_role[file_role])

	plt.plot(role_vec)
	plt.plot(file_role_vec, 'r')
	plt.ylabel(fin_y_label, fontsize = 7)
	plt.xlabel('Time', fontsize = 7)

# Reads in the files and arranges them to be used as python functions.

def read_files(filenames):

	content = read_file_contents(filenames)
	arranged = arrange_content(content)

	return(arranged)

# Reads in all of the file.

def read_file_contents(filenames):

	try:
		fname = filenames[0]
		with open(fname) as f:
			content = f.readlines()
	except Exception:
		print("Couldn't open file!")
		exit(1)

	return(content)

# Arranges the lines, splitting them at comma.

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
				   [R1_role, R1_role_file],
				   [R2_role, R2_role_file],
				   [A1_role, A1_role_file],
				   [A2_role, A2_role_file]
					]
	return(role_pairs)

# This makes the plot using matplotlib. It puts all of the plots in a single
# figure, hence the subplot.

def make_plot(model, role_pairs, file_contents):

	i = 0

	y_labs = ['Scouts', 'Passive (Site One)', 'Passive (Site Two)',
			  'Recruiters (Site One)', 'Recruiters (Site Two)', 
			  'Assessors (Site One)', 'Assessors (Site Two)']

	fig1 = plt.figure(1)
	rect = fig1.patch
	rect.set_facecolor('white')

	for pair in role_pairs:
		plt.subplot(4, 2, i + 1)
		plot_role(model, pair[0], file_contents, pair[1], y_labs[i])
		i = i + 1

	plt.show()

# Reads in all of the results.txt file and rearranges them into strings
# representing functions.

def get_functions():

	fname = "../../Results/results.txt"

	with open(fname) as f:
		content = f.readlines()

	functions_vec = rearrange_funcs(content)

	return(functions_vec)

# This gets all of the functions from the results.txt files and puts them in a
# vector. The way it works is that it senses 'END OF SOLUTION' and that way
# knows that it is ont the next function definitions for the model. It then
# reads the first line as the function, breaks at the spaces and skips the first
# two parts of the array as they are the error and complexity. The rest of the
# vector is then saved as the function.

def rearrange_funcs(content):

	counter = 0
	functions_vec = []
	first_pass = True

	for row in content:
		temp_row = []
		counter = counter + 1
		if("END OF SOLUTION" in row):
			first_pass = False
			counter = 0
		if (counter == 5 and first_pass == False) or (first_pass == True and counter == 4):
			spl_string = row.split(' ')
			for entry in spl_string:
				new_entry = entry.replace('\n', '')
				temp_row.append(new_entry.strip())
			str_list = filter(None, temp_row)
			str_list = str_list[2:]
			func = ' '.join(str_list)
			functions_vec.append(func)

	return(functions_vec)	

# This organises the functions. It goes through all of the variables and replaces
# the power (^ is power in Eureqa, but we need math.pow in Python). It then
# replaces all of the trig functions with the Python equivalents and filters gaps
# from the results vector.

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

# Replaces all of the trig functions with python friendly versions.

def replace_trig(new_func):

	new_func = new_func.replace("sin", "math.sin")
	new_func = new_func.replace("cos", "math.cos")

	return(new_func)

# This creates the function declarations, formatting them with the correct
# whitespace for python.

def create_functions(organised):

	func_names = ["dS", "dP1", "dP2", "dA1", "dA2", "dR1", "dR2"]
	func_strings = []

	for i in range(0, len(func_names)):
		f_string = make_func_string(func_names[i], organised[i])
		func_strings.append(f_string)

	print

	return(func_strings)

# This creates function strings formatted for Python that can be executed within
# the program to make the model.

def make_func_string(func_name, func):

	func_string = "def " + func_name + "(S, P1, P2, A1, A2, R1, R2):\n" +	\
				  "\tresult = " + func + "\n" +			\
				  "\treturn(result)"

	return(func_string)

# Running main.

if __name__ == "__main__":
	main()