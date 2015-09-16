dir_string = "~/Documents/Programming/Git_Folders/MSc-Thesis-Code/Pratt_Planque_Equation_Checking/Data/Planque/Var_Parameters"

setwd(dir_string)

all_data = read.csv("var_simulation_results.txt")

split_points = which(all_data$t == 1)

run_1 = all_data[ split_points[1] : split_points[2] - 1, ]
run_2 = all_data[ (split_points[2] + 1) : split_points[3] - 1, ]
run_3 = all_data[ (split_points[3] + 1) : split_points[4] - 1, ]  
run_4 = all_data[ (split_points[4] + 1) : split_points[5] - 1, ]
run_5 = all_data[ (split_points[5] + 1) : split_points[6] - 1, ]
run_6 = all_data[ (split_points[6] + 1) : split_points[7] - 1, ]  
run_7 = all_data[ (split_points[7] + 1) : split_points[8] - 1, ]
run_8 = all_data[ (split_points[8] + 1) : split_points[9] - 1, ]
run_9 = all_data[ (split_points[9] + 1) : split_points[10] - 1, ] 
run_10 = all_data[ (split_points[10] + 1) : nrow(all_data), ]  

runs_list = list(run_1, run_2, run_3, run_4, run_5, run_6, run_7, run_8, run_9, run_10)
names = c("run_1.txt", "run_2.txt", "run_3.txt", "run_4.txt", "run_5.txt", 
		  "run_6.txt", "run_7.txt", "run_8.txt", "run_9.txt", "run_10.txt")

for( i in 1:length(runs_list) ){ write.csv(runs_list[[i]], names[i], row.names = FALSE) }