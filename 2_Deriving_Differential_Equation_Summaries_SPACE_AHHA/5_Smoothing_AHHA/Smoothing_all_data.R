# - James Collerton
# - Student Number 46114
# - Source Code for MSc Thesis

# This is the code for smoothing all of the AH-HA data. It implements the
# nonparametric-based methodology as outlined in the thesis, taking in all
# files within a given directory and smoothing them individually.

################################################################################
# Libraries

library("plyr")

################################################################################
# Global variables

# Directory string is used for the current directory, we then loop through each
# of the contained directories.

# Here we have all of the files directly in that folder, but if you wanted to
# drop several folders worth of files in you could do that here.

dir_string = "~/Documents/Programming/Git_Folders/MSc-Thesis-Code/Submission/2_Deriving_Differential_Equation_Summaries_SPACE_AHHA/5_Smoothing_AHHA/Data"
# directories = c("CS_150_Q_1", "CS_175_Q_5", "CS_200_Q_8", "CS_225_Q_10", "CS_250_Q_12")
directories = c("")

# Row and column indices for the dataframes created. Appends these and creates a
# vector in order to provide labels for plots.

N = 1
Q = 2
S = 3
P0 = 4
P1 = 5
P2 = 6
R1 = 7
R2 = 8
A1 = 9
A2 = 10

role_vec = c(S, P1, P2, A1, A2, R1, R2)
description_vec = c("Number of Scouts",
					"Number of Passive (Site One)",
					"Number of Passive (Site Two)",
					"Number of Assessors (Site One)",
					"Number of Assessors (Site Two)",
					"Number of Recruiters (Site One)",
					"Number of Recruiters (Site Two)")

################################################################################

# Gets all of the files within a folder and makes a list out of their contents.

get_all_files <- function(filenames, all_files){

	for(single_file in filenames){ 
	  if(single_file != "output.txt"){
	    all_files[[length(all_files) + 1]] = read.csv(single_file)  
	  }
	}

	return(all_files)	

}

# This is the bandwidth driven custom smoothing function for the assessors at
# site one.

create_piecewise_smooth <- function(data){

	# Finds maximum point
	maximum_point = max(data)
	max_ind_vec = which(data == maximum_point)
	max_ind = max_ind_vec[length(max_ind_vec)]

	# Finds bandwidth around point.
	start_of_curve = 1
	end_of_curve = max_ind

	# Finds curve to smooth
	to_smooth = data[start_of_curve: end_of_curve]
	smoothed_spline = create_individual_spline(to_smooth, 6)
	smoothed_curve = smoothed_spline$y
	smoothed_curve[1] = 0

	# Attaches rest of data
	unsmoothed = data[end_of_curve + 1: length(data)]
	unsmoothed = unsmoothed[!is.na(unsmoothed)]
	end_spline = create_individual_spline(unsmoothed, 10)
	smoothed_end = end_spline$y
	smoothed_end[1] = smoothed_curve[length(smoothed_curve)] 

	# Finished curve
	finished_curve = c(smoothed_curve, smoothed_end)

	return(finished_curve)

}

# Creates a smoothed curve using spline smoothing.

create_individual_spline <- function(data, deg_free){

	x = seq(1, length(data))
	spline_res <- smooth.spline(x, data, df = deg_free)

	return(spline_res)

}

# The next few functions are used in plotting the results and can be put in
# to see what is going on.

# This function plots the underlying assessors at site one so we can see the
# general shape of the curve.
plot_A1_underlying <- function(aggregated_list){

	plot(aggregated_list[,A1], type = "l", col = "dodgerblue", 
						   	   xlab = "Time", ylab = "Number of Assessors (Site One)",
						   	   lwd = 2,
						   	   ylim = c(0, 18))

	legend(90, 18,
	       c("Underlying"),
	       lty=c(1),
	       lwd=c(2),
	       col=c("dodgerblue"),
	       cex = 0.8 )

}

# This function plots a bandwidth around the maximum point, to show where the 
# two sections of the smoothing method take place. The first part has a higher
# sensitivity to the data (capturing the peak), the second has a lower sensitivity
# (removing the noise)
plot_A1_bandwidth <- function(aggregated_list){

	maximum_point = max(aggregated_list[,A2])
	max_ind_vec = which(aggregated_list[,A2] == maximum_point)
	max_ind = max_ind_vec[length(max_ind_vec)]

	plot(aggregated_list[,A1], type = "l", col = "dodgerblue", 
							   xlab = "Time", ylab = "Number of Assessors (Site One)",
							   lwd = 2,
							   ylim = c(0, 18))
	abline(v = 0, lty = 2, lwd = 2, col = "indianred2")
	abline(v = max_ind, lty = 2, lwd = 3, col = "firebrick1")
	abline(v = max_ind + 7, lty = 2, lwd = 2, col = "indianred2")

	legend(85, 18,
       c("Underlying", "Maximum Point", "Bandwidth"),
       lty=c(1, 2, 2),
       lwd=c(2, 2, 2),
       col=c("dodgerblue", "firebrick1", "indianred2"),
       cex = 0.8 )

}

# Finally we overlay the smoothed function on top, demonstrating that we have
# a reasonable result.
plot_A1_smooth <- function(aggregated_list, smooth_mat){

	smooth_mat[,A1][smooth_mat[,A1] < 0] <- 0

	plot(aggregated_list[,A1], type = "l", col = "dodgerblue", 
							   xlab = "Time", ylab = "Number of Assessors (Site One)",
							   lwd = 2,
							   ylim = c(0, 18))
	lines(smooth_mat[,A1], lwd = 2, col = "firebrick1")

	legend(90, 18,
	       c("Underlying", "Smooth"),
	       lty=c(1, 1),
	       lwd=c(2, 2),
	       col=c("dodgerblue", "firebrick1"),
	       cex = 0.8 )

}

# This is used to plot the smoothed output for each role.
plot_smooth_underlying <- function(aggregated_list, smooth_mat, i, leg_x, leg_y){

	# We know these roles begin at zero, so we set the first value as zero.
	check = role_vec[i]
		if(check == A1 || check == A2 || check == R1 || check == R2){
			smooth_mat[1, check] <- 0
		}

		# We set all negative values to zero.
		set_to_zero = which(aggregated_list[,role_vec[i]] < 0)
		aggregated_list[set_to_zero, role_vec[i]] <- 0

		# Set all of the smoothed negative values to zero
		set_to_zero = which(smooth_mat[,role_vec[i]] < 0)
		smooth_mat[set_to_zero, role_vec[i]] <- 0

		# We plot the role.
		plot(aggregated_list[,role_vec[i]], type = "l", col = "dodgerblue", 
								   xlab = "Time", ylab = description_vec[i],
								   lwd = 2)

		lines(smooth_mat[,role_vec[i]], lwd = 2, col = "firebrick1")

		legend(leg_x, leg_y,
		       c("Underlying", "Global Behaviour"),
		       lty=c(1, 1),
		       lwd=c(2, 2),
		       col=c("dodgerblue", "firebrick1"),
		       cex = 0.6 )

}

# This smooths all of the files. Removes an empty end column of a matrix, then
# loops through all values of the dataframe and smooths them. Treats the A1 and
# A2 columns as special cases in order to capture the initial peaks.
smooth_files <- function(aggregated_list)
{	
	aggregated_list = aggregated_list[, -ncol(aggregated_list)]
	smooth_mat = matrix(0, nrow = nrow(aggregated_list), ncol = ncol(aggregated_list))

	# P1, P2, R1, R2, A1, A2
	smoothing_dfs = c(10, 20, 4, 15, 7, 4, 4)	

	for(colmn in 1:ncol(aggregated_list)){
		if(colmn < 4){ smooth_mat[,colmn] = aggregated_list[,colmn] }
		else{
			x = seq(1, length(aggregated_list[,colmn]))
			if(colmn != A1 && colmn != A2){ 
				spline_res <- smooth.spline(x, aggregated_list[,colmn], df = smoothing_dfs[colmn - 3]) 
				smooth_mat[,colmn] = spline_res$y
			} else if(colmn == A1){
				piecewise_res = create_piecewise_smooth(aggregated_list[,colmn])
				smooth_mat[,colmn] = piecewise_res
			} else if(colmn == A2){
				piecewise_res = create_piecewise_smooth(aggregated_list[,colmn])
				smooth_mat[,colmn] = piecewise_res
			}
		}
	}

	plot_A1_underlying(aggregated_list)
	plot_A1_bandwidth(aggregated_list)
	plot_A1_smooth(aggregated_list, smooth_mat)

	x_coords = c(95,95,95,95,95,95,95)
	y_coords = c(41,3,12,15.2,13.2,18.5,4)

	for(i in 1:length(role_vec)){
		plot_smooth_underlying(aggregated_list, smooth_mat, i, x_coords[i], y_coords[i])
	}

	return(smooth_mat)
}

# Prevents negative values in the number of scouts.

correct_S <- function(scaled_files)
{
	for(i in 1: length(scaled_files)){
		scaled_files[which(scaled_files[,S] < 0), S] = 0
	}

	return(scaled_files)
}

# Sets the initial conditions for the roles where we know the amounts start at
# zero to zero.

set_starts_to_zero <- function(smooth_list){

	for(i in 1:ncol(smooth_list)){
		if(i == A1 || i == A2 || i == R1 || i == R2){
			smooth_list[1, i] = 0
		}
	}

	return(smooth_list)
}

# Used to write the smoothed output to files.

write_to_file <- function(smooth_list, file_counter)
{	

	setwd("~/Documents/Programming/Git_Folders/MSc-Thesis-Code/Submission/2_Deriving_Differential_Equation_Summaries_SPACE_AHHA/5_Smoothing_AHHA/Output_Data")

	write_file_df = data.frame(smooth_list)
	t = seq(from = 1, to = nrow(write_file_df) - 1)
	write_file_df = data.frame(write_file_df[-nrow(write_file_df),], t)

	write_file_df[write_file_df < 0] <- 0 

	write_file_df <- rename(write_file_df, c(
								"X1" = "N", 
								"X2" = "Q",
								"X3" = "S",
								"X4" = "P0",
								"X5" = "P1",
								"X6" = "P2",
								"X7" = "R1",
								"X8" = "R2",
								"X9" = "A1",
								"X10" = "A2" ))

	write_file_df$R1[1] <- 0
	write_file_df$R2[1] <- 0
	write_file_df$A1[1] <- 0
	write_file_df$A2[1] <- 0

	write.csv(write_file_df, paste("AHHA_smoothed_", file_counter, ".txt", sep = ""), row.names = FALSE)
}

################################################################################

# Loops through all of the directories and finds all of the files within each
# of the directories. Then smooths all of the files found.

file_counter = 0

for(dir in directories){

	setwd(paste(dir_string, dir, sep = ""))

	all_files = list()

	filenames <- list.files(path = ".", pattern = NULL, all.files = FALSE,
	                        full.names = FALSE, recursive = FALSE,
	                        ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

	all_files = get_all_files(filenames, all_files)

	test = list()

	for(single_file in all_files){

		test[[length(test) + 1]] <- data.matrix(single_file)
		smooth_list = smooth_files(single_file)
		write_to_file(smooth_list, file_counter)

		file_counter = file_counter + 1

	}

}

################################################################################