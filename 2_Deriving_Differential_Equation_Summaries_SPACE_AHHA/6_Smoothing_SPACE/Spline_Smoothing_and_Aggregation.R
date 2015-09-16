# - James Collerton
# - Student Number 46114
# - Source Code for MSc Thesis

# This is the code for the nonparametric regression based smoothing of the SPACE
# data. It will take in a number of files in the correct folder, and then output
# their smoothed equivalents in an output file. It will also append them back to
# back for the multi emigration derivation case, even though the project never
# extended to this.

# NOTE: You will have to change the directories for the script to function on
# your machine.

################################################################################
# Libraries

library("ftnonpar")

################################################################################

################################################################################
# Global variables

# These are indices of the input file dataframe. All of the S data is in the
# first col, the P0 data in the second etc.

S_ind = 1
P0_ind = 2
P1_ind = 3
P2_ind = 4
R1_ind = 5
R2_ind = 6
A1_ind = 7
A2_ind = 8
Q_ind = 9
t_ind = 10

# Setting so that we only plot one graph per frame (not necessary but will
# overwrite any previous settings if they're there.)

par(mfrow = c(1,1))

################################################################################

# Gets all of the directories within the given folder. Then runs smoothing and
# puts them all back to back. We remove the All_Data_Smoothed folder as we don't
# want to resmooth smoothed things, and delete its contents.

main <- function(){

	dir_string = "~/Documents/Programming/Git_Folders/MSc-Thesis-Code/Submission/2_Deriving_Differential_Equation_Summaries_SPACE_AHHA/6_Smoothing_SPACE/Data_for_Write_Up"

	setwd(dir_string)

	directories <- list.dirs(full.names = TRUE, recursive = FALSE)

	directories <- directories[directories != "./All_Data_Smoothed"]

	delete_all_files(dir_string)

	run_smoothing(directories, dir_string)

	put_back_to_back(dir_string)

}

# Deletes all files so we don't resmooth things that have already been smoothed.

delete_all_files <- function(dir_string){

	setwd(paste(dir_string, "All_data_smoothed", sep = "/"))

	filenames <- list.files(path = ".", pattern = NULL, all.files = FALSE,
				            full.names = FALSE, recursive = FALSE,
				            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

	for(single_file in filenames){
		unlink(single_file)
	}

}

# For each directory it gets all of the filenames, finds the population size
# and quorum and then smooths the file. It will then create a file with
# a current file number which is incremented.

run_smoothing <- function(directories, dir_string){

	file_number = 0

	for(dir in directories){
		if(dir != "All_data_smoothed"){

			setwd(paste(dir_string, dir, sep = "/"))

			filenames <- list.files(path = ".", pattern = NULL, all.files = FALSE,
				            full.names = FALSE, recursive = FALSE,
				            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

			pop_size = unique(na.omit(as.numeric(unlist(strsplit(unlist(dir), "[^0-9]+")))))[1]
			quor_thresh = unique(na.omit(as.numeric(unlist(strsplit(unlist(dir), "[^0-9]+")))))[2]

			for(single_file in filenames){
				if(single_file != "output.txt"){
					file_number = file_number + 1
					setwd(paste(dir_string, dir, sep = "/"))
					smooth_file(single_file, dir_string, file_number, pop_size, quor_thresh)
				}
			}
		}
	}

}

# This plot role function is not necessary, but is useful in seeing the output
# of the smoothing and sanity checking it.

plot_role <- function(spline, underlying, leg_x, leg_y, ylabel, y_limit){

	plot(underlying, type = "l", col = "dodgerblue",
				 xlab = "Time", ylab = ylabel, lwd = 2,
				 ylim = y_limit)
	lines(spline$y, col = "firebrick1", lwd = 2)

	legend(leg_x, leg_y,
	       c("Underlying", "Global Behaviour"),
	       lty=c(1, 1),
	       lwd=c(2, 2),
	       col=c("dodgerblue", "firebrick1"),
	       cex = 0.7 )

}

# Used to create the dRole vectors to ease the discovery of the differential
# as described in the thesis.

create_diff_vec <- function(func){
  
  return(func[ 2 : length(func) ] - func[ 1: (length(func) - 1) ])
  
}

# This function is not currently in the code, but can be put in to view how
# nonparametric regression based smoothing simplifies the problem of discovering
# the differential.

plot_differentials <- function(spline, underlying){

	par(mfrow = c(1,2))

	plot_role(spline, underlying, 475, 43, "Number of Scouts", c(12, 43))
	
	spline_diff = create_diff_vec(spline$y)
	underlying_diff = create_diff_vec(underlying)

	plot(underlying_diff, type = "l", col = "dodgerblue", lty = 2,
						  xlab = "Time", ylab = "D(Scouts)")
	lines(spline_diff, type = "l", col = "firebrick1", lwd = 2)

}

# This actually carries out the smoothing of the file. For each role a number
# of degrees of freedom are specified, which roughly translates to a sensitivity 
# to the data. Higher degrees of freedom mean a higher sensitivity. These can
# be altered to create more or less complex curves. For the purpose of this project,
# they were specified to give what was deemed as appropriate results.

# Smoothing can lead to values less than zero, which is not realistic in our
# application, and so the values are floored there. We then create the quorum,
# time and colony size vectors.

smooth_file <- function(single_file, dir_string, file_number, pop_size, quor_thresh){

	single_file = read.csv(single_file)

	S = create_individual_spline(single_file$S, 7)
	A1 = create_individual_spline(single_file$A1, 7)
	A2 = create_individual_spline(single_file$A2, 7)
	P0 = create_individual_spline(single_file$P0, 10)
	P1 = create_individual_spline(single_file$P1, 10)
	P2 = create_individual_spline(single_file$P2, 10)
	R1 = create_individual_spline(single_file$R1, 10)
	R2 = create_individual_spline(single_file$R2, 10)

	S$y[S$y < 0] <- 0
	A1$y[A1$y < 0] <- 0
	A2$y[A2$y < 0] <- 0
	P0$y[P0$y < 0] <- 0
	P1$y[P1$y < 0] <- 0
	P2$y[P2$y < 0] <- 0
	R1$y[R1$y < 0] <- 0
	R2$y[R2$y < 0] <- 0

	Q = rep(quor_thresh, nrow(single_file))
	t = seq(from = 1, to = nrow(single_file))
	N = rep(pop_size, nrow(single_file))

	create_write_dataframe(S$y, A1$y, A2$y, P0$y, P1$y, P2$y, R1$y, R2$y, Q, t, N, dir_string, file_number)

}

# Creates and writes a dataframe to file in the output folder.

create_write_dataframe <- function(S, A1, A2, P0, P1, P2, R1, R2, Q, t, N, dir_string, file_number){

	setwd(paste(dir_string, "All_data_smoothed", sep = "/"))

	write_df <- data.frame(S, A1, A2, P0, P1, P2, R1, R2, Q, t, N)

	file_name = paste("output_", file_number, sep = "", ".txt")

	write.csv(write_df, file_name, row.names = FALSE)

}

# This creates a spline smoothed output with the given degrees of freedom.

create_individual_spline <- function(data, deg_free){

	x = seq(1, length(data))
	spline_res <- smooth.spline(x, data, df = deg_free)

	return(spline_res)

}

# As outlined in the earlier sections of the thesis, we devise a method of
# simplifying the differential equation discovery problem by precalculating
# the differentials and appending them back to back.

put_back_to_back <- function(dir_string){

	setwd(paste(dir_string, "All_data_smoothed", sep = "/"))

	filenames <- list.files(path = ".", pattern = NULL, all.files = FALSE,
	            full.names = FALSE, recursive = FALSE,
	            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

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

	for(single_file in filenames){

		file_table = read.csv(single_file)

		S = c(S, file_table$S)
		P0 = c(P0, file_table$P0)
		P1 = c(P1, file_table$P1)
		P2 = c(P2, file_table$P2)
		A1 = c(A1, file_table$A1)
		A2 = c(A2, file_table$A2)
		R1 = c(R1, file_table$R1)
		R2 = c(R2, file_table$R2)
		N = c(N, file_table$N)
		Q = c(Q, file_table$Q)
		t = c(t, file_table$t)

	}
	
	write_df <- data.frame(S, A1, A2, P0, P1, P2, R1, R2, Q, t, N)

	write.csv(write_df, "final_back_to_back.txt", row.names = FALSE)
}

################################################################################
# Running main.

main()