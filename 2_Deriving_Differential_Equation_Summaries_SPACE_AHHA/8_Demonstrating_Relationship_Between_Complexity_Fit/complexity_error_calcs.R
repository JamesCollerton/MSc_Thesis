# - James Collerton
# - Student Number 46114
# - Source Code for MSc Thesis

# This is the code for the statistics tests relating complexity to accuracy over
# the whole model. It takes in the complexities of the different functions and
# runs the tests as documented in the thesis.

################################################################################
# Function declarations.

# Function for changing the opacity of the colours in the bar chart so they're
# more readable.
add.alpha <- function(col, alpha=1){

  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
                     function(x) 
                       rgb(x[1], x[2], x[3], alpha=alpha)) 

}

# Adding error bars to the barplot
error.bar <- function(x, y, upper, lower=upper, length=0.1, ...){

	if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
	stop("vectors must be same length")
	arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, lwd = 2, ...)

}

################################################################################

# Reading in all of the values for the tests.

# The complexity values for each of the terms in each of the models.
planque_q1_complex_vals <- c(4, 1, 4, 1, 1)
planque_q10_complex_vals <- c(6, 3, 5, 1, 1)
planque_var_complex_vals <- c(9, 1, 4, 2, 1)
pratt_q1_complex_vals <- c(4, 3, 2, 2, 2, 2, 2)
pratt_q10_complex_vals <- c(4, 4, 4, 2, 2, 0, 1)
pratt_var_complex_vals <- c(3, 6, 6, 2, 2, 1, 1)
SPACE_complex_vals <- c(7, 10, 8, 8, 7, 3, 5)
AHHA_complex_vals <- c(6, 5, 10, 12, 1, 7, 1)

# Taking the mean complexities.
planque_q1_complex <- mean(planque_q1_complex_vals)
planque_q10_complex <- mean(planque_q10_complex_vals)
planque_var_complex <- mean(planque_var_complex_vals)

pratt_q1_complex <- mean(pratt_q1_complex_vals)
pratt_q10_complex <- mean(pratt_q10_complex_vals)
pratt_var_complex <- mean(pratt_var_complex_vals)

SPACE_complex <- mean(SPACE_complex_vals)
AHHA_complex <- mean(AHHA_complex_vals)

# Standard deviations to make the error bars.
complexity_sds <- c( sd(planque_q1_complex_vals),
					 sd(planque_q10_complex_vals),
					 sd(planque_var_complex_vals),
					 sd(pratt_q1_complex_vals),
					 sd(pratt_q10_complex_vals),
					 sd(pratt_var_complex_vals),
					 sd(SPACE_complex_vals),
					 sd(AHHA_complex_vals))

# R squared values
pratt_q1_R_sq <- 0.9982459
pratt_q10_R_sq <- 0.997952
pratt_var_R_sq <- 0.9872605

planque_q1_R_sq <- 0.9995744
planque_q10_R_sq <- 0.9997239
planque_var_R_sq <- 0.9972632

SPACE_R_sq <- 0
AHHA_R_sq <- 0

# The average complexities of the different model components.
complexities <- c(planque_q1_complex,
				  planque_q10_complex,
				  planque_var_complex,
				  pratt_q1_complex,
				  pratt_q10_complex,
				  pratt_var_complex,
				  SPACE_complex,
				  AHHA_complex)

R_sq_vals <- c(planque_q1_R_sq,
			   planque_q10_R_sq,
			   planque_var_R_sq,
			   pratt_q1_R_sq,
			   pratt_q10_R_sq,
			   pratt_var_R_sq,
			   SPACE_R_sq,
			   AHHA_R_sq)

# The names for each of the models.
names <- c("Planque Q1",
			"Planque Q10",
			"Planque Var",
			"Pratt Q1",
			"Pratt Q10",
			"Pratt Var",
			"SPACE",
			"AHHA")

################################################################################

# Create the dataframe of the average complexities, R squared values, the names
# of the models, standard deviations. That way when we order by complexity we
# order all of the other components.
info_df <- data.frame(complexities, R_sq_vals, names, complexity_sds)

# Ordering the info_df by complexity.
info_df <- info_df[ order(info_df[,1], info_df[,1]), ]

# Setting out the borders for the graphs.
op <- par(mar = c(9,4,2,5) + 0.1)

# Plotting the bars
barx <- barplot(info_df$complexities, names.arg = info_df$names,
		las = 2, cex.names = 0.5,
		xlab = "", ylab = "Average Complexity per Equation",
		cex.axis = 0.8, col = add.alpha("firebrick1", 0.6), border = NA)

# Adding the bars.
error.bar(barx, info_df$complexities, 1.96 * info_df$complexity_sds / 8)

# Setting it so we can plot two graphs over each other.
par(new = TRUE)

# Plots the R squared valeus and adds an axis
plot(info_df$R_sq_vals, xaxt = "n", yaxt = "n", bty = "n", type = "o", pch = 16,
			    col = "dodgerblue4", xlab = "", ylab = "", lwd = 2)
axis(4, cex.axis = 0.8)
mtext("Average R Squared Value per Model", side=4, line=3)

# Adds legend.
par(xpd=TRUE)
legend("bottom", inset=c(-2,-0.4), legend=c("Average Complexity","Average R Squared"),
				 lwd = c(2, 2), title="Model", cex = 0.6,
				 col = c("firebrick1", "dodgerblue4") )

################################################################################

# Checking the PPMCC

check_corr <- data.frame(complexities, R_sq_vals) 

cor.test(check_corr$R_sq_vals, check_corr$complexities)

################################################################################