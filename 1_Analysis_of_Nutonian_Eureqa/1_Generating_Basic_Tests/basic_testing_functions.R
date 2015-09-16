# - James Collerton
# - Student Number 46114
# - Source Code for MSc Thesis

# This is the code to generate the original basic testing functions for Eureqa.
# The main is at the bottom and runs all of the functions, as well as plotting
# the Eureqa discovered solutions. You may need to create two directories
# wherever you want to output the data and images to, or you can comment out 
# the lines.

############################################
# Packages and libraries

# Uncomment out these lines if you need to install the libraries for the script.
# install.packages("scatterplot3d")
# install.packages("ggplot2")
# install.packages("plyr")

library("grid")
library("ggplot2")
library("scatterplot3d")

# Sets the output for the .csv files and the random seed so we get the same
# results each time.
setwd("~/Documents/Programming/Git_Folders/MSc-Thesis-Code/Testing_Function_Results")
set.seed(1000)


############################################
# Linear function with normal noise.

create_linear_func <- function(x, gradient, mean, std_dev)
{    
  y = gradient * x
  
  noise = rnorm( length(y), mean, std_dev )
  
  y_noise = y + noise
  
  return(y_noise)
}

#############################################

#############################################
# Polynomial function with normal noise (note, begins at x^1).

create_polynomial <- function(x, coefficients, mean, std_dev)
{
  y = 0
  
  for(i in 1:length(coefficients)){ y = y + x^i * coefficients[i]}
  
  noise = rnorm( length(y), mean, std_dev )
  
  y_noise = y + noise
  
  return(y_noise)
}

#############################################

#############################################
# Sin wave function with normal noise.

create_sin_wave <- function(x, outer_coeff, inner_coeff, mean, std_dev)
{ 
  y = outer_coeff * sin( inner_coeff * x)
  
  noise = rnorm( length(y), mean, std_dev )
  
  y_noise = y + noise
  
  return(y_noise)
}

#############################################

#############################################
# Exponential function with normal noise.

create_exponential <- function(x, outer_coeff, inner_coeff, mean, std_dev)
{
  y = outer_coeff * exp( inner_coeff * x )
  
  noise = rnorm( length(y), mean, std_dev )
  
  y_noise = y + noise
  
  return(y_noise)
}

#############################################
# Step function with normal noise.

create_step_func <- function(x, start, fin, oscill, mean, std_dev)
{
  oscill_point = length(x) / oscill
  
  y = rep(start, length(x))
  
  for(i in 2:length(x)){
    
    if(i %% oscill_point == 0){
      
      if(y[i-1] == start){y[i] = fin}
      else if(y[i-1] == fin){y[i] = start}
      
    }
    else{ y[i] = y[i-1] }
    
  }
  
  noise = rnorm( length(y), mean, std_dev )
  
  y_noise = y + noise
  
  return(y_noise)
}

#############################################

#############################################
# Polynomial with sin factor.

polynomial_sin <- function(x, coefficients, inner_coef, outer_coef,
                           mean, std_dev)
{
  y = create_polynomial(x, coefficients, 0, 0)
  y = y + create_sin_wave(x, inner_coef, outer_coef, 0, 0)
  
  noise = rnorm( length(y), mean, std_dev )
  y_noise = y + noise
  return(y_noise)
}

#############################################

#############################################
# Polynomial with step function.

polynomial_step <- function(x, coefficients, start, fin, oscill,
                            mean, std_dev)
{
  y = create_polynomial(x, coefficients, 0, 0)
  y = y + create_step_func(x, start, fin, oscill, 0, 0)
  
  noise = rnorm( length(y), mean, std_dev )
  y_noise = y + noise
  return(y_noise)
}

#############################################

#############################################
# Linear function in 3D with noise.

threed_lin_fun <- function(x, y_coeff, z_coeff, mean, std_dev)
{
  y = y_coeff * x
  z = z_coeff * y
  
  y_noise = rnorm( length(y), mean, std_dev )
  z_noise = rnorm( length(z), mean, std_dev )
  
  y_fin = y + y_noise
  z_fin = z + z_noise
  
  finished_vec = cbind(x, y_fin, z_fin)
  
  return(finished_vec)
}

#############################################

#############################################
# Polynomial with step function.

threed_polynomial <- function(x, y_coeff, z_coeff, mean, std_dev)
{
  y = create_polynomial(x, y_coeff, 0, 0)
  z = create_polynomial(x, z_coeff, 0, 0)
  
  y_noise = rnorm( length(y), mean, std_dev )
  z_noise = rnorm( length(z), mean, std_dev )
  
  y_fin = y + y_noise
  z_fin = z + z_noise
  
  finished_vec = cbind(x, y_fin, z_fin)
  
  return(finished_vec)
}

#############################################

#############################################
# 3D function with step function.

threed_step <- function(x, start, fin, oscill, z_coeff,
                        mean, std_dev)
{
  y = create_step_func(x, start, fin, oscill, 0, 0)
  z = create_polynomial(x, z_coeff, 0, 0)
  
  y_noise = rnorm( length(y), mean, std_dev )
  z_noise = rnorm( length(z), mean, std_dev )
  
  y_fin = y + y_noise
  z_fin = z + z_noise
  
  finished_vec = cbind(x, y_fin, z_fin)
  
  return(finished_vec)
}

#############################################

#############################################
# Function to write 2D functions to CSV.

write_twod_csv <- function(x, y, filename, info_1, info_2)
{
  setwd("~/Documents/Programming/Git_Folders/MSc-Thesis-Code/Testing_Function_Results")
  data_matrix = cbind(x, y)
  data_matrix = rbind(c(info_1, info_2), data_matrix )
  write.csv(data_matrix, filename, row.names = FALSE)
}

#############################################

#############################################
# Function to write 3D functions to CSV.

write_threed_csv <- function(x, y, z, filename, info_1, info_2, info_3)
{
  setwd("~/Documents/Programming/Git_Folders/MSc-Thesis-Code/Testing_Function_Results")
  data_matrix = cbind(x, y, z)
  data_matrix = rbind(c(info_1, info_2, info_3), data_matrix )
  write.csv(data_matrix, filename, row.names = FALSE)
}

#############################################

#############################################
# Creating a 2D qplot

use_twod_q_plot <- function(x, y, fn, mean, std_dev, filename)
{
  setwd("~/Documents/Programming/Git_Folders/MSc-Thesis-Code/Testing_Function_Plots")
  
  jpeg(filename)
  plot(x, y, col = "dodgerblue", pch = 16)
  dev.off()
}

#############################################

#############################################
# Creating a 3D scatterplot

threed_scatplot <- function(x, y, z, fn, mean, std_dev, filename)
{
  setwd("~/Documents/Programming/Git_Folders/MSc-Thesis-Code/Testing_Function_Plots")
  
  scatterplot3d(x, y, z, color = "dodgerblue2", pch = 16)
  
  png(filename, width = 12, height = 8, units="in", res=300)
  scatterplot3d(x, y, z, color = "dodgerblue2", pch = 16)
  dev.off()
}

#############################################

#############################################
# Create and plot 2D linear func

linear_func <- function(x, mean, std_dev, grad, csv_file, img_file, fn)
{
  y = create_linear_func(x, grad, mean, std_dev)
  write_twod_csv(x, y, csv_file, "x", "y")
  use_twod_q_plot(x , y, fn, mean, std_dev, img_file)

  return(y)
}

#############################################

#############################################
# Create and plot 2D exponential func

exponential_func <- function(x, mean, std_dev, outer_coeff, inner_coeff,
                             csv_file, img_file, fn)
{  
  y = create_exponential(x, outer_coeff, inner_coeff, mean, std_dev)
  write_twod_csv(x, y, csv_file, "x", "y")
  use_twod_q_plot(x , y, fn, mean, std_dev, img_file)
}

#############################################

#############################################
# Create and plot 2D polynomial func

polynomial_func <- function(x, mean, std_dev, coefficients,
                            csv_file, img_file, fn)
{
  y = create_polynomial(x, coefficients, mean, std_dev)
  write_twod_csv(x, y, csv_file, "x", "y")
  use_twod_q_plot(x , y, fn, mean, std_dev, img_file)
}

#############################################

#############################################
# Create and plot 2D sin func

sin_func <- function(x, mean, std_dev, outer_coeff, inner_coeff,
                     csv_file, img_file, fn)
{
  y = create_sin_wave(x, outer_coeff, inner_coeff, mean, std_dev)
  write_twod_csv(x, y, csv_file, "x", "y")
  use_twod_q_plot(x , y, fn, mean, std_dev, img_file)
}

#############################################

#############################################
# Create and plot 2D step func

step_func <- function(x, mean, std_dev, start, fin,
                      oscill, csv_file, img_file, fn)
{
  mean = 2
  std_dev = 1
  y = create_step_func(x, 0, 20, 10, mean, std_dev)
  write_twod_csv(x, y, "step_function.csv", "x", "y")
  use_twod_q_plot(x , y, "y = step(20, 10),", 
                  mean, std_dev, "2D_step.jpg")
}

#############################################

#############################################
# Create and plot 2D polynomial and sin func

polynomial_sin_func <- function(x, mean, std_dev, coefficients, outer_coeff,
                                inner_coeff, csv_file, img_file, fn)
{
  y = polynomial_sin(x, coefficients, outer_coeff, inner_coeff, mean, std_dev)
  write_twod_csv(x, y, csv_file, "x", "y")
  use_twod_q_plot(x , y, fn, mean, std_dev, img_file)
}

#############################################

#############################################
# Create and plot 2D polynomial and step func

polynomial_step_func <- function(x, mean, std_dev, coefficients, start, fin,
                                 oscill, csv_file, img_file, fn)
{
  y = polynomial_step(x, coefficients, start, fin, oscill, mean, std_dev)
  write_twod_csv(x, y, csv_file, "x", "y")
  use_twod_q_plot(x , y, fn, mean, std_dev, img_file)

  return(y)
}

#############################################

#############################################
# Create and plot 3D linear func

threed_linear <- function(x, mean, std_dev, y_coeff, z_coeff,
                          csv_file, img_file, fn)
{
  fin_vec = threed_lin_fun(x, y_coeff, z_coeff, mean, std_dev)
  write_threed_csv(fin_vec[,"x"], fin_vec[,"y_fin"], fin_vec[,"z_fin"],
                   csv_file, "x" ,"y", "z")
  threed_scatplot(fin_vec[,"x"], fin_vec[,"y_fin"], fin_vec[,"z_fin"],
                  fn, mean, std_dev, img_file)
}

#############################################

#############################################
# Create and plot 3D polynomial func

threed_polynomial_func <- function(x, mean, std_dev, y_coeff, z_coeff,
                                   csv_file, img_file, fn)
{
  fin_vec = threed_polynomial(x, y_coeff, z_coeff, mean, std_dev)
  write_threed_csv(fin_vec[,"x"], fin_vec[,"y_fin"], fin_vec[,"z_fin"],
                   csv_file, "x" ,"y", "z")
  threed_scatplot(fin_vec[,"x"], fin_vec[,"y_fin"], fin_vec[,"z_fin"],
                  fn, mean, std_dev, img_file)
}

#############################################

#############################################
# Create and plot 3D polynomial func

threed_step_func <- function(x, mean, std_dev, start, fin, oscill, z_coeff,
                             csv_file, img_file, fn)
{
  fin_vec = threed_step(x, start, fin, oscill, z_coeff, mean, std_dev)
  write_threed_csv(fin_vec[,"x"], fin_vec[,"y_fin"], fin_vec[,"z_fin"],
                   csv_file, "x" ,"y", "z")
  threed_scatplot(fin_vec[,"x"], fin_vec[,"y_fin"], fin_vec[,"z_fin"],
                  fn, mean, std_dev, img_file)
}

#############################################

#############################################
# Main.

# First part creates all the functions and writes them to .csv. Second part plots
# the Eureqa derived solutions.

main <- function()
{
  ##############################################################################
  # Writes all the .csv files and prints the .jpegs

  x = seq(0, 99, 1)
  
  linear_func(x, 0, 10, 1, "linear_function.csv", "linear.jpg", "y = x,")

  exponential_func(x, 0, 30, 1, 1/15, "exponential_function.csv", 
                   "exponential.jpg", "y = exp(1/15 * x),")
  
  polynomial_func(x, 0, 100, c(1/100, 1/400, 1/500), "polynomial_function.csv",
                  "polynomial.jpg", "y = 1/100 * x^2 + 1/400 * x + 1/500,")
  
  sin_func(x, 0, 0.25, 1, 1/10, "sin_function.csv",
           "sin.jpg", "y = sin(1/10 * x),")
  
  step_func(x, 0, 1, 0, 20, 10, "step_function.csv", "2D_step.jpg",
            "y = step(20, 10),")
  
  polynomial_sin_func(x, 0, 0, c(1/1000, 1/4000, 1/500), 100, 1,
                      "polynomial_sin_function.csv", "polynomial_sin.jpg",
                      "y = 1/1000*x^2 + 1/40000*x +1/5000*x + 100*sin(x),")
  
  polynomial_step_func(x, 0, 3, c(1/1000, 1/4000, 1/500), 0, 200, 20,
                       "polynomial_step_function.csv", "polynomial_step.jpg",
                       "y = 1/1000*x^2 + 1/40000*x +1/5000*x + step(20, 20),")
  
  threed_linear(x, 0, 20, 3, 4, "threed_linear_func.csv", "3D_Linear_Func.png",
                "Function: y = 3 * x, z = 4 * y" )
  
  threed_polynomial_func(x, 0, 20, c(1/100, 1/200, 1/300), c(1/100, 1/200, 1/300),
                         "threed_polyn_func.csv", "3D_Polynomial.png", 
                         "Function: y = z = 1/100*x^2 + 1/200*x + 1/300")
  
  threed_step_func(x, 0, 1, 0, 20, 20, c(1/100, 1/200, 1/300), "threed_step_func.csv",
                   "3D_Polynomial_Step.png", "Function:y = 1/100*x^2 + 1/200*x + 1/300, z = step(20, 20)")
  

  ##############################################################################
  # Plotting Eureqa results.

  y = create_linear_func(x = x, gradient = 1, mean = 0, std_dev = 10)
  plot(y, col = "dodgerblue", pch = 16, xlab = "x", ylab = "y", cex = 0.75)
  lines(x, col = "deeppink", lwd = 4)

  y = create_polynomial(x = x, coefficients = c(1/100, 1/400, 1/500), 0, 100)
  plot(y, col = "dodgerblue", pch = 16, xlab = "x", ylab = "y", cex = 0.75)
  lines(0.00205* x^3, col = "deeppink", lwd = 4)

  y = create_exponential(x = x, 1, 1/15, 0, 30)
  plot(y, col = "dodgerblue", pch = 16, xlab = "x", ylab = "y", cex = 0.75)
  lines(exp(1/15* x), col = "deeppink", lwd = 4)

  y = create_sin_wave(x = x, outer_coeff = 1, inner_coeff = 1/10, mean = 0, std_dev = 0.25)
  plot(y, col = "dodgerblue", pch = 16, xlab = "x", ylab = "y", cex = 0.75)
  lines(sin(0.101 * x), col = "deeppink", lwd = 4)

  y = polynomial_sin(x = x, coefficients = c(1/100, 1/400, 1/500), inner_coef = 100, outer_coef = 1, mean = 0, std_dev = 0)
  plot(y, col = "dodgerblue", pch = 16, xlab = "x", ylab = "y", cex = 0.75)
  lines(0.00200 * x^3 + 100.00549 * sin(x), col = "deeppink", lwd = 2)

  y = create_step_func(x = x, start = 0, fin = 20, oscill = 10, mean = 0, std_dev = 1)
  plot(y, col = "dodgerblue", pch = 16, xlab = "x", ylab = "y", cex = 0.75)
  lines(19.96893 * ( sin(3.57221 - 12.25059 * x) > 0 ), col = "deeppink", lwd = 2)

  y = polynomial_step(x = x, coefficients = c(1/100, 1/400, 1/500), start = 0, fin = 200, oscill = 20, mean = 0, std_dev = 3)
  plot(y, col = "dodgerblue", pch = 16, xlab = "x", ylab = "y", cex = 0.75)
  lines(0.00200 * x^3 + 199.65703 * ( sin(5.61195 + 11.93316 * x) > 0 ), col = "deeppink", lwd = 2)

  xyz_vec = threed_lin_fun(x, 3, 4, 0, 20)
  s3d <- scatterplot3d(xyz_vec[,1], xyz_vec[,2], xyz_vec[,3], color = "dodgerblue2", pch = 16,
                       cex.symbols = 0.7, xlab = "x", ylab = "y", zlab = "z")
  s3d$points3d(x, 3 * x, 12 * x, type = "l",  col = "deeppink", lwd = 3)

  xyz_vec = threed_polynomial(x = x, y_coeff = c(1/100, 1/200, 1/300), z_coeff = c(1/100, 1/200, 1/300), mean = 0, std_dev = 20)
  s3d <- scatterplot3d(xyz_vec[,1], xyz_vec[,2], xyz_vec[,3], color = "dodgerblue2", pch = 16,
                       cex.symbols = 0.7, xlab = "x", ylab = "y", zlab = "z")
  s3d$points3d(x, 0.00309 * x^3, 0.00309 * x^3, type = "l",  col = "deeppink", lwd = 3)
  
  xyz_vec = threed_step(x = x, start = 0, fin = 20, oscill = 20, z_coeff = c(1/100, 1/200, 1/300), mean = 0, std_dev = 1)
  s3d <- scatterplot3d(xyz_vec[,1], xyz_vec[,2], xyz_vec[,3], color = "dodgerblue2", pch = 16,
                       cex.symbols = 0.7, xlab = "x", ylab = "y", zlab = "z")
  s3d$points3d(x, 19.73796 * (sin(-0.64033  * x) > 0), 0.00309 * x^3, type = "l",  col = "deeppink", lwd = 3)

}

#############################################
# Calling main.

main()

#############################################