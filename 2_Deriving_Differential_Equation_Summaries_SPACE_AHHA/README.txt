- James Collerton
- Student Number 46114
- Source Code for MSc Thesis

This directory contains all of the code used in the attempts to derive the
differential equation summaries of SPACE and AH-HA. There are eight folders:

1. SPACE_Scripts: Within this folder are new C# scripts for the SPACE model 
				  which augment the model to produce the correct form of output
				  for the project.

2. AHHA_Model: Within this folder is the AH-HA model, repackaged to be used with
			   current technologies, as well as all of the necessary libraries
			   and build instructions. The model has been rebuilt for this project
			   and to output the correct form of data. Written in Java. Also
			   included is a .pdf of instructions for running the AH-HA model. It
			   was built in 2004, and so requires some work in order to get it
			   to run.

3. AHHA_Training_Data: This folder does not contain the training data, but a
				       link has been provided to a website where the data is hosted.
				       Alternatively you can build and run the AH-HA model with the
				       required parameters using the code in 2.

4. SPACE_Training_Data: This folder contains a link to where all of the SPACE data
						is hosted online.

5. Smoothing_AHHA_Data: This folder contains all of the scripts necessary for the
						nonparametric regression-based smoothing of the AHHA model.
						Written in language R.

6. Smoothing_SPACE: This folder contains all of the scripts necessary for the
					smoothing of SPACE. Written in R.

7. Demonstrating_SPACE_Instability: These are the scripts for demonstrating the 
									instability of Eureqa derived SPACE. The model 
									is written, run and compared to the training data.

8. Demonstrating_Relationship_Between_Complexity_Fit: This is the script for the tests
													  relating complexity to fit. It
													  includes the plots of values and
													  the PPMCC test.

NOTE: SPECIFYING RELATIVE DIRECTORIES IN R IS DIFFICULT AND SYSTEM DEPENDENT.
	  SOME CODE MAY REQUIRE YOU TO CHANGE THE CURRENT DIRECTORY BEFORE RUNNING.