#!/bin/bash
# To run project.

# Finds the directory name, and the parent directory name.
dir=$PWD
parentdir=$(dirname $dir)

# Sets all of the necessary paths for the server files and the boost files. 
eureqa_include_path=$parentdir/Required_Libs/eureqa_api_1_11_0/
eureqa_server_path=$parentdir/Required_Libs/eureqa_server_1_12_1_mac/
boost_include_path=$parentdir/Required_Libs/boost_1_58_0/
boost_library_path=$parentdir/Required_Libs/boost_1_58_0/stage/lib/

# Removes any pre-existent path files from the directories.
rm -f Scripts/basic_client/eureqa_include_path.txt
rm -f Scripts/basic_client/eureqa_server_path.txt
rm -f Scripts/basic_client/boost_include_path.txt
rm -f Scripts/basic_client/boost_library_path.txt

# This writes the paths to the .txt files, to be read from by other parts of
# the program.
echo $eureqa_include_path >> Scripts/basic_client/eureqa_include_path.txt
echo $eureqa_server_path >> Scripts/basic_client/eureqa_server_path.txt
echo $boost_include_path >> Scripts/basic_client/boost_include_path.txt
echo $boost_library_path >> Scripts/basic_client/boost_library_path.txt

# Finally we run the first part of the program.
cd Scripts/basic_client/
./basic_client.sh