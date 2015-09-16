#!/bin/bash
# Starts server

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
cd $DIR

value=$(cat eureqa_server_path.txt)
sudo $value/eureqa_server