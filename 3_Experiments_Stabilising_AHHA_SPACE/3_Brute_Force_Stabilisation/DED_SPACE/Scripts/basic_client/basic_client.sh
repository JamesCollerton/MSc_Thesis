#!/bin/bash
# To run basic client

make clean

make

open -a Terminal.app start_server.sh

sleep 5

./basic_client
