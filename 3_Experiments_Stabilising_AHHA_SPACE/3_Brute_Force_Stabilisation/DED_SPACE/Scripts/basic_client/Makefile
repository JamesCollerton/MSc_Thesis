# Eureqa API example project
#

#------------------------------------------
# Edit the following paths for your system:
#------------------------------------------

EUREQA_PATH_FILE := eureqa_include_path.txt
EUREQA_PATH := $(shell cat ${EUREQA_PATH_FILE})

BOOST_LIBRARY_FILE := boost_library_path.txt
BOOST_LIBRARY := $(shell cat ${BOOST_LIBRARY_FILE})

BOOST_INCLUDE_FILE := boost_include_path.txt
BOOST_INCLUDE := $(shell cat ${BOOST_INCLUDE_FILE})

# EUREQA_INCLUDE_PATH ?= /Users/JamesCollerton/Documents/Programming/Git_Folders/MSc-Thesis-Code/Playing_Automating_Eureqa/eureqa_api_1_11_0/
# BOOST_INCLUDE_PATH  ?= /Users/JamesCollerton/Documents/Programming/Git_Folders/MSc-Thesis-Code/Playing_Automating_Eureqa/boost_1_58_0/
# BOOST_LIBRARY_PATH  ?= /Users/JamesCollerton/Documents/Programming/Git_Folders/MSc-Thesis-Code/Playing_Automating_Eureqa/boost_1_58_0/stage/lib/

EUREQA_INCLUDE_PATH ?= $(EUREQA_PATH)
BOOST_INCLUDE_PATH  ?= $(BOOST_INCLUDE)
BOOST_LIBRARY_PATH  ?= $(BOOST_LIBRARY)

#------------------------------------------
# Edit the following if you want to use a specific build of boost
# 
# For example: building boost 1.52 on Ubuntu 64 with gcc yields
# libraries named libboost_system-gcc46-mt-1_52.a. In this caseL
# BOOST_TAIL=-gcc46-mt-1_52
#------------------------------------------
BOOST_TAIL?=

basic_client:  basic_client.o
	g++ basic_client.o \
	$(BOOST_LIBRARY_PATH)libboost_system$(BOOST_TAIL).a \
	$(BOOST_LIBRARY_PATH)libboost_serialization$(BOOST_TAIL).a \
	$(BOOST_LIBRARY_PATH)libboost_date_time$(BOOST_TAIL).a \
	$(BOOST_LIBRARY_PATH)libboost_thread$(BOOST_TAIL).a \
	-lpthread  \
	-o basic_client

basic_client.o: basic_client.cpp
	g++ basic_client.cpp -I$(EUREQA_INCLUDE_PATH) -c -o basic_client.o
	-I$(BOOST_INCLUDE_PATH) \
	-I$(EUREQA_INCLUDE_PATH) \
	-c -o basic_client.o


clean: 
	rm -f basic_client basic_client.o
