#include <iostream>
#include <time.h>
#include <string>
#include <fstream>
#include <stdlib.h>

#include <eureqa/eureqa.h>
#include <boost/serialization/utility.hpp>

#ifdef WIN32
inline void sleep(int sec) { Sleep(sec*1000); }
#endif

// - James Collerton
// - Student Number 46114
// - Source Code for MSc Thesis

// This is the source code for the Eureqa API part of the brute force method.
// The purpose of this program is to connect to the Eureqa server and run queries
// through there. The program outputs to a results.txt file, which is then read
// in by a python script, which parses it and turns it into a set of differential
// equation models. These differential equation models are then run and the one
// with the minimal error between itself and the underlying data is chosen as
// our representative model.

// --------------------------- FUNCTION DECLARATIONS ---------------------------

void pause_exit(int code);

void print_intro();

eureqa::data_set import_data();

eureqa::search_options specify_search(std::string search_string);

void connect_to_server(eureqa::connection *conn);

void query_server(eureqa::connection *conn);

void send_data_set(eureqa::connection *conn, eureqa::data_set *data);

void send_opt(eureqa::connection *conn, eureqa::search_options *options);

void search_start(eureqa::connection *conn);

void monitor_run_search(eureqa::connection *conn, std::ofstream *myfile, 
                        std::string search_string);

void write_best_solution(std::string best_solution, std::ofstream *myfile,
                         std::string search_string);

void disconnect_from_server(eureqa::connection *conn);

void run_bash();

// ----------------------------------- MAIN ------------------------------------

// Declare our dataset, search options and connections. We then create an array
// of all of the search terms to be looped through in order to look for all of
// the seperate components of our model. Finally we loop through all of the 
// elements of the model and solve for them, writing the results to file and
// at the end calling a bash script to run the Python file to create the results.

// NOTE: We are limited in the amount of variables we can use to define functions
// due to the cost of Eureqa server (£7000!). This also limits us in the number
// of lines we can have in the data file (100).

int main(int argc, char *argv[])
{
    eureqa::data_set data;
    eureqa::search_options options;
    eureqa::connection conn;

    // Strings for the definition of functions.
    std::string func_array[] = {"dS = f(S, A1, A2, P2)",
                                 "dP1 = f(S, A1, A2, P2)", 
                                 "dP2 = f(S, A1, A2, P2)",
                                 "dA1 = f(S, A1, A2, P2)",
                                 "dA2 = f(S, A1, A2, P2)",
                                 "dR1 = f(S, A1, A2, P2)",
                                 "dR2 = f(S, A1, A2, P2)"};

    std::ofstream myfile;
    myfile.open ("../../Results/results.txt");

    print_intro();

    for(int i = 0; i < 7; ++i){

        data = import_data();

        connect_to_server(&conn);

        query_server(&conn);
        
        send_data_set(&conn, &data);

        options = specify_search(func_array[i]);

        send_opt(&conn, &options);

        search_start(&conn);

        monitor_run_search(&conn, &myfile, func_array[i]);

        disconnect_from_server(&conn);

    }
    
    myfile.close();
    run_bash();
    pause_exit(0);
    
    return 0;
}

// ------------------------------ FUNCTIONS ------------------------------------

// Prints an introduction.

void print_intro()
{
    std::cout << "Eureqa server running." << std::endl;
    std::cout << std::endl;
}

// Used for importing data from a text file.

eureqa::data_set import_data()
{
    // initialize a data set
    std::cout << std::endl;
    std::cout << "> Importing data set from a text file" << std::endl;

    eureqa::data_set data; // holds the data
    std::string error_msg; // error information during import

    // import data from a text file
    if (!data.import_ascii("../../Data/Data_for_Client/for_eureqa_thirteen.txt", error_msg))
    {
        std::cout << error_msg << std::endl;
        std::cout << "Unable to import this file" << std::endl;
        pause_exit(-1);
    }
    
    // print information about the data set
    std::cout << "Data set imported successfully" << std::endl;
    std::cout << data.summary() << std::endl;

    return(data);
}

// Sets the search options for the program.

eureqa::search_options specify_search(std::string search_string)
{
    // initialize search options
    eureqa::search_options options(search_string);              // holds the search options
    std::cout << std::endl;
    std::cout << "> Setting the search options" << std::endl;
    std::cout << options.summary() << std::endl;

    return(options);
}

// Connects to the search server, printing the appropriate error messages if
// it has not been possible.

void connect_to_server(eureqa::connection *conn)
{
    // connect to a eureqa server
    std::cout << std::endl;
    std::cout << "> Connecting to a eureqa server at 127.0.0.1" << std::endl;

    if (!conn->connect("127.0.0.1"))
    {
        std::cout << "Unable to connect to server" << std::endl; 
        std::cout << "Try running the eureqa_server binary provided with ";
        std::cout << "the Eureqa API (\"server\" sub-directory) first." << std::endl;
        pause_exit(-1);
    }
    else if (!conn->last_result()) 
    { 
        std::cout << "Connection made successfully, but ";
        std::cout << "the server sent back an error message:" << std::endl;
        std::cout << conn->last_result() << std::endl;
        pause_exit(-1); 
    }
    else
    {
        std::cout << "Connected to server successfully, and ";
        std::cout << "the server sent back a success message:" << std::endl;
        std::cout << conn->last_result() << std::endl;
    }

}

// This runs the queries on the server.

void query_server(eureqa::connection *conn)
{
    // query the server's information
    eureqa::server_info serv;
    std::cout << std::endl;
    std::cout << "> Querying the server systems information" << std::endl;
    
    if (!conn->query_server_info(serv))
    {
        std::cout << "Unable to recieve the server information" << std::endl;
        pause_exit(-1);
    }
    else
    {
        std::cout << "Recieved server information successfully:" << std::endl;
        std::cout << serv.summary() << std::endl;
    }

}

// Passes the data to the server.

void send_data_set(eureqa::connection *conn, eureqa::data_set *data)
{
    // send data set
    std::cout << std::endl;
    std::cout << "> Sending the data set to the server" << std::endl;
    
    if (!conn->send_data_set(*data))
    {
        std::cout << "Unable to transfer the data set" << std::endl;
        pause_exit(-1);
    }
    else if (!conn->last_result())
    {
        std::cout << "Data set transferred successfully, but ";
        std::cout << "the server sent back an error message:" << std::endl;
        std::cout << conn->last_result() << std::endl;
        pause_exit(-1);
    }
    else
    {
        std::cout << "Data set transferred successfully, and ";
        std::cout << "the server sent back a success message:" << std::endl;
        std::cout << conn->last_result() << std::endl;
    }
}

// Now we send the options to the server (functions we search in terms of).

void send_opt(eureqa::connection *conn, eureqa::search_options *options)
{
    // send options
    std::cout << std::endl;
    std::cout <<  "> Sending search options to the server" << std::endl;
    
    if (!conn->send_options(*options))
    {
        std::cout << "Unable to transfer the search options" << std::endl;
        pause_exit(-1);
    }
    else if (!conn->last_result())
    {
        std::cout << "Search options transferred successfully, but ";
        std::cout << "the server sent back an error message:" << std::endl;
        std::cout << conn->last_result() << std::endl;
        pause_exit(-1);
    }
    else
    {
        std::cout << "Search options transferred successfully, and ";
        std::cout << "the server sent back a success message:" << std::endl;
        std::cout << conn->last_result() << std::endl;
    }
}

// Finally we ask the server to start searching for results.

void search_start(eureqa::connection *conn)
{
    // start searching
    std::cout << std::endl;
    std::cout << "> Telling server to start searching" << std::endl;
    
    if (!conn->start_search())
    {
        std::cout << "Unable to send the start command" << std::endl;
        pause_exit(-1);
    }
    else if (!conn->last_result())
    {
        std::cout << "Start command sent successfully, but ";
        std::cout << "the server sent back an error message:" << std::endl;
        std::cout << conn->last_result() << std::endl;
        pause_exit(-1);
    }
    else
    {
        std::cout << "Start command sent successfully, and ";
        std::cout << "the server sent back a success message:" << std::endl;
        std::cout << conn->last_result() << std::endl;
    }
}

// As the server is running the search, we query it for information on the
// results to print to screen.

void monitor_run_search(eureqa::connection *conn, std::ofstream *myfile, 
                        std::string search_string)
{
    // monitor the search
    int iteration_counter = 0;

    std::cout << std::endl;
    std::cout << "> Monitoring the search progress" << std::endl;
    
    eureqa::search_progress progress;           // recieves the progress and new solutions
    eureqa::solution_frontier best_solutions;   // filters out the best solutions
    
    // continue searching (for 50 server iterations)
    while (conn->query_progress(progress) && iteration_counter < 50)
    {
        // print the progress (e.g. number of generations)
        std::cout << "> " << progress.summary() << std::endl;
        std::cout << std::endl;
        
        // the eureqa server sends a stream of new solutions in the progress
        // here we filter out and store only the best solutions
        if (best_solutions.add(progress.solution_))
        {
            std::cout << "New solution found:" << std::endl;
            std::cout << progress.solution_ << std::endl;
        }

        iteration_counter++;
        sleep(1);
    }

    write_best_solution(best_solutions.to_string(), myfile, search_string);
}

// Now we write the best solution frontier to screen and print it onto our file.
// That way the solutions can be parsed and turned into differential equation
// models by the Python file.

void write_best_solution(std::string best_solution, std::ofstream *myfile,
                         std::string search_string)
{
    std::cout << std::endl;
    std::cout << "> Best solution in given iterations:" << std::endl;

    std::cout << std::endl;
    std::cout << best_solution << std::endl;
    std::cout << std::endl;

    *myfile << search_string;
    *myfile << "\n";
    *myfile << best_solution;
    *myfile << "\nEND OF SOLUTION\n\n";
}

// Disconnects from the server.

void disconnect_from_server(eureqa::connection *conn)
{
    conn->disconnect();
}

// Calls the bash script that runs the following plot.

void run_bash()
{
    system("../combining_results/run_combine.sh");
}

// Pauses and then exits.

void pause_exit(int code)
{
    std::cout << std::endl;
    #ifdef WIN32
    system("pause");
    #endif
    exit(code);
}
