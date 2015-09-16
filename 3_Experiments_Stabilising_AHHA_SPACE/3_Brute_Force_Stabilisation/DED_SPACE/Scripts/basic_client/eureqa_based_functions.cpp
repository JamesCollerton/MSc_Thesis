void print_intro()
{
    std::cout << "The basic_client attempts to connect to a Eureqa Server ";
    std::cout << "running on the local machine and perform a search. It ";
    std::cout << "is an extension of the minimal_client, with informational ";
    std::cout << "messages and proper error checking. It is meant to be ";
    std::cout << "used as a source code example." << std::endl;
    std::cout << std::endl;
}

eureqa::data_set import_data()
{
    // initialize a data set
    std::cout << std::endl;
    std::cout << "> Importing data set from a text file" << std::endl;

    eureqa::data_set data; // holds the data
    std::string error_msg; // error information during import

    // import data from a text file
    if (!data.import_ascii("../../Data/back_to_back.txt", error_msg))
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

eureqa::search_options specify_search(std::string search_string)
{
    // initialize search options
    eureqa::search_options options(search_string); // holds the search options
    std::cout << std::endl;
    std::cout << "> Setting the search options" << std::endl;
    std::cout << options.summary() << std::endl;

    return(options);
}

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

void monitor_run_search(eureqa::connection *conn, std::ofstream *myfile, 
                        std::string search_string)
{
    // monitor the search
    int iteration_counter = 0;

    std::cout << std::endl;
    std::cout << "> Monitoring the search progress" << std::endl;
    
    eureqa::search_progress progress;           // recieves the progress and new solutions
    eureqa::solution_frontier best_solutions;   // filters out the best solutions
    
    // continue searching (until user hits ctrl-c)
    while (conn->query_progress(progress) && iteration_counter < 10)
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

void disconnect_from_server(eureqa::connection *conn)
{
    conn->disconnect();
}

void run_bash()
{
    system("../plotting_results/run_plot.sh");
}

void pause_exit(int code)
{
    std::cout << std::endl;
    #ifdef WIN32
    system("pause");
    #endif
    exit(code);
}