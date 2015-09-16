using UnityEngine;
using System.Collections;
using System.IO;

// - James Collerton
// - Student Number 46114
// - Source Code for MSc Thesis

// This script is used with the existing SPACE model in order to have it output
// the necessary data for the project. It works in tandem with the output.cs
// file.

public class BatchRunner : MonoBehaviour {
	
	// Sets the number of experiments in total we plan to do.
	public int firstExperiment = 1; 
	public int lastExperiment = 250;

	// Sets the scene we plan to do (can be changed within the SPACE interface).
	public string sceneName = "Test 2";

	// Initial conditions for the quorum threshold and colony size.
	public int quorumThreshold = 0;
	public int colony_size = 150;

	// Number of times we want to repeat for a quorum and the max number of
	// colony repeats. So we look at five quorum thresholds five times.
	public int quorum_repeats = 5;
	public int colony_size_repeats = 25;

	// List of quorums and colony sizes initialised. Will be accessed from the
	// output file
	public int[] quorums;
	public int[] colony_sizes;

	// The start index of quorum and colony size arrays.
	public int quorum_index = 0;
	public int colony_size_index = 0;

	// When the program gets started this is also started. We have an array of
	// five quorums and five colony sizes that are looped through in order to
	// create the data for the project. We want to have five sets of data for
	// each colony size, hence the colony_size_repeats.
	void Start () 
	{
		quorums = new int[5] {0, 5, 8, 10, 12};
		colony_sizes = new int[5] {150, 175, 200, 225, 250};
		DontDestroyOnLoad(gameObject);
		Application.LoadLevel(sceneName);

		colony_size_repeats = 5 * quorum_repeats;

		// Writes to screen in UNITY so we can check we are at the experiment
		// we expect to be at.
		Debug.Log("Quorum Threshold = " + quorumThreshold);
		Debug.Log("Colony size = " + colony_size);
		Debug.Log("Quorum index = " + quorum_index);
		Debug.Log("Colony size index = " + colony_size_index);
	}

	// Checks if folder already exists in directory and creates if not.
	public void create_folder(string path)
	{
	  bool folderExists = Directory.Exists("Results/" + path);
	  if (!folderExists){ Directory.CreateDirectory("Results/" + path); }
	}
	
	// The output files are in the Results/ folder and come under the
	// number trial they are. Used by the output.cs script to write the output
	// to the correct file.
	public string GetNextOutputFile()
	{
		create_folder("CS_" + colony_size + "_Q_" + quorumThreshold);
		return("Results/CS_" + colony_size + "_Q_" + quorumThreshold + "/" +
			   firstExperiment + ".csv");	
	}
	
	// One is added to the experiment number every time it's run to generate
	// unique names for the files. Then if we have repeated the experiment 
	// the specified number of times, we move up one in the quorum and colony
	// size vectors.
	public void StartExperiment()
	{
		firstExperiment++;

		quorum_index = CycleExperiment(quorum_index, quorum_repeats);
		colony_size_index = CycleExperiment(colony_size_index, colony_size_repeats);

		quorumThreshold = quorums[quorum_index];
		colony_size = colony_sizes[colony_size_index];

		Application.LoadLevel(sceneName);
	}

	// Used to cycle through each of the indices for the quorum and populations
	// in order to do all possible combinations. 4 is obviously as we repeat
	// each experiment five times.
	public int CycleExperiment(int spec_index, int repeats)
	{
		if( (firstExperiment - 1) % repeats == 0){
			if(spec_index < 4){ spec_index++; }
			else{ spec_index = 0; }
		}

		return(spec_index);
	}
	
	// Gets the experiment number for the output.cs file.
	public int GetExperimentID()
	{
		return firstExperiment;
	}

}