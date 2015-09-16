using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using System;
using System.IO;
using System.Text;

// - James Collerton
// - Student Number 46114
// - Source Code for MSc Thesis

// Controls how we output the ant roles to file. Works in tandem with the 
// BatchRunner.cs script in SPACE.

public class Output : MonoBehaviour 
{
	
	Transform s;
	List<Transform> a, r, p;
	public UnityEngine.GameObject[] ants;
	StreamWriter sw;
	int c;
	float timeStep = 1f;
	public SimData History;

	int quorumThresh = 0;
	int colony_size = 0;
	
	public void SetUp()
	{	
		//Get all ant state container objects
		a = new List<Transform>(); 					// List of assessing ants.
		r = new List<Transform>();  				// List of recruiting ants.
		p = new List<Transform>(); 					// List of passive ants.
		s = GameObject.Find("S").transform; 		// This finds all of the scouting ants.
		p.Add(GameObject.Find("P0").transform); 	// Number of passive ants in original nest.
		
		// For each new nest adds them to a list.
		for(int i = 1; i <= GameObject.Find("NewNests").transform.childCount; i++)
		{
			p.Add(GameObject.Find("P" + i).transform); 	// So P_1 is passive ants at nest 1 etc.
			a.Add(GameObject.Find("A" + i).transform);  // Same but for assessing.
			r.Add(GameObject.Find("R" + i).transform); 	// Same but for recruiting.
		}
		
		// Declares the game object and the batch object.
		GameObject batchGO = GameObject.Find("BatchRunner");
		BatchRunner batch = (BatchRunner) batchGO.transform.GetComponent("BatchRunner");

		// Gets the output filename from the batch class.
		string outputFile = batch.GetNextOutputFile();
		quorumThresh = batch.quorumThreshold;
		colony_size = batch.colony_size;
		
		// Creates a new file to write to with the new name.
		try{ sw = new StreamWriter(outputFile); } 
		catch{ print("Couldn't find output file"); return; }

		// Prints to the UNITY 3D console so we can check that the parameter values
		// in the output class match what we expect to see from the batchrunner class.
		Debug.Log("QuorumThresh = " + quorumThresh);
		Debug.Log("Colony size = " + colony_size);
		
		// Write column titles
		sw.Write("T, ");
		sw.Write("N, ");
		sw.Write("Q, ");
		sw.Write("S, ");
		for(int i = 0; i < p.Count; i++){ sw.Write("P" + i + ", "); }
		for(int i = 1; i <= a.Count; i++){ sw.Write("A" + i + ", "); }
		for(int i = 1; i <= r.Count; i++){
			if(i == r.Count){ sw.Write("R" + i); }
			else{ sw.Write("R" + i + ", "); }
		}
		sw.WriteLine();
		c = 0;
		
		// Make writestatestofile be called every timeStep
		InvokeRepeating("WriteStatesToFile", 0f, timeStep);
	}
	
	void WriteStatesToFile()
	{
		// Check if setup, c is used as an indicator at the end of the 
		// previous function. s is to see if the simulation has been setup
		// in the rest of the program.
		if(s == null || c < 0) return;
		
		// Writes all of the data at the current time step to file.
		sw.Write(Mathf.Round(c * timeStep) + ", ");
		sw.Write(colony_size + ", ");
		sw.Write(quorumThresh + ", ");
		sw.Write(s.childCount + ", ");
		for(int i = 0; i < p.Count; i++){ sw.Write(p[i].childCount + ", "); }
		for(int i = 0; i < a.Count; i++){ sw.Write(a[i].childCount + ", "); }
		for(int i = 0; i < r.Count; i++){
			if(i == r.Count - 1){ sw.Write(r[i].childCount); }
			else{ sw.Write(r[i].childCount + ", "); }
		}
		sw.WriteLine();
		c++;
		
		//If there are no passive ants left in the original nest then restart the simulation
		if(p[0].childCount == 0){
			c = -1;
			sw.Close();
			((BatchRunner) GameObject.Find("BatchRunner").GetComponent("BatchRunner")).StartExperiment();
		}
	}
}