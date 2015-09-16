// - James Collerton
// - Student Number 46114
// - Source Code for MSc Thesis

// This is the modified AH-HA model used to generate the necessary data for the
// project. It has been restructured to create data of the right form, and new
// functionality is as indicated.

// Building the model is documented in the supporting documents, and the 
// instructions for doing so can be read there. All the necessary libraries are
// included in the required libs folder.

/*********************************************************************************************
*                      (c) 2004-2006 University of Bristol, UK
* 
* Grant: SISEBIA (Social Insects, Simulated Evolution and Biologically Inspired Algorithms)
* Project: AH-HA
* $Author: marshall $
* $Date: 2005/08/05 14:39:43 $
* $Revision: 1.9 $ 
*
*********************************************************************************************/

package nests;

import ahha.*;
import java.util.Iterator;
import java.lang.Integer;
import java.io.FileWriter;
import java.io.IOException;
import uchicago.src.sim.engine.ActionGroup;
import uchicago.src.sim.engine.BasicAction;
import uchicago.src.sim.engine.SimInit;
import uchicago.src.sim.engine.SimModelImpl;
import uchicago.src.sim.engine.Schedule;
import uchicago.src.sim.analysis.OpenSequenceGraph;
import uchicago.src.sim.util.Random;
import cern.jet.random.engine.RandomSeedTable;

// -------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -------------------------------------------------------------------------

// NEW IMPORTS

import java.io.*;
import java.nio.*;
import java.io.File;

// -------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -------------------------------------------------------------------------
// -------------------------------------------------------------------------

/**
 * The main class for the AH-HA nest selection model
 * 
 */
public class Nests extends SimModelImpl
{
	/** The model's ant colony */
	private AntColony mAntColony;
	/** The colony's size */
	private int mColonySize;
	/** The number of scouts in the colony */
	private int mNumScouts;
	/** The colony's normal quorum threshold */
	private int mNormalQuorumThreshold;
	/** The colony's hostile environment quorum threshold */
	private int mHostileQuorumThreshold;
	/** The colony's emergency quorum threshold */
	private int mEmergencyQuorumThreshold;
	/** The colony scouts' preference switching probability */
	private double mPreferenceSwitchProb;
	/** The colony scouts' probability to start scouting */
	private double mStartScoutingProb;
	/** The colony scouts' probability to stop scouting */
	private double mStopScoutingProb;
	/** The colony scouts' probability to lead reverse tandem-runs */
	private double mReverseTandemRunProb;
	/** The colony scouts' probability to change their current nest */
	private double mChangeNestProb;
	/** The maximum nest quality (for the best possible nest site) */
	private int mMaxNestQuality;
	/** The time the colony's scouts take to assess a site */
	private int mAssessmentDelay;
	/** The colony scouts' tandem-running speed */
	private int mTandemRunSpeed;
	/** The colony scouts' carrying speed */
	private int mCarryingSpeed;
	/** The colony's nest quality requirement */
	private int mNestQualityRequirement;
	/** Is the colony's environment hostile? */
	private boolean mEnvironmentHostile;
	/** Is distance significant for the colony? */
	private boolean mDistanceSignificant;
	/** Do ants compare nest sites against each other for quality? */
	private boolean mCompareNestSiteQualities;
	/** Is the model configured to be equivalent to the Pratt et al EBM? */
	private boolean mPrattEbmEquivalent;
	/** Is the model running in batch mode? */
	private boolean mBatch;
	/** The model's nests */
	private Nest[] mNests;
	/** The distance between nests 1 and 2 */
	private int mNest1ToNest2Distance;
	/** The distance between nests 1 and 3 */
	private int mNest1ToNest3Distance;
	/** The distance between nests 2 and 3 */
	private int mNest2ToNest3Distance;
	/** Is nest 1 habitable? */
	private boolean mNest1Habitable;
	/** The quality of nest 1 */
	private int mNest1Quality;
	/** The quality of nest 2 */
	private int mNest2Quality;
	/** The quality of nest 3 */
	private int mNest3Quality;
	/** The standard deviation of the noise in measurements of nest quality */
	private int mNestQualityStdDev;
	/** The time taken for the colony to vacate its original nest */
	private int mTimeToVacation;
	/** The quorum size in nest 1 on the last timestep */
	private int mLastTickNest1QuorumSize;
	/** The quorum size in nest 2 on the last timestep */
	private int mLastTickNest2QuorumSize;
	/** The quorum size in nest 3 on the last timestep */
	private int mLastTickNest3QuorumSize;
	/** The number of timesteps since a change in the quorum levels in the different nests */
	private int mNumTicksSinceQuorumSizeChange;
	/** The number of recruitment acts during the current emigration */
	private int mNumRecruitmentActs;
	/** The model's schedule */
	private Schedule mSchedule;
	/** The model's quorum graph  */
	private OpenSequenceGraph mQuorumGraph;
	/** The model's recruitment graph  */
	private OpenSequenceGraph mRecruitmentGraph;
	/** The model's log file */
	private FileWriter mLogFile;
	/** The model's random seed */
	private static long mRandomSeed = 0; // TODO: random number seed sequence doesn't work for batch mode
	/** The model's next random seed */
	private static long mNextRandomSeed = 0;
	/** The model (nasty hack to allow model to stop itself during update action) */
	private Nests mThis;

	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------

	// NEW VARIABLES

	private PrintWriter writer;

	private int file_number;

	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------

	/**
	 * Main method for AH-HA nest selection model
	 * 
	 * @param args
	 * 
	 * @return void
	 * 
	 */
	public static void main(String[] args)
	{
		Nests nests;
		SimInit init;
		String inputFile = null, outputFile = null;
		boolean batch;

		if (args.length == 0)
		{
			System.out.println("Usage: Nests batchmode {true, false} <inputfile <outputfile>>");
			
			return;
		}
		if (args[0].equals("true"))
		{
			batch = true;
		}
		else
		{
			batch = false;
		}
		if (args.length >= 2)
		{
			inputFile = args[1];
		}
		if (args.length >= 3)
		{
			outputFile = args[2];
		}
		init = new SimInit();
		nests = new Nests(outputFile, batch);
		nests.setRngSeed(RandomSeedTable.getSeedAtRowColumn(0, 0));
		init.loadModel(nests, inputFile, batch);
	}

	/**
	 * Initialises the random distribution used by the model with the given random seed
	 * 
	 * @param seed
	 * 
	 * @return void
	 * 
	 */
	public static void initialiseDistribution(long seed)
	{
		Random.setSeed(seed);
		Random.createUniform();
		Random.createNormal(0, 5);
	}
	
	/**
	 * Gets the name of the model
	 * 
	 * @return Model's name
	 * 
	 */
	public String getName()
	{
		return new String("Nests");
	}
	
	/**
	 * Gets the model's parameters
	 * 
	 * @return model's parameters
	 * 
	 */
	public String[] getInitParam()
	{
		return new String[] {"ColonySize", "NumScouts", "NormalQuorumThreshold", "HostileQuorumThreshold", "EmergencyQuorumThreshold", "PreferenceSwitchProb", "StartScoutingProb", "StopScoutingProb", "ReverseTandemRunProb", "ChangeNestProb", "MaxNestQuality", "AssessmentDelay", "TandemRunSpeed", "CarryingSpeed", "NestQualityRequirement", "EnvironmentHostile", "DistanceSignificant", "CompareNestSiteQualities", "PrattEbmEquivalent", "Nest1Habitable", "Nest1Quality", "Nest2Quality", "Nest3Quality", "NestQualityStdDev", "Nest1ToNest2Distance", "Nest1ToNest3Distance", "Nest2ToNest3Distance"};
	}

	/**
	 * Gets the model's schedule
	 * 
	 * @return model's schedule
	 * 
	 */
	public Schedule getSchedule()
	{
		return mSchedule;
	}
	
	// AH-HA getters and setters follow...
	
	/**
	 * Gets the colony scouts' carrying speed
	 * 
	 * @return colony scouts' carrying speed
	 * 
	 */
	public int getCarryingSpeed() {
		return mCarryingSpeed;
	}

	/**
	 * Gets the colony scouts' probability to change their current nest
	 * 
	 * @return colony scouts' probability to change their current nest
	 * 
	 */
	public double getChangeNestProb()
	{
		return mChangeNestProb;
	}

	/**
	 * Gets the colony's size
	 * 
	 * @return colony's size
	 * 
	 */
	public int getColonySize()
	{
		return mColonySize;
	}

	/**
	 * Do ants in the colony compare nest sites with each other for quality?
	 * 
	 * @return ants in the colony compare nest sites with each other for quality (true / false)
	 * 
	 */
	public boolean getCompareNestSiteQualities()
	{
		return mCompareNestSiteQualities;
	}

	/**
	 * Is the model configured to be equivalent to the Pratt et al EBM?
	 * 
	 * @return true if the model is configured to be equivalent to the Pratt et al EBM, false otherwise
	 * 
	 */
	public boolean getPrattEbmEquivalent()
	{
		return mPrattEbmEquivalent;
	}

	/**
	 * Is distance significant for the colony?
	 * 
	 * @return distance significant for the colony (true / false)
	 * 
	 */
	public boolean getDistanceSignificant()
	{
		return mDistanceSignificant;
	}

	/**
	 * Gets the colony's emergency quorum threshold
	 * 
	 * @return colony's emergency quorum threshold
	 * 
	 */
	public int getEmergencyQuorumThreshold()
	{
		return mEmergencyQuorumThreshold;
	}

	/**
	 * Is the colony's environment hostile?
	 * 
	 * @return colony's environment hostile (true / false)
	 * 
	 */
	public boolean getEnvironmentHostile()
	{
		return mEnvironmentHostile;
	}

	/**
	 * Gets the colony's hostile environment quorum threshold
	 * 
	 * @return colony's hostile environment quorum threshold
	 * 
	 */
	public int getHostileQuorumThreshold()
	{
		return mHostileQuorumThreshold;
	}

	/**
	 * Gets the maximum nest quality (for the best possible nest site)
	 * 
	 * @return maximum nest quality
	 * 
	 */
	public int getMaxNestQuality()
	{
		return mMaxNestQuality;
	}

	/**
	 * Gets the time the colony's scouts take to assess a nest site
	 * 
	 * @return time the colony's scouts take to assess a nest site
	 * 
	 */
	public int getAssessmentDelay()
	{
		return mAssessmentDelay;
	}

	/**
	 * Gets the colony's nest quality requirement
	 * 
	 * @return colony's nest quality requirement
	 * 
	 */
	public int getNestQualityRequirement()
	{
		return mNestQualityRequirement;
	}

	/**
	 * Gets the colony's normal quorum threshold
	 * 
	 * @return colony's normal quorum threshold
	 * 
	 */
	public int getNormalQuorumThreshold()
	{
		return mNormalQuorumThreshold;
	}

	/**
	 * Gets the number of scouts in the colony
	 * 
	 * @return number of scouts in the colony
	 * 
	 */
	public int getNumScouts()
	{
		return mNumScouts;
	}

	/**
	 * Gets the colony scouts' preference switch probability
	 * 
	 * @return colony scouts' preference switch probability
	 * 
	 */
	public double getPreferenceSwitchProb()
	{
		return mPreferenceSwitchProb;
	}

	/**
	 * Gets the colony scouts' probability of leading a reverse tandem-run
	 * 
	 * @return colony scouts' probability of leading a reverse tandem-run
	 * 
	 */
	public double getReverseTandemRunProb()
	{
		return mReverseTandemRunProb;
	}

	/**
	 * Gets the colony scouts' probability of starting scouting
	 * 
	 * @return colony scouts' probability of starting scouting
	 * 
	 */
	public double getStartScoutingProb()
	{
		return mStartScoutingProb;
	}

	/**
	 * Gets the colony scouts' probability of stoppinh scouting
	 * 
	 * @return colony scouts' probability of stopping scouting
	 * 
	 */
	public double getStopScoutingProb()
	{
		return mStopScoutingProb;
	}

	/**
	 * Gets the colony scouts' reverse tandem-running speed
	 * 
	 * @return colony scouts' reverse tandem-running speed
	 * 
	 */
	public int getTandemRunSpeed()
	{
		return mTandemRunSpeed;
	}

	/**
	 * Sets the colony scouts' carrying speed
	 * 
	 * @param carryingSpeed
	 * 
	 * @return void
	 * 
	 */
	public void setCarryingSpeed(int carryingSpeed)
	{
		mCarryingSpeed = carryingSpeed;
	}

	/**
	 * Sets the colony scouts' probability of changing their current nest after switching preference (i.e. changing the origin of their recruitment efforts)
	 *
	 * @param changeNestProb
	 * 
	 * @return void
	 * 
	 */
	public void setChangeNestProb(double changeNestProb)
	{
		mChangeNestProb = changeNestProb;
	}

	/**
	 * Sets the colony's size
	 * 
	 * @param colonySize
	 * 
	 * @return void
	 * 
	 */
	public void setColonySize(int colonySize)
	{
		mColonySize = colonySize;
	}

	/**
	 * Sets whether the colony scouts directly compare nest site qualities
	 * 
	 * @param compareNestSiteQualities
	 * 
	 * @return void
	 * 
	 */
	public void setCompareNestSiteQualities(boolean compareNestSiteQualities)
	{
		mCompareNestSiteQualities = compareNestSiteQualities;
	}

	/**
	 * Configures the model to be equivalent or not to the Pratt et al EBM
	 * 
	 * @param prattEbmEquivalent
	 * 
	 * @return void
	 * 
	 */
	public void setPrattEbmEquivalent(boolean prattEbmEquivalent)
	{
		mPrattEbmEquivalent = prattEbmEquivalent;
	}

	/**
	 * Sets whether distance is significant in the colony scouts' discovery and recruitment rates
	 * 
	 * @param distanceSignificant
	 * 
	 * @return void
	 * 
	 */
	public void setDistanceSignificant(boolean distanceSignificant)
	{
		mDistanceSignificant = distanceSignificant;
	}

	/**
	 * Sets the colony's emergency quorum threshold
	 * 
	 * @param colony's emergency quorum threshold
	 * 
	 * @return void
	 * 
	 */
	public void setEmergencyQuorumThreshold(int emergencyQuorumThreshold)
	{
		mEmergencyQuorumThreshold = emergencyQuorumThreshold;
	}

	/**
	 * Sets whether the colony's environment is hostile
	 * 
	 * @param environmentHostile
	 * 
	 * @return void
	 * 
	 */
	public void setEnvironmentHostile(boolean environmentHostile)
	{
		mEnvironmentHostile = environmentHostile;
	}

	/**
	 * Sets the colony scouts' hostile quorum threshold
	 * 
	 * @param hostileQuorumThreshold
	 * 
	 * @return void
	 * 
	 */
	public void setHostileQuorumThreshold(int hostileQuorumThreshold)
	{
		mHostileQuorumThreshold = hostileQuorumThreshold;
	}

	/**
	 * Sets the maximum nest quality (for the best possible nest site)
	 * 
	 * @param maxNestQuality
	 * 
	 * @return void
	 * 
	 */
	public void setMaxNestQuality(int maxNestQuality)
	{
		mMaxNestQuality = maxNestQuality;
	}

	/**
	 * Sets the colony scouts' assessment delay
	 * 
	 * @param assessmentDelay
	 * 
	 * @return void
	 * 
	 */
	public void setAssessmentDelay(int assessmentDelay)
	{
		mAssessmentDelay = assessmentDelay;
	}

	/**
	 * Sets the colony's nest quality requirement
	 * 
	 * @param nestQualityRequirement
	 * 
	 * @return void
	 * 
	 */
	public void setNestQualityRequirement(int nestQualityRequirement)
	{
		mNestQualityRequirement = nestQualityRequirement;
	}

	/**
	 * Sets the colony scouts' normal quorum threshold
	 * 
	 * @param normalQuorumThreshold
	 * 
	 * @return void
	 * 
	 */
	public void setNormalQuorumThreshold(int normalQuorumThreshold)
	{
		mNormalQuorumThreshold = normalQuorumThreshold;
	}

	/**
	 * Set the colony's number of scouts
	 * 
	 * @param numScouts
	 * 
	 * @return void
	 * 
	 */
	public void setNumScouts(int numScouts)
	{
		mNumScouts = numScouts;
	}

	/**
	 * Sets the colony scouts' preference switch probability
	 * 
	 * @param prefSwitchProb
	 * 
	 * @return void
	 * 
	 */
	public void setPreferenceSwitchProb(double prefSwitchProb)
	{
		mPreferenceSwitchProb = prefSwitchProb;
	}

	/**
	 * Sets the colony scouts' reverse tandem run probability
	 * 
	 * @param reverseTandemRunProb
	 * 
	 * @return void
	 * 
	 */
	public void setReverseTandemRunProb(double reverseTandemRunProb)
	{
		mReverseTandemRunProb = reverseTandemRunProb;
	}

	/**
	 * Set the colony scouts' start scouting probability
	 * 
	 * @param startScoutingProb
	 * 
	 * @return void
	 * 
	 */
	public void setStartScoutingProb(double startScoutingProb)
	{
		mStartScoutingProb = startScoutingProb;
	}

	/**
	 * Sets the colony scouts' stop scouting probability
	 * 
	 * @param stopScoutingProb
	 * 
	 * @return void
	 * 
	 */
	public void setStopScoutingProb(double stopScoutingProb)
	{
		mStopScoutingProb = stopScoutingProb;
	}

	/**
	 * Set the colony scouts' tandem run speed
	 * 
	 * @param tandemRunSpeed
	 * 
	 * @return void
	 * 
	 */
	public void setTandemRunSpeed(int tandemRunSpeed)
	{
		mTandemRunSpeed = tandemRunSpeed;
	}

	// Nests getters and setters follow...
	
	/**
	 * Gets the habitability of nest 1
	 * 
	 * @return is nest 1 habitable? (true / false)
	 * 
	 */
	public boolean getNest1Habitable()
	{
		return mNest1Habitable;
	}

	/**
	 * Gets nest 1's quality
	 * 
	 * @return nest 1's quality
	 * 
	 */
	public int getNest1Quality()
	{
		return mNest1Quality;
	}

	/**
	 * Gets the distance between nests 1 and 2
	 * 
	 * @return distance between nests 1 and 2
	 * 
	 */
	public int getNest1ToNest2Distance()
	{
		return mNest1ToNest2Distance;
	}

	/**
	 * Gets the distance between nests 1 and 3
	 * 
	 * @return distance between nests 1 and 3
	 * 
	 */
	public int getNest1ToNest3Distance()
	{
		return mNest1ToNest3Distance;
	}

	/**
	 * Gets nest 2's quality
	 * 
	 * @return nest 2's quality
	 * 
	 */
	public int getNest2Quality()
	{
		return mNest2Quality;
	}

	/**
	 * Gets the distance between nests 2 and 3
	 * 
	 * @return distance between nests 2 and 3
	 * 
	 */
	public int getNest2ToNest3Distance()
	{
		return mNest2ToNest3Distance;
	}

	/**
	 * Gets nest 3's quality
	 * 
	 * @return nest 3's quality
	 * 
	 */
	public int getNest3Quality()
	{
		return mNest3Quality;
	}

	/**
	 * Gets the standard deviation in measurements of nest quality
	 * 
	 * @return standard deviation in measurements of nest quality
	 * 
	 */
	public int getNestQualityStdDev()
	{
		return mNestQualityStdDev;
	}

	/**
	 * Sets the habitability of nest 1
	 * 
	 * @param habitable
	 * 
	 * @return void
	 * 
	 */
	public void setNest1Habitable(boolean habitable)
	{
		mNest1Habitable = habitable;
	}

	/**
	 * Sets the quality of nest 1
	 * 
	 * @param quality
	 * 
	 * @return void
	 * 
	 */
	public void setNest1Quality(int quality)
	{
		mNest1Quality = quality;
	}

	/**
	 * Sets the distance from nest 1 to nest 2
	 * 
	 * @param distance
	 * 
	 * @return void
	 * 
	 */
	public void setNest1ToNest2Distance(int distance)
	{
		mNest1ToNest2Distance = distance;
	}

	/**
	 * Sets the distance from nest 1 to nest 3
	 * 
	 * @param distance
	 * 
	 * @return void
	 * 
	 */
	public void setNest1ToNest3Distance(int distance)
	{
		mNest1ToNest3Distance = distance;
	}

	/**
	 * Sets the quality of nest 2
	 * 
	 * @param quality
	 * 
	 * @return void
	 * 
	 */
	public void setNest2Quality(int quality)
	{
		mNest2Quality = quality;
	}

	/**
	 * Sets the distance from nest 2 to nest 3
	 * 
	 * @param distance
	 * 
	 * @return void
	 * 
	 */
	public void setNest2ToNest3Distance(int distance)
	{
		mNest2ToNest3Distance = distance;
	}

	/**
	 * Sets the quality of nest 3
	 * 
	 * @param quality
	 * 
	 * @return void
	 * 
	 */
	public void setNest3Quality(int quality)
	{
		mNest3Quality = quality;
	}

	/**
	 * Sets the standard deviation of noise in nest assessments
	 * 
	 * @param stdDev
	 * 
	 * @return void
	 * 
	 */
	public void setNestQualityStdDev(int stdDev)
	{
		mNestQualityStdDev = stdDev;
	}

	/**
	 * Sets up the model ready to run
	 * 
	 * @return void
	 * 
	 */
	public void setup()
	{
		if (mRandomSeed == 0)
		{
			mRandomSeed = Random.getSeed();
		}
		else
		{
			mRandomSeed = mNextRandomSeed;
		}
		initialiseDistribution(mRandomSeed);
		mNextRandomSeed = RandomSeedTable.getSeedAtRowColumn(Random.uniform.nextIntFromTo(0, Integer.MAX_VALUE), Random.uniform.nextIntFromTo(0, Integer.MAX_VALUE));
		mSchedule = new Schedule();
		if (mQuorumGraph != null)
		{
			mQuorumGraph.dispose();
		}
		if (mRecruitmentGraph != null)
		{
			mRecruitmentGraph.dispose();
		}
		if (!mBatch)
		{
			mQuorumGraph = new OpenSequenceGraph("Quorum Size in Nests", this);
			this.registerMediaProducer("Quorum Graph", mQuorumGraph);
			mRecruitmentGraph = new OpenSequenceGraph("Recruitment Between Nests", this);
			this.registerMediaProducer("Recruitment Graph", mRecruitmentGraph);
		}
		mTimeToVacation = 0;
	}
	
	/**
	 * Gets the number of recruiters from site 1 to site 2
	 * 
	 * @return number of recruiters from site 1 to site 2
	 * 
	 */
	public int getNumRecruitersBetweenSites1and2()
	{
		return mAntColony.getNumRecruitersBetweenSites(mNests[1], mNests[2]);
	}

	/**
	 * Gets the number of recruiters from site 1 to site 3
	 * 
	 * @return number of recruiters from site 1 to site 3
	 * 
	 */
	public int getNumRecruitersBetweenSites1and3()
	{
		return mAntColony.getNumRecruitersBetweenSites(mNests[1], mNests[3]);
	}

	/**
	 * Gets the number of recruiters from site 2 to site 1
	 * 
	 * @return number of recruiters from site 2 to site 1
	 * 
	 */
	public int getNumRecruitersBetweenSites2and1()
	{
		return mAntColony.getNumRecruitersBetweenSites(mNests[2], mNests[1]);
	}

	/**
	 * Gets the number of recruiters from site 2 to site 3
	 * 
	 * @return number of recruiters from site 2 to site 3
	 * 
	 */
	public int getNumRecruitersBetweenSites2and3()
	{
		return mAntColony.getNumRecruitersBetweenSites(mNests[2], mNests[3]);
	}

	/**
	 * Gets the number of recruiters from site 3 to site 1
	 * 
	 * @return number of recruiters from site 3 to site 1
	 * 
	 */
	public int getNumRecruitersBetweenSites3and1()
	{
		return mAntColony.getNumRecruitersBetweenSites(mNests[3], mNests[1]);
	}

	/**
	 * Gets the number of recruiters from site 3 to site 2
	 * 
	 * @return number of recruiters from site 3 to site 2
	 * 
	 */
	public int getNumRecruitersBetweenSites3and2()
	{
		return mAntColony.getNumRecruitersBetweenSites(mNests[3], mNests[2]);
	}

	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------

	// NEW FUNCTIONS

	// Number of scouts in the individual sites. We classify this as all
	// of the active ants - ants in different roles.
	public int getNewNumScouts()
	{
		int num_scouts = getNumScouts() - getNumRecruitersBetweenSites1and2() -
										  getNumRecruitersBetweenSites1and3() -
										  getNumAssessingSite( mNests[2] ) -
										  getNumAssessingSite( mNests[3] );
		
		return(num_scouts);
	}

	// Number of passive ants in the individual sites.
	public int getNumPassiveSite(Nest nest_site)
	{
		int num_passive_ants = nest_site.getQuorumSize(mAntColony, false);
		
		return(num_passive_ants);
	}

	// The quorum threshold used in the emigration.
	public int getQuorumThreshold()
	{
		return( mAntColony.getQuorumThreshold() );
	}

	// Number of assessing ants in the individual sites.
	public int getNumAssessingSite(Nest nest_site)
	{
		int num_assessors = mAntColony.getNumAssessingSite(nest_site);

		return(num_assessors);
	}

	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------

	/**
	 * Gets the total number of ants in or commited to all nests (should equal the colony size)
	 * 
	 * @return total quorum size
	 * 
	 */
	public int getTotalQuorumSize()
	{
		return mNests[1].getQuorumSize() + mNests[2].getQuorumSize() + mNests[3].getQuorumSize();
	}

	/**
	 * Begins the model's execution
	 * 
	 * @return void
	 * 
	 */
	public void begin()
	{
		ActionGroup updateActionGroup;
		BasicAction updateAction;
		OpenSequenceGraph graph;
		Iterator i1;
		int l1;

		// -------------------------------------------------------------------------
		// -------------------------------------------------------------------------
		// -------------------------------------------------------------------------
		// -------------------------------------------------------------------------

		// NEW FUNCTIONS

		// Create a file with the current experiment number and then increase the
		// file number so that in the following trial we write a new file.
		create_file(file_number);
		++file_number;

		// -------------------------------------------------------------------------
		// -------------------------------------------------------------------------
		// -------------------------------------------------------------------------
		// -------------------------------------------------------------------------
		
		mNests = new Nest[4];
		mNests[1] = new Nest(mNest1Quality, mNestQualityStdDev);
		mNests[1].setHabitable(mNest1Habitable);
		mNests[2] = new Nest(mNest2Quality, mNestQualityStdDev);
		mNests[3] = new Nest(mNest3Quality, mNestQualityStdDev);
		mNests[1].addNeighbouringNestSite(mNests[2], mNest1ToNest2Distance);
		mNests[1].addNeighbouringNestSite(mNests[3], mNest1ToNest3Distance);
		mNests[2].addNeighbouringNestSite(mNests[1], mNest1ToNest2Distance);
		mNests[2].addNeighbouringNestSite(mNests[3], mNest2ToNest3Distance);
		mNests[3].addNeighbouringNestSite(mNests[1], mNest1ToNest3Distance);
		mNests[3].addNeighbouringNestSite(mNests[2], mNest2ToNest3Distance);
		mAntColony = new AntColony(mColonySize, mNumScouts, mNormalQuorumThreshold, mEmergencyQuorumThreshold, mHostileQuorumThreshold, mPreferenceSwitchProb, mStartScoutingProb, mStopScoutingProb, mReverseTandemRunProb, mChangeNestProb, mMaxNestQuality, mAssessmentDelay, mTandemRunSpeed, mCarryingSpeed, mNests[1], mNestQualityRequirement, mEnvironmentHostile, mDistanceSignificant, mCompareNestSiteQualities, mPrattEbmEquivalent);
		if (!mBatch)
		{
			for (l1 = 1; l1 <= 3; l1++)
			{
				mQuorumGraph.createSequence("Nest " + l1, mNests[l1], "getQuorumSize");
			}
			mQuorumGraph.createSequence("All Nests", this, "getTotalQuorumSize");
			mRecruitmentGraph.createSequence("1 to 2", this, "getNumRecruitersBetweenSites1and2");
			mRecruitmentGraph.createSequence("1 to 3", this, "getNumRecruitersBetweenSites1and3");
			mRecruitmentGraph.createSequence("2 to 1", this, "getNumRecruitersBetweenSites2and1");
			mRecruitmentGraph.createSequence("2 to 3", this, "getNumRecruitersBetweenSites2and3");
			mRecruitmentGraph.createSequence("3 to 1", this, "getNumRecruitersBetweenSites3and1");
			mRecruitmentGraph.createSequence("3 to 2", this, "getNumRecruitersBetweenSites3and2");
			mQuorumGraph.setAxisTitles("Time", "");
			mRecruitmentGraph.setAxisTitles("Time", "");
		}
		updateAction = new BasicAction()
		{
			public void execute()
			{
				OpenSequenceGraph graph;
				Iterator i1;
				String logString;
				int l1;
				
//				Call the function which outputs the data we need for the project.
				print_info();
				
				if (Random.getSeed() != mRandomSeed)
				{
					mRandomSeed = Random.getSeed();
					initialiseDistribution(mRandomSeed);
					mNextRandomSeed = RandomSeedTable.getSeedAtRowColumn(Random.uniform.nextIntFromTo(0, Integer.MAX_VALUE), Random.uniform.nextIntFromTo(0, Integer.MAX_VALUE));
				}
				mAntColony.update();
				if (!mBatch)
				{
					mQuorumGraph.step();
					mRecruitmentGraph.step();
				}
				if (mTimeToVacation == 0 && mNests[1].getQuorumSize() == 0)
				{
					mTimeToVacation = (int) mThis.getTickCount();
				}
				if (Math.abs(mNests[1].getQuorumSize() - mLastTickNest1QuorumSize) <= 2 && Math.abs(mNests[2].getQuorumSize() - mLastTickNest2QuorumSize) <= 2 && Math.abs(mNests[3].getQuorumSize() - mLastTickNest3QuorumSize) <= 2) // TODO: remove magic numbers
				{
//					mNumTicksSinceQuorumSizeChange++;
				}
				else
				{
					mLastTickNest1QuorumSize = mNests[1].getQuorumSize();
					mLastTickNest2QuorumSize = mNests[2].getQuorumSize();
					mLastTickNest3QuorumSize = mNests[3].getQuorumSize();
					mNumTicksSinceQuorumSizeChange = 0;
					mNumRecruitmentActs = mAntColony.getNumRecruitmentActs();
				}
//				if (mAntColony.getQuorumThreshold() == 0 || mNumTicksSinceQuorumSizeChange >= 100) // TODO: remove magic number
				// halt simulation if emigration has been completed, stalled, or not been completed in the time it would take one scout to tandem-run the entire colony to the most distant nest site
				if ((mNests[1].getQuorumSize() == 0 && getTotalQuorumSize() == mAntColony.getColonySize()) || mThis.getTickCount() >= (((double) Math.max(mNest1ToNest2Distance, mNest1ToNest3Distance) / mTandemRunSpeed) * mColonySize))
				{
					boolean colonySplit, finalDecisionOptimal;
					if (mAntColony.getQuorumThreshold() == 0)
					{
						mNumTicksSinceQuorumSizeChange = 0;
						mNumRecruitmentActs = mAntColony.getNumRecruitmentActs();
						colonySplit = false;
						if (mNests[3].getQuorumSize() == mColonySize)
						{
							finalDecisionOptimal = true;
						}
						else
						{
							finalDecisionOptimal = false;
						}
					}
					else
					{
						if (!(mNests[1].getQuorumSize(mAntColony, false) == 0 && mNests[2].getQuorumSize(mAntColony, false) == 0))
						{
							mNumTicksSinceQuorumSizeChange = 0;
							mNumRecruitmentActs = mAntColony.getNumRecruitmentActs();
						}
						colonySplit = true;
						if (mNests[3].getQuorumSize() > mNests[2].getQuorumSize())
						{
							finalDecisionOptimal = true;
						}
						else
						{
							finalDecisionOptimal = false;
						}
					}
					logString = new String(mColonySize + "," + mNumScouts + ","
						+ mNormalQuorumThreshold + "," + mHostileQuorumThreshold + ","
						+ mEmergencyQuorumThreshold + "," + mPreferenceSwitchProb + ","
						+ mStartScoutingProb + "," + mStopScoutingProb + "," + mReverseTandemRunProb + ","
						+ mChangeNestProb + "," + mMaxNestQuality + "," + mAssessmentDelay + ","
						+ mTandemRunSpeed + "," + mCarryingSpeed + ","
						+ mNestQualityRequirement + "," + mPrattEbmEquivalent + "," + mEnvironmentHostile + ","
						+ mDistanceSignificant + "," + mCompareNestSiteQualities + ","
						+ mNest1ToNest2Distance + "," + mNest1ToNest3Distance) + ","
						+ mNest2ToNest3Distance + "," + mNest1Habitable + ","
						+ mNest1Quality + "," + mNest2Quality + "," + mNest3Quality + ","
						+ mNestQualityStdDev + "," + colonySplit + "," + finalDecisionOptimal + ","
						+ mTimeToVacation + "," + (mThis.getController().getCurrentTime() - mNumTicksSinceQuorumSizeChange) + ","
						+ mNumRecruitmentActs + "," + mNests[1].getQuorumSize() + ","
						+ mNests[2].getQuorumSize() + "," + mNests[3].getQuorumSize(); 
					if (mLogFile == null)
					{
						System.out.println(logString);
					}
					else
					{
						try
						{
							mLogFile.write(logString + "\n");
							mLogFile.flush();
						}
						catch (IOException exception)
						{
							throw new RuntimeException("Nests.execute: error writing to log file");
						}
					}
					mThis.getController().stopSim();

					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------

					// NEW FUNCTIONS

					writer.close();

					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
				}
			}
			
			// -------------------------------------------------------------------------
			// -------------------------------------------------------------------------
			// -------------------------------------------------------------------------
			// -------------------------------------------------------------------------

			// NEW FUNCTIONS

			// Writes the new info to a file
			public void print_info()
			{
				int[] variable_vals = new int[]{getColonySize(),
												getQuorumThreshold(),
												getNewNumScouts(),
												getNumPassiveSite( mNests[1] ),
												getNumPassiveSite( mNests[2] ),
												getNumPassiveSite( mNests[3] ),
												getNumRecruitersBetweenSites1and2(),
												getNumRecruitersBetweenSites1and3(),
												getNumAssessingSite( mNests[2] ),
												getNumAssessingSite( mNests[3] )
												};

				write_line(variable_vals);
			}

			public void write_line(int[] variable_vals)
			{
				for(int value : variable_vals){ writer.printf("%d, ", value); }

				writer.printf("\n");
			}

			// -------------------------------------------------------------------------
			// -------------------------------------------------------------------------
			// -------------------------------------------------------------------------
			// -------------------------------------------------------------------------
			
		};
		updateActionGroup = new ActionGroup();
		updateActionGroup.addAction(updateAction);
		mSchedule.scheduleActionAtInterval(1, updateAction);
		if (!mBatch)
		{
			mQuorumGraph.display();
			mRecruitmentGraph.display();
		}

	}
	
	/**
	 * Nests constructor
	 *
	 * @param name for log file (null indicates send results to stdout)
	 * @param batch (true if model is running in batch mode, false otherwise)
	 * 
	 */
	public Nests(String logFileName, boolean batch)
	{
		String logString;

		// -------------------------------------------------------------------------
		// -------------------------------------------------------------------------
		// -------------------------------------------------------------------------
		// -------------------------------------------------------------------------

		// NEW VARIABLES

		file_number = 0;

		// -------------------------------------------------------------------------
		// -------------------------------------------------------------------------
		// -------------------------------------------------------------------------
		// -------------------------------------------------------------------------
		
		mThis = this;
		mAntColony = null;
		mColonySize = 100;
		mNumScouts = 10;
		mNormalQuorumThreshold = 20;
		mHostileQuorumThreshold = 15;
		mEmergencyQuorumThreshold = 10;
		mPreferenceSwitchProb = 0.5;
		mStartScoutingProb = 0.1;
		mStopScoutingProb = 0.0;
		mReverseTandemRunProb = 0.1;
		mChangeNestProb = 1.0;
		mMaxNestQuality = 100;
		mAssessmentDelay = 0;
		mTandemRunSpeed = 5;
		mCarryingSpeed= 15;
		mNestQualityRequirement = 20;
		mEnvironmentHostile = false;
		mDistanceSignificant = true;
		mCompareNestSiteQualities = true;
		mPrattEbmEquivalent = false;
		mBatch = batch;
		mNests = null;
		mNest1ToNest2Distance = 100;
		mNest1ToNest3Distance = 200;
		mNest2ToNest3Distance = 200;
		mNest1Habitable = false;
		mNest1Quality = 10;
		mNest2Quality = 20;
		mNest3Quality = 21;
		mNestQualityStdDev = 0;
		mTimeToVacation = 0;
		mLastTickNest1QuorumSize = 0;
		mLastTickNest2QuorumSize = 0;
		mLastTickNest3QuorumSize = 0;
		mNumTicksSinceQuorumSizeChange = 0;
		mNumRecruitmentActs = 0;
		mSchedule = null;
		mQuorumGraph = null;
		mRecruitmentGraph = null;
		logString = new String("ColonySize,NumScouts,NormalQuorumThreshold," +
			"HostileQuorumThreshold,EmergencyQuorumThreshold,PreferenceSwitchProb," +
			"StartScoutingProb,StopScoutingProb,ReverseTandemRunProb,ChangeNestProb,MaxNestSiteQuality,AssessmentDelay," +
			"TandemRunSpeed,CarryingSpeed,NestQualityRequirement,PrattEbmEquivalent,EnvironmentHostile," +
			"DistanceSignificant,CompareNestSiteQualities,Nest1ToNest2Distance,Nest1ToNest3Distance," +
			"Nest2ToNest3Distance,Nest1Habitable,Nest1Quality,Nest2Quality,Nest3Quality," +
			"NestQualityStdDev,ColonySplit,FinalDecisionOptimal,TimeToVacation,TimeToCompletion," +
			"NumRecruitmentActs,Nest1QuorumSize,Nest2QuorumSize,Nest3QuorumSize"); 
		if (logFileName != null)
		{
			try
			{
				mLogFile = new FileWriter(logFileName);
				mLogFile.write(logString + "\n");
			}
			catch (IOException exception)
			{
				throw new RuntimeException("Error opening log file: " + logFileName);
			}
		}
		else
		{
			mLogFile = null;
			System.out.println(logString);
		}

		// -------------------------------------------------------------------------
		// -------------------------------------------------------------------------
		// -------------------------------------------------------------------------
		// -------------------------------------------------------------------------

		// NEW FUNCTIONS

		delete_directory();
		create_folder();

		// -------------------------------------------------------------------------
		// -------------------------------------------------------------------------
		// -------------------------------------------------------------------------
		// -------------------------------------------------------------------------
	}

	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------

	// NEW FUNCTIONS

	// Creates a folder for the AH-HA results.
	public void create_folder()
	{
        File new_dir = new File("ahha_results");

        if (!new_dir.exists()) {

            try{
                new_dir.mkdir();
            } 
            catch(SecurityException se){ 
            	System.out.println("Error creating directory.");
            	System.exit(-1);
            }
        }
	}

	// Empties the folder if it's there.
	public void delete_directory() {

		File directory = new File("ahha_results");

	    if(directory.exists()){
	        File[] files = directory.listFiles();
	        if(null!=files){
	            for(int i=0; i<files.length; i++) {
	                   files[i].delete();
	                }
	            }
	     }
	    
	}

	// Creates a file to write the results to.
	public void create_file(int file_number)
	{
		try{
			writer = new PrintWriter("ahha_results/results_" + file_number + ".txt", "UTF-8");

			writer.printf("N, Q, S, P_0, P_1, P_2, R_1, R_2, A_1, A_2, \n");
		}
		catch(Exception e){

			System.out.printf("\n\nError creating file.\n\n");
			System.exit(1);

		}
	}

	public void close_file()
	{
		writer.close();
	}

	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
}

/*********************************************************************************************
* History:
* $Log: Nests.java,v $
* Revision 1.9  2005/08/05 14:39:43  marshall
* Added missing javadoc comment
*
* Revision 1.8  2005/01/04 14:02:52  marshall
* Renamed mMaxRecruitmentDelay to mMaxNestQuality and improved javadoc comments
*
* Revision 1.7  2005/01/03 14:32:56  marshall
* Added total quorum size graph plot
*
* Revision 1.6  2004/10/05 13:45:11  marshall
* Added TODO comment on irreproducible random seed sequences in batch mode
*
* Revision 1.5  2004/08/04 15:47:04  marshall
* Changed halting condition to avoid premature simulation termination
*
* Revision 1.4  2004/07/29 14:45:23  marshall
* Added a site assessment time cost and a termination condition for when emigrations overrun
*
*
* 
*********************************************************************************************/