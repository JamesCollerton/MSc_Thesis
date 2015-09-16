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
* $Date: 2005/01/04 14:04:49 $
* $Revision: 1.6 $ 
*
*********************************************************************************************/

package ahha;

import java.util.LinkedList;
import java.util.Iterator;
import uchicago.src.sim.util.Random;

/**
 * The class that represents ant colonies.
 * In a given application, the search processes should inherit from this class.
 *  
 */
public class AntColony
{
	/** The number of ants in the colony */
	private final int mColonySize;
	/** The number of scouts in the colony */
	private final int mNumScouts;
	/** The scouts in the colony */
	private LinkedList mScouts;
	/** The colony's current nest site */
	private NestSite mCurrentNest;
	/** The colony's nest quality requirement */
	private int mNestQualityRequirement;
	/** The colony's current environment (hostile or non-hostile) */
	private boolean mEnvironmentHostile;
	/** The colony's current quorum threshold (> 0 indicates colony is house-hunting) */
	private int mQuorumThreshold;
	/** The colony's quorum threshold for emigration with current nest site intact */
	private int mNormalQuorumThreshold;
	/** The colony's quorum threshold for emigration with current nest site destroyed */
	private int mEmergencyQuorumThreshold;
	/** The colony's quorum threshold for emigration with current nest site destroyed and in a hostile environment */
	private int mHostileQuorumThreshold;
	/** The probability with which an ant will evaluate another nest site against its current preference (0 to 1) */
	private double mPreferenceSwitchProb;
	/** The probability with which idle scouts will start scouting when the colony is seeking to emigrate (0 to 1) */
	private double mStartScoutingProb;
	/** The probability with which active scouts will stop scouting when the colony is seeking to emigrate (0 to 1) */
	private double mStopScoutingProb;
	/** The probability with which recruiting scouts will initiate a reverse tandem-run */
	private double mReverseTandemRunProb;
	/** The probability with which a scout will change its assessment of the current nest after switching its preference (if other colony members present at old preference) */
	private double mCurrentNestSwitchProb;
	/** The maximum nest quality (that for the best possible nest site) */ 
	private int mMaxNestSiteQuality;
	/** The time taken to assess a nest site */
	private int mAssessmentDelay;
	/** The speed (distance / timestep) of tandem-running recruitment */
	private int mTandemRunSpeed;
	/** The speed (distance / timestep) or carrying recruitment */
	private int mCarryingSpeed;
	/** Does distance to a neighbouring nest-site affect its chances of discovery? (true indicates it does) */
	private boolean mDistanceAffectsDiscovery; // TODO: change to general distance on/off flag
	/** Do ants compare nest sites against each other for quality? */
	private boolean mCompareNestSiteQualities;
	/** Is AH-HA configured to be equivalent to the Pratt et al EBM */
	private boolean mPrattEbmEquivalent;
	/** The number of recruitment acts performed within the colony during the last emigration */
	private int mNumRecruitmentActs;

	/**
	 * General utility method to see if a probabilistic event occurs
	 * 
	 * @param probability (0 <= probability <= 1)
	 * 
	 * @return true if the event occurs on this occassion, false otherwise
	 * 
	 */
	public static boolean eventOccurs(double probability)
	{
		if (probability < 0 || probability > 1)
		{
			throw new IllegalArgumentException("AntColony.eventOccurs called with probability out of range (0 <= " + probability + " <= 1)");
		}
		
		if (probability == 0)
		{
			return false;
		}
		else if (probability == 1)
		{
			return true;
		}
		else
		{
			return Random.uniform.nextDoubleFromTo(0, 1) <= probability;
		}
	}

	/**
	 * Gets the size of the colony
	 * 
	 * @return number of ants in the colony (> 0)
	 * 
	 */
	public int getColonySize()
	{
		return mColonySize;
	}

	/**
	 * Gets the number of recruitment acts performed within the colony during the last emigration
	 * 
	 * @return number of recruitment acts performed within the colony during the last emigration
	 * 
	 */
	public int getNumRecruitmentActs()
	{
		return mNumRecruitmentActs;
	}

	/**
	 *  Returns the colony's current quorum threshold
	 * 
	 * @return colony's current quorum threshold (0 indicates colony is not seeking to emigrate)
	 * 
	 */
	public int getQuorumThreshold()
	{
		return mQuorumThreshold;
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
	 * Gets the colony's current nest
	 * 
	 * @return colony's current nest (null indicates colony is homeless)
	 * 
	 */
	public NestSite getCurrentNest()
	{
		return mCurrentNest;
	}

	/**
	 * Gets the carrying speed of an ant
	 * 
	 * @return ant's carrying speed
	 * 
	 */
	public int getCarryingSpeed()
	{
		return mCarryingSpeed;
	}

	/**
	 * Gets the tandem running speed of an ant
	 * 
	 * @return ant's tandem running speed
	 * 
	 */
	public int getTandemRunSpeed()
	{
		return mTandemRunSpeed;
	}

	/**
	 * Gets the probability with which an ant will evaluate another nest site against its current preference
	 * 
	 * @return probability of assessing another nest site (0 to 1)
	 * 
	 */
	public double getPreferenceSwitchProb()
	{
		return mPreferenceSwitchProb;
	}

	/**
	 * Gets the probability with which an idle scout will start scouting when the colony is seeking to emigrate 
	 * 
	 * @return probability an idle scout will start scouting
	 * 
	 */
	public double getStartScoutingProb()
	{
		return mStartScoutingProb;
	}

	/**
	 * Gets the probability with which an active scout will stop scouting when the colony is seeking to emigrate 
	 * 
	 * @return probability an active scout will stop scouting
	 * 
	 */
	public double getStopScoutingProb()
	{
		return mStopScoutingProb;
	}

	/**
	 * Gets the probability with which a recruiting scout will initiate a reverse tandem-run 
	 * 
	 * @return probability a recruiting scout will initiate a reverse tandem-run
	 * 
	 */
	public double getReverseTandemRunProb()
	{
		return mReverseTandemRunProb;
	}

	/**
	 * Gets the probability with which a scout will change its assessment of the current nest after switching its preference (if other colony members present at old preference) 
	 * 
	 * @return probability with which a scout will change its assessment of the current nest after switching its preference (if other colony members present at old preference)
	 * 
	 */
	public double getCurrentNestSwitchProb()
	{
		return mCurrentNestSwitchProb;
	}

	/**
	 * Gets the maximum nest quality (for the best possible nest site)
	 * 
	 * @return maximum recruitment delay
	 * 
	 */
	public int getMaxNestSiteQuality()
	{
		return mMaxNestSiteQuality;
	}

	/**
	 * Gets the time a scout take to assess a site
	 * 
	 * @return time scout takes to assess a site
	 * 
	 */
	public int getAssessmentDelay()
	{
		return mAssessmentDelay;
	}

	/**
	 * Determines if the colony's environment is hostile or not
	 * 
	 * @return true if the environment is hostile, false otherwise
	 * 
	 */
	public boolean isEnvironmentHostile()
	{
		return mEnvironmentHostile;
	}

	/**
	 * Gets the colony's quorum threshold for normal emigrations
	 * 
	 * @return colony's quorum threshold for normal emigrations
	 * 
	 */
	public int getNormalQuorumThreshold()
	{
		return mNormalQuorumThreshold;
	}

	/**
	 * Gets the colony's quorum threshold for normal emigrations in hostile environments
	 * 
	 * @return colony's quorum threshold for normal emigrations in hostile environments
	 * 
	 */
	public int getHostileQuorumThreshold()
	{
		return mHostileQuorumThreshold;
	}

	/**
	 * Gets the colony's quorum threshold for emergency emigrations
	 * 
	 * @return colony's quorum threshold for emergency emigrations
	 * 
	 */
	public int getEmergencyQuorumThreshold()
	{
		return mEmergencyQuorumThreshold;
	}

	/**
	 * Gets the number of scouts recruiting from the specified origin to the specified destination nest site
	 * 
	 * @param origin nest site (!= null)
	 * @param destination nest site (!= null)
	 * 
	 * @return number of recruiters between specified nest sites
	 * 
	 */
	public int getNumRecruitersBetweenSites(NestSite origin, NestSite destination)
	{
		if (origin == null)
		{
			throw new IllegalArgumentException("AntColony.getNumRecruitersBetweenSites called with origin == null");
		}
		if (destination == null)
		{
			throw new IllegalArgumentException("AntColony.getNumRecruitersBetweenSites called with destination == null");
		}
		int numRecruiters = 0;
		Ant scout;
		Iterator i1;
		
		i1 = mScouts.iterator();
		while (i1.hasNext())
		{
			scout = (Ant) i1.next();
			if (scout.getCurrentNest() != null && scout.getCurrentNest().equals(origin) && scout.getPreference() != null && scout.getPreference().equals(destination))
			{
				numRecruiters++;
			}
		}
		
		return numRecruiters;
	}

	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------

	// NEW FUNCTIONS

	// Gets the number of ants assessing a site.
	public int getNumAssessingSite(NestSite nest_site)
	{
		if (nest_site == null)
		{
			throw new IllegalArgumentException("AntColony.getNumAssessingSite called with nest_site == null");
		}

		int numAssessors = 0;
		Ant scout;
		Iterator i1;
		
		i1 = mScouts.iterator();
		while (i1.hasNext())
		{
			scout = (Ant) i1.next();
			if( scout.getAssessing() && scout.getPreference() != null && scout.getPreference().equals(nest_site) ){ 
				numAssessors++; 
			}
		}
		
		return numAssessors;
	}

	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------

	/**
	 * Determines if distance between nest sites affects their chances of discovery
	 * 
	 * @return true if distance between nest sites affects discovery chances, false otherwise
	 * 
	 */
	public boolean distanceAffectsDiscovery()
	{
		return mDistanceAffectsDiscovery;
	}

	/**
	 * Determines if ants compare nest sites against each other for quality
	 * 
	 * @return true if ants compare nest sites against each other for quality, false otherwise
	 * 
	 */
	public boolean compareNestSiteQualities()
	{
		return mCompareNestSiteQualities;
	}

	/**
	 * Determines if AH-HA is configured to be equivalent to the Pratt et al EBM
	 * 
	 * @return true if AH-HA is configured to be equivalent to the Pratt et al EBM, false otherwise
	 * 
	 */
	public boolean isPrattEbmEquivalent()
	{
		return mPrattEbmEquivalent;
	}

	/**
	 * Increments the number of recruitment acts performed within the colony during the last emigration
	 * 
	 * @return void
	 *
	 */
	public void incrementNumRecruitmentActs()
	{
		mNumRecruitmentActs++;
	}

	/**
	 * Forces colony emigration by destroying current nest
	 * 
	 * @return void
	 *
	 */
	public void emigrate()
	{
		// destroy current nest
		mCurrentNest.setHabitable(false);
	}

	/**
	 * Probabilistically recruits a scout to a nest site
	 * 
	 * @param nest site to try and recruit scout to (!= null)
	 * @param transporting (true if the recruit was carried, false if it followed a tandem-run)
	 * @param activeAntsOnly (true if only active ants should be considered for recruitment, false otherwise) 
	 * 
	 * @return true if a scout was recruited to the nest site, false otherwise
	 * 
	 */
	public boolean recruitScoutToSite(NestSite nestSite, boolean transporting, boolean activeAntsOnly)
	{
		if (nestSite == null)
		{
			throw new IllegalArgumentException("AntColony.recruitScoutToSite called with nestSite == null");
		}
		Ant scout;
		Iterator i1;

		if (!activeAntsOnly)
		{
			// TODO: randomise search order?
			i1 = mScouts.iterator();
			while (i1.hasNext())
			{
				scout = (Ant) i1.next();
				if (scout.getPreference() == null)
				{
					return scout.recruitToNestSite(nestSite, transporting);
				}
			}
		}
		// no preferenceless scouts found, try and recruit a scout with an existing preference
		if (!mPrattEbmEquivalent)
		{
			scout = (Ant) mScouts.get(Random.uniform.nextIntFromTo(0, mScouts.size() - 1));

			return scout.recruitToNestSite(nestSite, transporting);
		}
		else
		{
			i1 = mScouts.iterator();
			while (i1.hasNext())
			{
				scout = (Ant) i1.next();
				if (scout.getPreference() != null && !scout.getPreference().equals(nestSite))
				{
					return scout.recruitToNestSite(nestSite, transporting);
				}
			}
			
			return false;
		}
	}
	
	/**
	 * Attempts to lead an ant by reverse tandem-run from one nest site to another (reallocates recruitment effort)
	 * 
	 * @param origin of reverse tandem-run (!= null)
	 * @param destination of reverse tandem-run (!= null)
	 * 
	 * @return true if a scout was lead by reverse tandem-run, false otherwise
	 * 
	 */
	public boolean reverseTandemRunFromTo(NestSite origin, NestSite destination)
	{
		if (origin == null)
		{
			throw new IllegalArgumentException("AntColony.reverseTandemRunScoutToNestSite called with origin == null");
		}
		if (destination == null)
		{
			throw new IllegalArgumentException("AntColony.reverseTandemRunScoutToNestSite called with destination == null");
		}
		Ant scout;
		Iterator i1;
		
		// TODO: randomise search order?
		i1 = mScouts.iterator();
		while (i1.hasNext())
		{
			scout = (Ant) i1.next();
			if ((scout.getPreference() != null && scout.getPreference().equals(origin) && scout.getCurrentNest() != destination) || (scout.getPreference() == null && scout.getCurrentNest().equals(origin)))
			{
				scout.reverseTandemRunFromTo(origin, destination);
				
				return true;
			}
		}

		return false;
	}

	/**
	 * Updates the ant colony (house-hunt / emigrate, etc.)
	 * 
	 * @return void
	 *
	 */
	public void update()
	{
		Iterator i1;
		Ant scout;

		// System.out.printf("\n\n QUORUM: %d \n\n", mQuorumThreshold);

		if (mQuorumThreshold == 0)
		{
			if (!mCurrentNest.isHabitable())
			{
				// current nest destroyed, emergency emigration
				mQuorumThreshold = mEmergencyQuorumThreshold;
			}
			else
			{
				if (mCurrentNest.getSiteQuality() < mNestQualityRequirement)
				{
					// current nest unsatisfactory, move to improve
					if (mEnvironmentHostile)
					{
						// environment is hostile, set quorum threshold accordingly
						mQuorumThreshold = mHostileQuorumThreshold;
					}
					else
					{
						// environment is non-hostile, set quorum threshold accordingly
						mQuorumThreshold = mNormalQuorumThreshold;
					}
				}
				else
				{
					// TODO: what if assessment / emigration has started but current nest site is now satisfactory?
					mQuorumThreshold = 0;
				}
			}
			mNumRecruitmentActs = 0;
		}
		if (mQuorumThreshold > 0)
		{
			// TODO: randomise order of updates?
			i1 = mScouts.iterator();
			while (i1.hasNext())
			{
				scout = (Ant) i1.next();
				scout.update();
			}
		}
	}

	/**
	 * Indicates to the colony that emigration has been completed to a new nest
	 * 
	 * @param new nest (!= null)
	 * 
	 * @return void
	 * 
	 */
	public void emigrationCompleted(NestSite newNest)
	{
		if (newNest == null)
		{
			throw new IllegalArgumentException("AntColony.emigrationCompleted called with newNest == null");
		}
		
		mCurrentNest = newNest;
		mQuorumThreshold = 0;
	}
	
	/**
	 * AntColony constructor
	 * 
	 * @param colonySize (> 0)
	 * @param numScouts (> 0)
	 * @param normalQuorumThreshold (> 0)
	 * @param emergencyQuorumThreshold (> 0)
	 * @param hostileQuorumThreshold (> 0)
	 * @param preferenceSwitchProb (0 <= preferenceSwitchProb <= 1)
	 * @param startScoutingProb (0 <= startScoutingProb <= 1)
	 * @param stopScoutingProb (0 <= stopScoutingProb <= 1)
	 * @param reverseTandemRunProb (0 <= reverseTandemRunProb <= 1)
	 * @param currentNestSwitchProb (0 <= currentNestSwitchProb <= 1)
	 * @param maxRecruitmentDelay (> 0)
	 * @param assessmentDelay (>= 0)
	 * @param tandemRunSpeed (> 0)
	 * @param carryingSpeed (> 0)
	 * @param currentNest (!= null)
	 * @param nestQualityRequirement (> 0)
	 * @param environmentHostile
	 * @param distanceAffectsDiscovery
	 * @param compareNestSiteQualities
	 * @param prattEbmEquivalent
	 * 
	 */
	public AntColony(int colonySize, int numScouts, int normalQuorumThreshold, int emergencyQuorumThreshold, int hostileQuorumThreshold, double preferenceSwitchProb, double startScoutingProb, double stopScoutingProb, double reverseTandemRunProb, double currentNestSwitchProb, int maxRecruitmentDelay, int assessmentDelay, int tandemRunSpeed, int carryingSpeed, NestSite currentNest, int nestQualityRequirement, boolean environmentHostile, boolean distanceAffectsDiscovery, boolean compareNestSiteQualities, boolean prattEbmEquivalent)
	{
		if (colonySize <= 0)
		{
			throw new IllegalArgumentException("Attempt to construct AntColony with colonySize <= 0 (colonySize == " + colonySize + ")");
		}
		if (numScouts <= 0 || numScouts > colonySize)
		{
			throw new IllegalArgumentException("Attempt to construct AntColony with numScouts out of range (0 < " + numScouts + " <= " + colonySize + ")");
		}
		if (normalQuorumThreshold <= 0)
		{
			throw new IllegalArgumentException("Attempt to construct AntColony with normalQuorumThreshold <= 0 (normalQuorumThreshold == " + normalQuorumThreshold + ")");
		}
		if (emergencyQuorumThreshold <= 0)
		{
			throw new IllegalArgumentException("Attempt to construct AntColony with emergencyQuorumThreshold <= 0 (emergencyQuorumThreshold == " + emergencyQuorumThreshold + ")");
		}
		if (hostileQuorumThreshold <= 0)
		{
			throw new IllegalArgumentException("Attempt to construct AntColony with hostileQuorumThreshold <= 0 (hostileQuorumThreshold == " + hostileQuorumThreshold + ")");
		}
		if (preferenceSwitchProb < 0 || preferenceSwitchProb > 1)
		{
			throw new IllegalArgumentException("Attempt to construct AntColony with preferenceSwitchProb out of range (0 <= " + preferenceSwitchProb + " <= 1)");
		}
		if (startScoutingProb < 0 || startScoutingProb > 1)
		{
			throw new IllegalArgumentException("Attempt to construct AntColony with startScoutingProb out of range (0 <= " + startScoutingProb + " <= 1)");
		}
		if (stopScoutingProb < 0 || stopScoutingProb > 1)
		{
			throw new IllegalArgumentException("Attempt to construct AntColony with stopScoutingProb out of range (0 <= " + stopScoutingProb + " <= 1)");
		}
		if (reverseTandemRunProb < 0 || reverseTandemRunProb > 1)
		{
			throw new IllegalArgumentException("Attempt to construct AntColony with reverseTandemRunProb out of range (0 <= " + reverseTandemRunProb + " <= 1)");
		}
		if (currentNestSwitchProb < 0 || currentNestSwitchProb > 1)
		{
			throw new IllegalArgumentException("Attempt to construct AntColony with currentNestSwitchProb out of range (0 <= " + currentNestSwitchProb + " <= 1)");
		}
		if (maxRecruitmentDelay <= 0)
		{
			throw new IllegalArgumentException("Attempt to construct AntColony with maxRecruitmentDelay <= 0 (maxRecruitmentDelay == " + maxRecruitmentDelay + ")");
		}
		if (assessmentDelay < 0)
		{
			throw new IllegalArgumentException("Attempt to construct AntColony with assessmentDelay < 0 (assessmentDelay == " + assessmentDelay + ")");
		}
		if (tandemRunSpeed <= 0)
		{
			throw new IllegalArgumentException("Attempt to construct AntColony with tandemRunSpeed <= 0 (tandemRunSpeed == " + tandemRunSpeed + ")");
		}
		if (carryingSpeed <= 0)
		{
			throw new IllegalArgumentException("Attempt to construct AntColony with carryingSpeed <= 0 (carryingSpeed == " + carryingSpeed + ")");
		}
		if (currentNest == null)
		{
			throw new IllegalArgumentException("Attempt to construct AntColony with currentNest == null");
		}
		if (nestQualityRequirement <= 0)
		{
			throw new IllegalArgumentException("Attempt to construct AntColony with nestQualityRequirement <= 0 (nestQualityRequirement == " + nestQualityRequirement + ")");
		}
		int l1;
		
		mColonySize = colonySize;
		mNumScouts = numScouts;
		mScouts = new LinkedList();
		mCurrentNest = currentNest;
		for (l1 = 0; l1 < mNumScouts; l1++)
		{
			mScouts.addLast(new Ant(this));
		}
		mNestQualityRequirement = nestQualityRequirement;
		mEnvironmentHostile = environmentHostile;
		mNormalQuorumThreshold = normalQuorumThreshold;
		mEmergencyQuorumThreshold = emergencyQuorumThreshold;
		mHostileQuorumThreshold = hostileQuorumThreshold;
		mPreferenceSwitchProb = preferenceSwitchProb;
		mStartScoutingProb = startScoutingProb;
		mStopScoutingProb = stopScoutingProb;
		mReverseTandemRunProb = reverseTandemRunProb;
		mCurrentNestSwitchProb = currentNestSwitchProb;
		mMaxNestSiteQuality = maxRecruitmentDelay;
		mAssessmentDelay = assessmentDelay;
		mTandemRunSpeed = tandemRunSpeed;
		mCarryingSpeed = carryingSpeed;
		mDistanceAffectsDiscovery = distanceAffectsDiscovery;
		mCompareNestSiteQualities = compareNestSiteQualities;
		mPrattEbmEquivalent = prattEbmEquivalent;
		mCurrentNest.incrementQuorumSize(this, numScouts, true);
		mCurrentNest.incrementQuorumSize(this, mColonySize - numScouts, false);
		mNumRecruitmentActs = 0;
		if (!mCurrentNest.isHabitable())
		{
			// colony is homeless so must find a nest ASAP
			if (mEnvironmentHostile)
			{
				// environment is hostile, set quorum threshold accordingly
				mQuorumThreshold = mHostileQuorumThreshold;
			}
			else
			{
				// environment is non-hostile, set quorum threshold accordingly
				mQuorumThreshold = mEmergencyQuorumThreshold;
			}
		}
		else
		{
			// colony is housed so will only emigrate opportunistically
			if (mCurrentNest.getSiteQuality() < mNestQualityRequirement)
			{
				// current nest unsatisfactory, move to improve
				mQuorumThreshold = mNormalQuorumThreshold;
			}
			else
			{
				mQuorumThreshold = 0;
			}
		}
	}
}

/*********************************************************************************************
* History:
* $Log: AntColony.java,v $
* Revision 1.6  2005/01/04 14:04:49  marshall
* Changed from fixed length recruitment delay to per-timestep probability of beginning recruitment
*
* Revision 1.5  2004/11/30 16:05:40  marshall
* Changed carried scouts to be unable to recruit for biological plausibility
*
* Revision 1.4  2004/07/29 14:34:04  marshall
* Added a variable time cost for site assessment
*
* Revision 1.3  2004/05/19 13:31:36  marshall
* Made Pratt et al EBM equivalence mode more equivalent
*
* Revision 1.2  2004/05/07 16:56:24  marshall
* Added options for Pratt et al EBM equivalence and for recruitment decay rate
*
* Revision 1.1  2004/04/19 13:28:32  marshall
* First imported version
*
* 
*********************************************************************************************/