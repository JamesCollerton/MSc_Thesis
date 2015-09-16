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
* $Date: 2005/01/04 14:04:48 $
* $Revision: 1.14 $ 
*
*********************************************************************************************/

package ahha;

/**
 * The class used in the collective decision making process that is at the heart of AH-HA
 *  
 */
class Ant
{
	/** The colony the ant is a member of */
	private final AntColony mColony;
	/** The nest the ant considers to be the colony's current nest */
	private NestSite mCurrentNest;
	/** The ant's preferred nest site */
	private NestSite mPreference;
	/** The quality of the ant's preferred nest according to the ant's last assessment of it */
	private int mPreferenceAssessedQuality;
	/** The quality of the site the ant last considered according to the ant's last assessment of it */
	private int mConsideredNewSiteAssessedQuality;
	/** The ant's current quorum threshold (> 0 indicates ant is scouting) */
	private int mQuorumThreshold;
	/** The delay until an ant has finished assessing a nest site */
	private int mAssessmentDelay;
	/** The distance-related delay until the ant may begin recruiting to its current preference (iterations of Ant.update) */
	private int mRecruitmentDelay;
	/** The probability with which the ant will begin recruiting to its current preference, after its distance-related recruitment delay has expired (per iteration of Ant.update) */
	private double mRecruitmentProb;
	/** Ant is transporting? (false indicates ant is tandem-running) */
	private boolean mTransporting;
	/** The delay until the ant finishes transporting a nest mate */
	private int mTransportDelay;

	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------

	// NEW VARIABLE

	// To tell us if an ant is assessing or not.
	private boolean mAssessing;

	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	
	/**
	 * Gets the nest site that the ant considers to be the colony's current nest site (the source of the ant's recruitment efforts)
	 * 
	 * @return ant's current nest site
	 * 
	 */
	public NestSite getCurrentNest()
	{
		return mCurrentNest;
	}

	/**
	 * Gets the ant's preferred nest site to which it is recruiting (if any) 
	 * 
	 * @return ant's preferred nest site (null indicates ant has no preference and is still in the current nest) 
	 * 
	 */
	public NestSite getPreference()
	{
		return mPreference;
	}

	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------

	// NEW FUNCTIONS

	// If the ant is in an assessing state or not.
	public boolean getAssessing()
	{
		return mAssessing;
	}

	// Change the ant's state to assessing.
	public void setAssessing(boolean bool_val)
	{
		mAssessing = bool_val;
	}

	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------
	// -------------------------------------------------------------------------

	/**
	 * Reverse tandem-runs the ant between the specified nest site (reallocates recruitment effort)
	 * 
	 * @param origin of reverse tandem-run (!= null)
	 * @param destination of reverse tandem-run (!= null)
	 * 
	 * @return void
	 * 
	 */
	public void reverseTandemRunFromTo(NestSite origin, NestSite destination)
	{
		if (origin == null)
		{
			throw new IllegalArgumentException("Ant.reverseTandemRunFromTo called with origin == null");
		}
		if (destination == null)
		{
			throw new IllegalArgumentException("Ant.reverseTandemRunFromTo called with destination == null");
		}

		mCurrentNest = destination;
		mPreference = origin;
		// TODO: re-assign mPreferenceAssessedQuality?
	}

	/**
	 * Recruits the ant to a nest site if it has no current preference (probabilistically otherwise)
	 * 
	 * @param nest site to recruit ant to (!= null)
	 * @param transported (true if the recruit was carried, false if it followed a tandem-run)
	 *
	 * @return true if the ant was recruited to the nest site, false otherwise
	 * 
	 */
	public boolean recruitToNestSite(NestSite nestSite, boolean transported)
	{
		if (nestSite == null)
		{
			throw new IllegalArgumentException("Ant.recruitToNestSite called with nestSite == null");
		}
		
		if (mPreference == null)
		{
			// scout has no preference
			if (!nestSite.equals(mCurrentNest) && (!mCurrentNest.isHabitable() | chooseNewNestSite(nestSite)) && nestSite.isHabitable()) // full evaluation or-operator used because chooseNewNestSite MUST be evaluated TODO: check nest site meets colony's requirements 
			{
				mCurrentNest.decrementQuorumSize(mColony, 1, true);
				nestSite.incrementQuorumSize(mColony, 1, true);
				// TODO: should a recruited scout ALWAYS assess a site and delay according to its quality? Even if carried to the site?
				if (!transported)
				{
					mPreference = nestSite;
					mPreferenceAssessedQuality = mConsideredNewSiteAssessedQuality;
					mAssessmentDelay = mColony.getAssessmentDelay();
					mRecruitmentDelay = 0;
					mRecruitmentProb = calculateRecruitmentProb();
				}
				else
				{
					mCurrentNest = nestSite;
					mPreference = null;
					mPreferenceAssessedQuality = mConsideredNewSiteAssessedQuality;
					mRecruitmentDelay = 0;
					mRecruitmentProb = 0;
					mAssessmentDelay = mColony.getAssessmentDelay();
				}
				
				return true;
			}
			else
			{
				// scout cannot be recruited to the nest site it's currently in, to a nest site that is inferior, or to a nest-site that is uninhabitable
				return false;
			}
		}
		else
		{
			// scout has a preference and will only switch probabilistically and after comparing the new nest site to its current preference
			// TODO: is mPreferenceSwitchProbability the right value to use for probability of recruiting a scout? Could change to depend on quality of new nest site
			if (AntColony.eventOccurs(mColony.getPreferenceSwitchProb()))
			{
				if (chooseNewNestSite(nestSite) && !nestSite.equals(mPreference) && nestSite.isHabitable()) // TODO: compare nest site quality against colony's requirements
				{
					mPreference.decrementQuorumSize(mColony, 1, true);
					nestSite.incrementQuorumSize(mColony, 1, true);
					if (nestSite.equals(mCurrentNest))
					{
						// ant has been recruited back to its origin nest so will reverse the direction of its recruitment acts
						mCurrentNest = mPreference;
					}
					else if (mPreference.getQuorumSize(mColony) > 0 && AntColony.eventOccurs(mColony.getCurrentNestSwitchProb()))
					{
						// ant has probabilistically changed its opinion on which is the current nest site
						mCurrentNest = mPreference;
					}
					if (!transported)
					{
						mPreference = nestSite;
						mPreferenceAssessedQuality = mConsideredNewSiteAssessedQuality;
						// ant has spent some time assessing the site
						mAssessmentDelay = mColony.getAssessmentDelay();
						// TODO: should the ant incur an additional delay before recruiting as well?
					}
					else
					{
						mCurrentNest = nestSite;
						mPreference = null;
						mPreferenceAssessedQuality = mConsideredNewSiteAssessedQuality;
						mRecruitmentDelay = 0;
						mRecruitmentProb = 0;
						// ant has spent some time assessing the site
						mAssessmentDelay = mColony.getAssessmentDelay();
						// TODO: should the ant incur an additional delay before scouting as well?
					}
	
					return true;
				}
				else
				{
					// ant has spent some time assessing the site
					mAssessmentDelay = mColony.getAssessmentDelay();
					// TODO: should the ant incur an additional delay before recruiting as well?
					
					return false;
				}
			}
			else
			{
				return false;
			}
		}
	}

	/**
	 * Determines if the ant should choose the new nest site over the old one by comparing their qualities (if nest site comparison enabled)
	 * 
	 * @param newNestSite (!= null)
	 * 
	 * @return true if the new nest site is of higher quality than the ant's last assessment of its current preference, or if nest site comparison disabled, false otherwise
	 * 
	 */
	private boolean chooseNewNestSite(NestSite newNestSite)
	{

		if (newNestSite == null)
		{
			throw new IllegalArgumentException("Ant.chooseNewNestSite called with newNestSite == null");
		}
		double acceptProbability;
		
		if (mColony.compareNestSiteQualities())
		{
			mConsideredNewSiteAssessedQuality = newNestSite.getSiteQuality();
			if (mConsideredNewSiteAssessedQuality > mPreferenceAssessedQuality)
			{
				return true;
			}
			else
			{
				return false;
			}
		}
		else
		{
			// calculate normalised acceptance probability between 0.25 and 0.75 (TODO: refine)
			mConsideredNewSiteAssessedQuality = newNestSite.getSiteQuality();
			acceptProbability = (double) mConsideredNewSiteAssessedQuality / mColony.getMaxNestSiteQuality();
			if (acceptProbability > 1.0)
			{
				acceptProbability = 1.0;
			}
			acceptProbability = (acceptProbability * 0.5) + 0.25;
			if (AntColony.eventOccurs(acceptProbability))
			{
				return true;
			}
			else
			{
				return false;
			}
		}
	}

	/**
	 * Calculates the recruitment delay for recruitment from one nest to another
	 * 
	 * @param currentNest (!= null)
	 * @param preference (!= null)
	 * 
	 * @return recruitment delay from current nest to preferred nest
	 * 
	 */
	private int calculateRecruitmentDelay(NestSite currentNest, NestSite preference)
	{
		if (currentNest == null)
		{
			throw new IllegalArgumentException("Ant.calculateRecruitmentDelay called with currentNest == null");
		}
		if (preference == null)
		{
			throw new IllegalArgumentException("Ant.calculateRecruitmentDelay called with preference == null");
		}
		int recruitmentDelay;
		
		recruitmentDelay = 0;
		if (mColony.distanceAffectsDiscovery())
		{
			if (!mColony.isPrattEbmEquivalent())
			{
				// take account of distance between current nest and preference
				recruitmentDelay += currentNest.getDistanceToNeighbour(preference) / mColony.getCarryingSpeed();
			}
		}

		return recruitmentDelay;
	}

	/**
	 * Calculates the per-timestep probability of recruitment to the ant's preference
	 * 
	 * @return per-timestep probability of recruitment to the ant's preference
	 * 
	 */
	private double calculateRecruitmentProb()
	{
		double recruitmentProb;
		
		recruitmentProb = (double) mPreferenceAssessedQuality / mColony.getMaxNestSiteQuality();
		if (recruitmentProb > 1)
		{
			recruitmentProb = 1;
		}

		return recruitmentProb;
	}
	
	/**
	 * Probabalistically stop the ant scouting
	 * 
	 * @return void
	 *
	 */
	private void considerStoppingScouting()
	{
		if (AntColony.eventOccurs(mColony.getStopScoutingProb()))
		{
			if (mPreference != null)
			{
				mPreference.decrementQuorumSize(mColony, 1, true);
			}
			mPreference = null;
			mCurrentNest.incrementQuorumSize(mColony, 1, true);
			mTransporting = false;
			mRecruitmentDelay = 0;
			mRecruitmentProb = 0;
			mTransportDelay = 0;
		}
	}
	
	/**
	 * Probabilistically compares another nest site against the ant's current preference
	 * 
	 * @return void
	 *
	 */
	private void considerAlternativeNestSite()
	{
		NestSite alternativeNestSite;
		double prob;

		if (mColony.isPrattEbmEquivalent())
		{
			prob = mColony.getPreferenceSwitchProb() * 2;
		}
		else
		{
			prob = mColony.getPreferenceSwitchProb();
		}
		if (prob > 1)
		{
			prob = 1;
		}
		if (AntColony.eventOccurs(prob))
		{
			// compare another neighbouring nest site against current preference
			alternativeNestSite = mCurrentNest.getRandomNeighbour(mColony.distanceAffectsDiscovery()); // TODO: should distance not be taken into account here?
			if (alternativeNestSite.isHabitable() && chooseNewNestSite(alternativeNestSite) && !alternativeNestSite.equals(mCurrentNest)  && !alternativeNestSite.equals(mPreference)) // TODO: check nest site meets colony's requirements
			{
				// switch preferred nest site
				mPreference.decrementQuorumSize(mColony, 1, true);
				alternativeNestSite.incrementQuorumSize(mColony, 1, true);
				// probabilistically change ant's opinion on which is the current nest site
				if (mPreference.getQuorumSize(mColony) > 0 && AntColony.eventOccurs(mColony.getCurrentNestSwitchProb()))
				{
					mCurrentNest = mPreference;
					//System.out.println("  (current nest site changed to old preference)");
				}
				mPreference = alternativeNestSite;
				mPreferenceAssessedQuality = mConsideredNewSiteAssessedQuality;
				mAssessmentDelay = mColony.getAssessmentDelay();
				if (mRecruitmentDelay > 0 || mRecruitmentProb > 0)
				{
					mRecruitmentDelay = calculateRecruitmentDelay(mCurrentNest, mPreference);
					mRecruitmentProb = calculateRecruitmentProb();				
				}
			}
			else
			{
				// scout has spent some time performing the assessment
				mAssessmentDelay = mColony.getAssessmentDelay();
			}
		}
	}

	/**
	 * Initiates a recruitment act (tandem-running or transporting) by the ant
	 * 
	 * @return void
	 *
	 */
	private void beginRecruiting()
	{
		mRecruitmentProb = 0;
		// sense quorum (TODO: moderate by non-nestmates present in preferred nest site)
		if (mPreference.getQuorumSize(mColony) > mQuorumThreshold)
		{
			mTransporting = true; 
		}
		else
		{
			mTransporting = false;
		}
		if (mTransporting)
		{
			// ant is transporting to selected nest site
			if (mColony.distanceAffectsDiscovery())
			{
				// only multiply delay by distance if distance is important
				mTransportDelay = mCurrentNest.getDistanceToNeighbour(mPreference) / mColony.getCarryingSpeed();
			}
			else
			{
				// TODO: distance not important
			}
		}
		else
		{
			// ant is tandem-running to selected nest site
			if (mColony.distanceAffectsDiscovery())
			{
				// only multiply delay by distance if distance is important
				mTransportDelay = mCurrentNest.getDistanceToNeighbour(mPreference) / mColony.getTandemRunSpeed();
			}
			else
			{
				// TODO: distance not important
			}
		}
	}
	
	/**
	 * Updates the ant's state (assessing / recruiting to nest site, etc.)
	 * 
	 * @return void
	 *
	 */
	public void update()
	{
		NestSite alternativeNestSite;
		Ant recruitedScout = null;

		if (mPreferenceAssessedQuality == 0)
		{
			// initialise ant's perception of its current nest's quality (couldn't be done in Ant constructor as random distribution hadn't been initialised then)
			mPreferenceAssessedQuality = mCurrentNest.getSiteQuality();
		}
		if (mColony.getQuorumThreshold() > 0)
		{
			// colony is seeking to emigrate
			if (mPreference == null)
			{
				if (mAssessmentDelay > 0)
				{
				// ant is asessing the quality of its current nest

					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------

					// NEW FUNCTION

					setAssessing(true);

					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------

					mAssessmentDelay--;
				}
				if (mAssessmentDelay == 0 && AntColony.eventOccurs(mColony.getStartScoutingProb()))
				{

					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------

					// NEW FUNCTION

					// Stopped assessing the site and moving on.
					setAssessing(false);

					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------

					// ant must discover a nest site
					if (!mCurrentNest.isHabitable())
					{
						mQuorumThreshold = mColony.getEmergencyQuorumThreshold();
					}
					else
					{
						if (mColony.isEnvironmentHostile())
						{
							mQuorumThreshold = mColony.getHostileQuorumThreshold();
						}
						else
						{
							mQuorumThreshold = mColony.getNormalQuorumThreshold();
						}
					}
					mPreference = mCurrentNest.getRandomNeighbour(mColony.distanceAffectsDiscovery()); // TODO: what if colony does not have a current nest?
					mPreferenceAssessedQuality = mPreference.getSiteQuality();
					if (mPreference.isHabitable() && (!mCurrentNest.isHabitable() | chooseNewNestSite(mPreference))) // full evaluation or-operator used because chooseNewNestSite MUST be evaluated TODO: check nest site meets colony's requirements
					{
						mCurrentNest.decrementQuorumSize(mColony, 1, true);
						mPreference.incrementQuorumSize(mColony, 1, true);
						mAssessmentDelay = mColony.getAssessmentDelay();
						mRecruitmentDelay = calculateRecruitmentDelay(mCurrentNest, mPreference);
						mRecruitmentProb = calculateRecruitmentProb();
					}
					else
					{
						mPreference = null;
					}
				}
			}
			if (mPreference != null)
			{
				// ant has selected or is considering a nest site
				if (mAssessmentDelay > 0)
				{
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------

					// NEW FUNCTION

					setAssessing(true);

					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------

					// ant is assessing a nest site
					mAssessmentDelay--;
					// TODO: should scouts probabilistically consider stopping assessing?
				}
				else if (mRecruitmentDelay > 0 || mRecruitmentProb > 0)
				{

					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------

					// NEW FUNCTION

					// Ant has assessed the site and moved on.
					setAssessing(false);

					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------

					// ant is waiting to begin recruiting
					mRecruitmentDelay--;
					if (mRecruitmentDelay < 0)
					{
						mRecruitmentDelay = 0;
					}
					if (mRecruitmentDelay == 0 && AntColony.eventOccurs(mRecruitmentProb))
					{
						// ant begins recruiting to preferred nest site
						beginRecruiting();
					}
					else
					{
						// probabilistically consider an alternative nest site
						considerAlternativeNestSite();
						// TODO: should scouts probabilistically consider stopping assessing?
					}
				}
				else
				{

					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------

					// NEW FUNCTION

					// Ant has assessed the site and moved on.
					setAssessing(false);

					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------
					// -------------------------------------------------------------------------

					// ant is recruiting
					// probabilistically consider an alternative nest site
					if (!mColony.isPrattEbmEquivalent())
					{
//						considerAlternativeNestSite(); // TODO: should an alternative nest site be considered at this point
					}
					mTransportDelay--;
					// probabilistically consider stopping recruiting
					if (!mTransporting)
					{
						considerStoppingScouting();
					}
					if (mColony.isPrattEbmEquivalent() && mPreference != null)
					{
						// ant can 'magically' recruit other active scouts even while carrying/tandem-running
						mColony.recruitScoutToSite(mPreference, mTransporting, true);
					}
					if (mPreference != null && mTransportDelay <= 0)
					{
						if (!AntColony.eventOccurs(mColony.getReverseTandemRunProb())) // TODO: make reverse tandem-runs always take correct amount of time
						{
							// normal recruitment (to preferred nest site)
							if (mCurrentNest.getQuorumSize(mColony) > 0)
							{
								// ant has moved a nest mate from the current nest to its preferred new nest site
								if (!mColony.recruitScoutToSite(mPreference, mTransporting, false))
								{
									if (mCurrentNest.getQuorumSize(mColony, false) > 0)
									{
										mCurrentNest.decrementQuorumSize(mColony, 1, false);
										mPreference.incrementQuorumSize(mColony, 1, false);
										mColony.incrementNumRecruitmentActs();
									}
								}
								else
								{
									mColony.incrementNumRecruitmentActs();
								}
								// end of a recruitment act, so consider alternative nest site
								considerAlternativeNestSite();
								// begin recruiting to preferred nest site
								beginRecruiting();
							}
							else
							{
								// emigration from original nest has been completed
								mCurrentNest = mPreference;
								if (mPreference.getQuorumSize(mColony) == mColony.getColonySize())
								{
									// colony has converged on a single new nest site
									mColony.emigrationCompleted(mPreference);
								}
								mPreference = null;
								mTransporting = false;
								mRecruitmentDelay = 0;
								mRecruitmentProb = 0;
							}
						}
						else
						{
							// reverse tandem-run
							if (mColony.reverseTandemRunFromTo(mPreference, mCurrentNest))
							{
								mColony.incrementNumRecruitmentActs();
							}
						}
					}
				}
			}
		}
		else
		{
			// colony is not seeking to emigrate, reset ant's preference and transport mode
			mPreference = null;
			mCurrentNest = mColony.getCurrentNest();
			mTransporting = false;
			mRecruitmentDelay = 0;
			mRecruitmentProb = 0;
		}
	}
	
	/**
	 * Ant constructor
	 *
	 * @param colony (!= null)
	 * 
	 */
	public Ant(AntColony colony)
	{
		if (colony == null)
		{
			throw new IllegalArgumentException("Attempt to construct Ant with colony == null");
		}
		
		mColony = colony;
		mCurrentNest = mColony.getCurrentNest();
		mPreference = null;
		mPreferenceAssessedQuality = 0;
		mConsideredNewSiteAssessedQuality = 0;
		mQuorumThreshold = 0;
		mTransporting = false;
		mAssessing = false;
		mAssessmentDelay = 0;
		mRecruitmentDelay = 0;
		mRecruitmentProb = 0;
	}
}

/*********************************************************************************************
* History:
* $Log: Ant.java,v $
* Revision 1.14  2005/01/04 14:04:48  marshall
* Changed from fixed length recruitment delay to per-timestep probability of beginning recruitment
*
* Revision 1.13  2004/11/30 21:48:33  marshall
* Fixed crash bug in initialising ant's perception of current nest site quality
*
* Revision 1.12  2004/11/30 18:51:12  marshall
* Made scouts aware of the quality of their current nest, and removed a superflous parameter from chooseNewNestSite
*
* Revision 1.11  2004/11/30 16:05:40  marshall
* Changed carried scouts to be unable to recruit for biological plausibility
*
* Revision 1.10  2004/11/24 21:09:33  marshall
* Made ant store its quality assessment of a selected site and use it for future comparisons, etc.
*
* Revision 1.9  2004/10/05 13:47:53  marshall
* Modified ant to only consider alternative sites after completing an assessment or a recruitment act
*
* Revision 1.8  2004/07/29 14:35:07  marshall
* Added a variable time cost for site assessment
*
* Revision 1.7  2004/05/25 11:23:36  marshall
* Fixed preference switch prob error out of range
*
* Revision 1.6  2004/05/24 15:54:31  marshall
* Changed conditions under which scouts may stop scouting
*
* Revision 1.5  2004/05/19 13:31:36  marshall
* Made Pratt et al EBM equivalence mode more equivalent
*
* Revision 1.4  2004/05/11 18:07:07  marshall
* Fixed error in Pratt et al EBM equivalence option
*
* Revision 1.3  2004/05/07 16:56:25  marshall
* Added options for Pratt et al EBM equivalence and for recruitment decay rate
*
* Revision 1.2  2004/04/23 10:03:51  marshall
* Tidied up and added conditional acceptance for no direct comparisons case
*
* Revision 1.1  2004/04/19 13:28:32  marshall
* First imported version
*
* 
*********************************************************************************************/