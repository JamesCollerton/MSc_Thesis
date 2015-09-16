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
* $Date: 2005/01/04 14:03:55 $
* $Revision: 1.3 $ 
*
*********************************************************************************************/

package ahha;

import java.util.HashMap;
import java.util.ArrayList;
import java.util.Iterator;
import java.lang.RuntimeException;
import uchicago.src.sim.util.Random;

/**
 * The class that represents alternatives for ant colonies to assess and choose between.
 * In a given application, the candidate solutions should inherit from this class.
 * 
 */
public abstract class NestSite
{
	/**
	 * Inner-class for representing neighbouring nest sites and the distance to them
	 * 
	 */
	private class Neighbour
	{
		/** The neighbouring nest site */
		public final NestSite mNeighbour;
		/** The distance to the neighbouring nest site */
		public final int mDistance;
		
		public Neighbour(NestSite neighbour, int distance)
		{
			mNeighbour = neighbour;
			mDistance = distance;
		}
	}

	private class ColonyQuorum
	{
		/** The number of scouts in the quorum */
		public int mActiveQuorum;
		/** The number of passive ants in the quorum */
		public int mPassiveQuorum;
		
		public ColonyQuorum()
		{
			mActiveQuorum = 0;
			mPassiveQuorum = 0;
		}
	}
	
	/** Nest site is habitable? (true indicates it is) */
	private boolean mIsHabitable;
	/** Total number of ants present in / assesing / recruiting to nest site */
	private int mQuorumSize;
	/** Ants present in / assessing / recruiting to nest site from different colonies */
	private HashMap mColonyQuorums;
	/** List of neighbouring nest sites */
	private ArrayList mNeighbours;
	/** Total distance to all neighbouring nest sites */
	private int mTotalNeighbourDistance;
	
	/**
	 *Gets the total quorum size (number of ants in / assessing / recruiting to nest site) 
	 * 
	 * @return total quorum size (>= 0)
	 * 
	 */
	public int getQuorumSize()
	{
		return mQuorumSize;
	}
	
	/**
	 * Gets the quorum size for a given colony (number of ants in / assessing / recruiting to nest site)
	 * 
	 * @param colony (!= null)
	 * 
	 * @return quorum size (>= 0)
	 * 
	 */
	public int getQuorumSize(AntColony colony)
	{
		if (colony == null)
		{
			throw new IllegalArgumentException("NestSite.getQuorumSize called with colony == null");
		}
		ColonyQuorum quorum = null;
		
		quorum = (ColonyQuorum) mColonyQuorums.get(colony);
		if (quorum != null)
		{
			return quorum.mActiveQuorum + quorum.mPassiveQuorum;
		}
		else
		{
			return 0;
		}
	}

	/**
	 * Gets the quorum size for a given colony (number of ants in / assessing / recruiting to nest site)
	 * 
	 * @param colony (!= null)
	 * @param active ants? (true if quorum of scouts is required, false if quorum of passive ants is required)
	 * 
	 * @return quorum size (>= 0)
	 * 
	 */
	public int getQuorumSize(AntColony colony, boolean active)
	{
		if (colony == null)
		{
			throw new IllegalArgumentException("NestSite.getQuorumSize called with colony == null");
		}
		ColonyQuorum quorum = null;
		
		quorum = (ColonyQuorum) mColonyQuorums.get(colony);
		if (quorum != null)
		{
			if (active)
			{
				return quorum.mActiveQuorum;
			}
			else
			{
				return quorum.mPassiveQuorum;
			}
		}
		else
		{
			return 0;
		}
	}
	
	/**
	 * Increments the quorum size (number of ants assessing / recruiting to nest site)
	 * 
	 * @param colony ant joining quorum is from (!= null)
	 * @param number of ants to increment quorum by (> 0)
	 * @param active ants flag (true if ants are scouts, false otherwise)
	 * 
	 * @return void
	 * 
	 */
	public void incrementQuorumSize(AntColony colony, int numAnts, boolean active)
	{
		if (colony == null)
		{
			throw new IllegalArgumentException("NestSite.incrementQuorumSize called with colony == null");
		}
		if (numAnts < 0)
		{
			throw new IllegalArgumentException("NestSite.incrementQuorumSize called with numAnts < 0 (numAnts == " + numAnts + ")");
		}
		ColonyQuorum quorum;
		
		mQuorumSize += numAnts;
		quorum = (ColonyQuorum) mColonyQuorums.get(colony);
		if (quorum != null)
		{
			if (active)
			{
				quorum.mActiveQuorum += numAnts;
			}
			else
			{
				quorum.mPassiveQuorum += numAnts;
			}
		}
		else
		{
			quorum = new ColonyQuorum();
			if (active)
			{
				quorum.mActiveQuorum = numAnts;
			}
			else
			{
				quorum.mPassiveQuorum = numAnts;
			}
			mColonyQuorums.put(colony, quorum);
		}
	}

	/**
	 * Decrements the quorum size (number of ants assessing / recruiting to nest site)
	 * 
	 * @param colony ant leaving quorum is from (!= null)
	 * @param number of ants to decrement quorum by (<= NestSite.getQuorumSize(colony))
	 * @param active ants flag (true if ants are scouts, false otherwise)
	 * 
	 * @return void
	 * 
	 * @throws RuntimeError if no ants from colony present in nest site
	 * 
	 */
	public void decrementQuorumSize(AntColony colony, int numAnts, boolean active)
	{
		if (colony == null)
		{
			throw new IllegalArgumentException("NestSite.decrementQuorumSize called with colony == null");
		}
		ColonyQuorum quorum;

		mQuorumSize-= numAnts;
		quorum = (ColonyQuorum) mColonyQuorums.get(colony);
		if (quorum != null)
		{
			if (active)
			{
				if (quorum.mActiveQuorum >= numAnts)
				{
					quorum.mActiveQuorum -= numAnts;
				}
				else if (quorum.mActiveQuorum < numAnts)
				{
					throw new IllegalArgumentException("NestSite.decrementQuorumSize called with numAnts > NestSite.getQuorumSize(colony, true) (numAnts == " + numAnts + ")");
				}
			}
			else
			{
				if (quorum.mPassiveQuorum >= numAnts)
				{
					quorum.mPassiveQuorum -= numAnts;
				}
				else if (quorum.mPassiveQuorum < numAnts)
				{
					throw new IllegalArgumentException("NestSite.decrementQuorumSize called with numAnts > NestSite.getQuorumSize(colony, false) (numAnts == " + numAnts + ")");
				}
			}
			if (quorum.mActiveQuorum == 0 && quorum.mPassiveQuorum == 0)
			{
				mColonyQuorums.remove(colony);
			}
		}
		else
		{
			throw new RuntimeException("NestSite.decrementQuorumSize: no ants from colony present in nest site");
		}
	}
	
	/**
	 * Gets the site's quality
	 * 
	 * @return site's quality (int > 0)
	 * 
	 */
	public abstract int getSiteQuality();

	/**
	 * Sets whether the nest site is habitable or not
	 * 
	 * @param isHabitable (true if the nest site is habitable, false otherwise)
	 * 
	 * @return void
	 * 
	 */
	public void setHabitable(boolean isHabitable)
	{
		mIsHabitable = isHabitable;
	}

	/**
	 * Adds a nest site to the list of known neighbours
	 * 
	 * @param neighbour (!= null)
	 * @param distance (>= 0)
	 * 
	 * @return void
	 * 
	 */
	public void addNeighbouringNestSite(NestSite neighbour, int distance)
	{
		if (neighbour == null)
		{
			throw new IllegalArgumentException("NestSite.addNeighbouringNestSite called with neighbour == null");
		}
		if (distance < 0)
		{
			throw new IllegalArgumentException("NestSite.addNeighbouringNestSite called with distance < 0 (distance == " + distance + ")");
		}
		Neighbour existingNeighbour;
		Iterator i;
		// check if nest site is already a known neighbour
		i = mNeighbours.iterator();
		while (i.hasNext())
		{
			existingNeighbour = (Neighbour) i.next();
			if (existingNeighbour.mNeighbour.equals(neighbour))
			{
				throw new IllegalArgumentException("NestSite.addNeighbouringNestSite called with neighbour already in list of neighbours");
			}
		}
		
		mNeighbours.add(new Neighbour(neighbour, distance));
		mTotalNeighbourDistance += distance;
	}

	/** 
	 * Gets a random neighbouring NestSite
	 *
	 * @param distanceAffectsDiscovery (true indicates that distance to neighbour affects chances of discovery)
	 *  
	 * @return random neighbouring NestSite
	 * 
	 */
	public NestSite getRandomNeighbour(boolean distanceAffectsDiscovery)
	{
		Neighbour neighbour;
		Iterator i1;
		int randomInt, totalDistance = 0;

		if (distanceAffectsDiscovery)
		{
			// TODO: should getRandomNeighbur be able to return null, e.g. if there are only very distant neighbouring nests?
			// weight probability of discovery of each neighbour by its distance
			randomInt = Random.uniform.nextIntFromTo(0, mTotalNeighbourDistance - 1);
			i1 = mNeighbours.iterator();
			while (i1.hasNext())
			{
				neighbour = (Neighbour) i1.next();
				if (randomInt >= totalDistance && randomInt < totalDistance + (mTotalNeighbourDistance - neighbour.mDistance))
				{
					return neighbour.mNeighbour;
				}
				totalDistance += (mTotalNeighbourDistance - neighbour.mDistance);
			}
			
			throw new RuntimeException("NestSite.getRandomNeighbour: Weighted probability calculation failed");
		}
		else
		{
			// sampled neighbour drawn from uniform distribution over all neighbours
			neighbour = (Neighbour) mNeighbours.get(Random.uniform.nextIntFromTo(0, mNeighbours.size() - 1));
		}
		
		return neighbour.mNeighbour;
	}

	/**
	 * Gets the distance to the specified neighbour
	 * 
	 * @param neighbour (!= null)
	 * 
	 * @return distance to neighbour
	 * 
	 */
	public int getDistanceToNeighbour(NestSite neighbour)
	{
		if (neighbour == null)
		{
			throw new IllegalArgumentException("NestSite.getDistanceToNeighbour called with neighbour == null");
		}
		Iterator i1;
		Neighbour n;
		
		i1 = mNeighbours.iterator();
		while (i1.hasNext())
		{
			n = (Neighbour) i1.next();
			if (n.mNeighbour.equals(neighbour))
			{
				return n.mDistance;
			}
		}
		
		throw new IllegalArgumentException("NestSite.getDistanceToNeighbour called with non-neighbouring nest site");
	}
		
	/** Determines if the nest site is habitable or not
	 * 
	 * @return true if nest site is habitable, false otherwise
	 * 
	 */
	public boolean isHabitable()
	{
		return mIsHabitable;
	}
	
	/**
	 * NestSite constructor
	 *
	 */
	public NestSite()
	{
		mIsHabitable = true;
		mQuorumSize = 0;
		mColonyQuorums = new HashMap();
		mNeighbours = new ArrayList();
		mTotalNeighbourDistance = 0;
	}
}

/*********************************************************************************************
* History:
* $Log: NestSite.java,v $
* Revision 1.3  2005/01/04 14:03:55  marshall
* Improved header style
*
* Revision 1.2  2004/05/07 16:55:27  marshall
* Added single comment
*
* Revision 1.1  2004/04/19 13:28:33  marshall
* First imported version
*
* 
*********************************************************************************************/