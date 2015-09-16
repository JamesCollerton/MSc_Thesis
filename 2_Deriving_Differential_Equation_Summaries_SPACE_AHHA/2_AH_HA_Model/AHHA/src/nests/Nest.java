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
* $Date: 2005/01/04 14:01:52 $
* $Revision: 1.3 $ 
*
*********************************************************************************************/
 
package nests;

import ahha.*;
import uchicago.src.sim.util.Random;

/**
 * The nest class for the AH-HA nest selection model
 *  
 */
public class Nest extends NestSite 
{
	/** The quality of the nest site */
	private int mSiteQuality;
	/** The standard deviation of the noise in measurements of quality of the nest site */
	private int mSiteStdDev;
	
	/**
	 * Gets the nest site's quality (with noise)
	 * 
	 * @return nest site's quality (with noise, >= 1)
	 * 
	 */
	public int getSiteQuality()
	{
		int sampledSiteQuality;
		
		sampledSiteQuality = mSiteQuality + (int) Random.normal.nextDouble(0, mSiteStdDev);
		if (sampledSiteQuality <= 0)
		{
			sampledSiteQuality = 1;
		}
		
		return sampledSiteQuality;
	}
	
	/**
	 * Nest constructor
	 * 
	 * @param siteQuality (>= 0)
	 * @param siteStdDev (>= 0)
	 * 
	 */
	public Nest(int siteQuality, int siteStdDev)
	{
		if (siteQuality < 0)
		{
			throw new IllegalArgumentException("Attempt to construct Nest with siteQuality < 0 (siteQuality == " + siteQuality + ")");
		}
		if (siteStdDev < 0)
		{
			throw new IllegalArgumentException("Attempt to construct Nest with siteStdDev < 0 (siteStdDev == " + siteStdDev + ")");
		}

		mSiteQuality = siteQuality;
		mSiteStdDev = siteStdDev;
	}
}
/*********************************************************************************************
* History:
* $Log: Nest.java,v $
* Revision 1.3  2005/01/04 14:01:52  marshall
* Improved header style and added footer
*
*
* 
*********************************************************************************************/