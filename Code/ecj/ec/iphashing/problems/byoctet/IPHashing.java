/* Marek Kidon, xkidon00@stud.fit.vutbr.cz, Master Thesis
 * This modulce contains the first Direct design method presented
 * in the thesis. 
 */

package ec.iphashing.problems.byoctet;
import ec.util.*;
import ec.*;
import ec.gp.*;
import ec.gp.koza.*;
import ec.simple.*;

import ec.iphashing.*;

import java.util.*;

public class IPHashing extends GPProblem implements SimpleProblemForm
{
    public static final String P_DATA = "data";

    public int current24Octet;
    public int current16Octet;
    public int current8Octet;
    public int current0Octet;
    public final int maximumLoad = 8192;

    public List<IPData> IPList;

    public void setup(final EvolutionState state,
                      final Parameter base)
        {
	IPReader reader = new IPReader();
	// This is where you can change the desired cesnet dataset
	IPList = reader.read("data/cesnet_addrs_8192a");
	//IPList = reader.read("data/cesnet_addrs_8192b");
	//IPList = reader.read("data/cesnet_addrs_8192c");
	//IPList = reader.read("data/cesnet_addrs_8192d");

        super.setup(state,base);
        if (!(input instanceof IntData))
            state.output.fatal("IntData class must subclass from " + IntData.class,
                base.push(P_DATA), null);
        }

    // The evaluation function
    public void evaluate(final EvolutionState state,
                         final Individual ind,
                         final int subpopulation,
                         final int threadnum) {
        if (!ind.evaluated) {
	    IntData input = (IntData)this.input;
            int collisions = 0;

            // This is the fitness function.
	    Set loaded = new TreeSet<Integer>();
	    for (IPData ip : IPList) {
	        int[] octets = ip.getOctets();

	         current24Octet = octets[0];	
	         current16Octet = octets[1];	
	         current8Octet = octets[2];	
	         current0Octet = octets[3];	

                ((GPIndividual)ind).trees[0].child.eval(
                    state,threadnum,input,stack,((GPIndividual)ind),this);

	        loaded.add((Integer.toUnsignedLong(input.x)) % maximumLoad);
	    }
	    collisions = maximumLoad - loaded.size();
	    KozaFitness f = ((KozaFitness)ind.fitness);
	    f.setStandardizedFitness(state, (double) collisions / maximumLoad);
	    f.hits = maximumLoad - collisions;
	    ind.evaluated = true;
	}
    }
}
