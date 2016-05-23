package ec.iphashing.problems.cuckoo;
import ec.util.*;
import ec.*;
import ec.gp.*;
import ec.gp.koza.*;
import ec.simple.*;

import ec.iphashing.*;

import java.util.*;

public class Cuckoo extends GPProblem implements SimpleProblemForm
{
    public static final String P_DATA = "data";

    public int current24Octet;
    public int current16Octet;
    public int current8Octet;
    public int current0Octet;

    public int a0;
    public int a1;
    public int a2;

    public final int maximumLoad = 8192;
    public final int halfLoad = maximumLoad / 2;
    public final int maxCuckooLoop = 15;

    public List<IPData> IPList;

    public void setup(final EvolutionState state,
                      final Parameter base)
        {
	IPReader reader = new IPReader();
	IPList = reader.read("data/cesnet_addrs_8192d");

        super.setup(state,base);
        if (!(input instanceof IntData))
            state.output.fatal("IntData class must subclass from " + IntData.class,
                base.push(P_DATA), null);
        }

    public void evaluate(final EvolutionState state,
                         final Individual ind,
                         final int subpopulation,
                         final int threadnum) {
        if (!ind.evaluated) {
	    IntData input = (IntData)this.input;

	    // setup initial table load, just a helper variable
	    int load = 0;

            // init our hash table
	    IPData[] t1 = new IPData[halfLoad];
            IPData[] t2 = new IPData[halfLoad];

	    for (IPData ip : IPList) {

		IPData currentIP = ip;
		IPData tmpIP;
		long index1, index2;
	        int[] octets;

		for(int x = 0; x < maxCuckooLoop; x++) {
	            octets = currentIP.getOctets();
	            current24Octet = octets[0];	
	            current16Octet = octets[1];	
	            current8Octet = octets[2];	
	            current0Octet = octets[3];	

                    ((GPIndividual)ind).trees[0].child.eval(
                        state,threadnum,input,stack,((GPIndividual)ind),this);
                    index1 = (Integer.toUnsignedLong(input.x)) % halfLoad;

		    if (t1[(int) index1] == null) {
                        t1[(int) index1] = currentIP;
                        load++;
                        break;
                    } else {
			tmpIP = currentIP;
                        currentIP = t1[(int)index1];     
                        t1[(int)index1] = tmpIP;
                    }

	            octets = currentIP.getOctets();
	            current24Octet = octets[0];	
	            current16Octet = octets[1];	
	            current8Octet = octets[2];	
	            current0Octet = octets[3];	

                    ((GPIndividual)ind).trees[1].child.eval(
                        state,threadnum,input,stack,((GPIndividual)ind),this);
                    index2 = (Integer.toUnsignedLong(input.x)) % halfLoad;

		    if (t2[(int)index2] == null) {
                        t2[(int)index2] = currentIP;
                        load++;
                        break;
                    } else {
			tmpIP = currentIP;
                        currentIP = t2[(int)index2];     
                        t2[(int)index2] = tmpIP;
                    }
		}
	    }
	    KozaFitness f = ((KozaFitness)ind.fitness);
	    f.setStandardizedFitness(state, (double) (maximumLoad - load) / maximumLoad);
	    f.hits = load;
	    ind.evaluated = true;
	}
    }
}
