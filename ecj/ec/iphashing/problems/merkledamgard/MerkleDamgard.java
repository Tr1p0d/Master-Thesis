package ec.iphashing.problems.merkledamgard;
import ec.util.*;
import ec.*;
import ec.gp.*;
import ec.gp.koza.*;
import ec.simple.*;

import ec.iphashing.*;

import java.util.*;

public class MerkleDamgard extends GPProblem implements SimpleProblemForm
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

    public List<IPData> IPList;

    public void setup(final EvolutionState state,
                      final Parameter base)
        {
	IPReader reader = new IPReader();
	IPList = reader.read("data/cesnet_addrs_8192a");

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
            int collisions = 0;

	    Set loaded = new TreeSet<Integer>();
	    for (IPData ip : IPList) {
	        int[] octets = ip.getOctets();
                int prevSize = loaded.size();

	        current24Octet = octets[0];	
	        current16Octet = octets[1];	
	        current8Octet = octets[2];	
	        current0Octet = octets[3];	

                ((GPIndividual)ind).trees[0].child.eval(
                    state,threadnum,input,stack,((GPIndividual)ind),this);

                // BLOCK 0 OUTPUT
                a0 = input.x;

                ((GPIndividual)ind).trees[1].child.eval(
                    state,threadnum,input,stack,((GPIndividual)ind),this);

                // BLOCK 1 OUTPUT
                a1 = input.x;

                ((GPIndividual)ind).trees[2].child.eval(
                    state,threadnum,input,stack,((GPIndividual)ind),this);

                // BLOCK 2 OUTPUT
                a2 = input.x;

                ((GPIndividual)ind).trees[3].child.eval(
                    state,threadnum,input,stack,((GPIndividual)ind),this);

                // Insert into the table;
	        loaded.add((Integer.toUnsignedLong(input.x)) % maximumLoad);
	    }
	    //System.out.println(loaded);
	    //System.exit(1);
	    collisions = maximumLoad - loaded.size();
	    KozaFitness f = ((KozaFitness)ind.fitness);
	    f.setStandardizedFitness(state, (double) collisions / maximumLoad);
	    f.hits = maximumLoad - collisions;
	    ind.evaluated = true;
	}
    }
}
