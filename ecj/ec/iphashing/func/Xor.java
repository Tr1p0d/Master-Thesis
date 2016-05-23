package ec.iphashing.func;
import ec.*;
import ec.gp.*;
import ec.util.*;

import ec.iphashing.*;

public class Xor extends GPNode
    {
    public String toString() { return "xor"; }

    public int expectedChildren() { return 2; }

    public void eval(final EvolutionState state,
        final int thread,
        final GPData input,
        final ADFStack stack,
        final GPIndividual individual,
        final Problem problem)
        {
        long result;
        IntData rd = ((IntData)(input));

        children[0].eval(state,thread,input,stack,individual,problem);
        result = Integer.toUnsignedLong(rd.x);

        children[1].eval(state,thread,input,stack,individual,problem);
        rd.x =(int) ((result ^ rd.x) & 0xFFFFFFFFL);
        }
    }
