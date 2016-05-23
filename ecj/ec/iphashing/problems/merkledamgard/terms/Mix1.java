package ec.iphashing.problems.merkledamgard.terms;
import ec.*;
import ec.gp.*;
import ec.util.*;

import ec.iphashing.*;
import ec.iphashing.problems.merkledamgard.*;

public class Mix1 extends GPNode
    {
    public String toString() { return "A1"; }

    public int expectedChildren() { return 0; }

    public void eval(final EvolutionState state,
                     final int thread,
                     final GPData input,
                     final ADFStack stack,
                     final GPIndividual individual,
                     final Problem problem)
        {
        IntData rd = ((IntData)(input));
        rd.x = ((MerkleDamgard)problem).a1;
        }
    }
