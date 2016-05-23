package ec.iphashing.problems.cuckoo.terms;
import ec.*;
import ec.gp.*;
import ec.util.*;

import ec.iphashing.*;
import ec.iphashing.problems.cuckoo.*;

public class Octet8 extends GPNode
    {
    public String toString() { return "8-16"; }

    public int expectedChildren() { return 0; }

    public void eval(final EvolutionState state,
                     final int thread,
                     final GPData input,
                     final ADFStack stack,
                     final GPIndividual individual,
                     final Problem problem)
        {
        IntData rd = ((IntData)(input));
        rd.x = ((Cuckoo)problem).current8Octet;
        }
    }
