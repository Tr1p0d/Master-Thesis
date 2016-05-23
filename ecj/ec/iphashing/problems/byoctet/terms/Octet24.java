package ec.iphashing.problems.byoctet.terms;
import ec.*;
import ec.gp.*;
import ec.util.*;

import ec.iphashing.*;
import ec.iphashing.problems.byoctet.*;

public class Octet24 extends GPNode
    {
    public String toString() { return "o3"; }

    public int expectedChildren() { return 0; }

    public void eval(final EvolutionState state,
                     final int thread,
                     final GPData input,
                     final ADFStack stack,
                     final GPIndividual individual,
                     final Problem problem)
        {
        IntData rd = ((IntData)(input));
        rd.x = ((IPHashing)problem).current24Octet;
        }
    }
