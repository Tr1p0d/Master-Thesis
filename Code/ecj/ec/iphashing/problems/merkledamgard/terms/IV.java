package ec.iphashing.problems.merkledamgard.terms;
import ec.*;
import ec.gp.*;
import ec.util.*;

import ec.iphashing.*;
import ec.iphashing.problems.merkledamgard.*;

public class IV extends GPNode
    {
    public String toString() { return "IV"; }

    public int expectedChildren() { return 0; }

    public void eval(final EvolutionState state,
                     final int thread,
                     final GPData input,
                     final ADFStack stack,
                     final GPIndividual individual,
                     final Problem problem)
        {
        IntData rd = ((IntData)(input));
        }
    }
