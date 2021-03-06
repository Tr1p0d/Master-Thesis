/* Marek Kidon, xkidon00@stud.fit.vutbr.cz master thesis
   This module defines the complement operation for GP.
*/

package ec.iphashing.func;
import ec.iphashing.*;
import ec.*;
import ec.gp.*;
import ec.util.*;

public class Neg extends GPNode
    {
    public String toString() { return "~"; }

    public int expectedChildren() { return 1; }

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

        rd.x = (int) ((~result) & 0xFFFFFFFFL);
        }
    }
