/* Marek Kidon, xkidon00@stud.fit.vutbr.cz master thesis
   This module defines multiplication operation for GP.
*/

package ec.iphashing.func;
import ec.*;
import ec.gp.*;
import ec.util.*;

import ec.iphashing.*;

public class Mul extends GPNode
    {
    public String toString() { return "(*)"; }

    public int expectedChildren() { return 2; }

    public void eval(final EvolutionState state,
        final int thread,
        final GPData input,
        final ADFStack stack,
        final GPIndividual individual,
        final Problem problem)
        {
            long result1;
            long result2;
            IntData rd = ((IntData)(input));

            children[0].eval(state,thread,input,stack,individual,problem);
            result1 = Integer.toUnsignedLong(rd.x);

            children[1].eval(state,thread,input,stack,individual,problem);
            result2 = Integer.toUnsignedLong(rd.x);

            rd.x = (int) ((result1 * result2) & 0xFFFFFFFFL);
        }
    }
