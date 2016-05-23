/* Marek Kidon, xkidon00@stud.fit.vutbr.cz master thesis
   This module defines the right rotation operation for GP.
*/

package ec.iphashing.func;
import ec.iphashing.*;
import ec.*;
import ec.gp.*;
import ec.util.*;

public class RotateRight extends GPNode
    {
    public String toString() { return "rotateR"; }

    public int expectedChildren() { return 2; }

    public void eval(final EvolutionState state,
        final int thread,
        final GPData input,
        final ADFStack stack,
        final GPIndividual individual,
        final Problem problem)
        {
        int result;
        IntData rd = ((IntData)(input));

        children[0].eval(state,thread,input,stack,individual,problem);
        result = rd.x;

        children[1].eval(state,thread,input,stack,individual,problem);
	// rotate right
        rd.x = (result >>> rd.x) | (result << (32 - rd.x));
        }
    }
