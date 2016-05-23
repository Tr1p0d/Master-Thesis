package ec.iphashing;
import ec.util.*;
import ec.*;
import ec.gp.*;

public class IntData extends GPData
    {
    // here we work with 64bit int
    // each operation rounds it to the 32bit
    public int x;    

    public void copyTo(final GPData gpd)   // copy my stuff to another DoubleData
        { ((IntData)gpd).x = x; }
    }
