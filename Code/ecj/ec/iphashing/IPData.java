/* Marek Kidon, xkidon00@stud.fit.vutbr.cz master thesis
   This module defines the ip address container.
*/

package ec.iphashing;
import ec.util.*;
import ec.*;
import ec.gp.*;
import ec.gp.koza.*;
import ec.simple.*;

import java.io.*;
import java.util.*;

public class IPData {
    private int ipAddress;
    private int[] octets;

    IPData(String address) {
        ipAddress = 0;
        octets = new int[4];
        String[] stringOctets = address.split("\\.");

    //System.out.println(address);
    //System.out.println(stringOctets[0]);
    //System.out.println(stringOctets.length);

        for (int i = 0 ; i < 4; i++) {
            // Put it in reverse so we gen least sifnificant first.
            octets[i] = Integer.parseInt(stringOctets[3-i]);

            ipAddress += (octets[i] << (i * 8));
        }
    }

    public int getIPAddress() {
        return this.ipAddress;
    }

    public int[] getOctets() {
        return octets;
    }

    public String toString() {
        return Integer.toString(octets[3]) + "."
            + Integer.toString(octets[2]) + "."
            + Integer.toString(octets[1]) + "."
            + Integer.toString(octets[0]) + "\n";
    }
}
