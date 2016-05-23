/* marek kidon, xkidon00@stud.fit.vutbr.cz master thesis
   this module defines the ipreader and parser to for
   iphasning. it is used among all used approaches.
*/
package ec.iphashing;
import ec.util.*;
import ec.*;
import ec.gp.*;
import ec.gp.koza.*;
import ec.simple.*;

import java.io.*;
import java.util.*;

public class IPReader {
    public List<IPData> read(String file) {
        try {
            BufferedReader br = new BufferedReader(new FileReader(file));

            String line;
            List<IPData> data = new ArrayList<IPData>();
            while ((line = br.readLine()) != null) {
                data.add(new IPData(line));
            }
            return data;
        } catch (FileNotFoundException e) {
            System.out.println("Cannot open file");
            System.exit(1);
        } catch (IOException e) {
            System.out.println("Cannot read file");
            System.exit(1);
        }
        return new ArrayList();
    }
}
