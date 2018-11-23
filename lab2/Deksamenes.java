import java.util.*;
import java.math.*;
import java.io.*;

public class Deksamenes {
	
	public static void main(String[] args) {
		
		
		int N=0;
		BigInteger V = BigInteger.valueOf(0);
		List<Tuple> dex = new ArrayList<Tuple>();
		
		
		try {
			FileReader fr = new FileReader(args[0]);
			BufferedReader br = new BufferedReader (fr);
			Scanner reader = new Scanner(br);
			
			
			N = reader.nextInt();
						
			for(int j=0; j<N; j++){
				int B = reader.nextInt();
				int H = reader.nextInt();
				int W = reader.nextInt();
				int L = reader.nextInt();
				
				dex.add(new Tuple(B, W*L));
				dex.add(new Tuple(B+H, -W*L));
			}
			
			V=reader.nextBigInteger();
			
			reader.close();
		}
		catch (Exception ex) {
			ex.printStackTrace();
			return;
	    }
		
		Collections.sort(dex, new TupleComparator());
		
		int i = 0;
		BigInteger h, e, hlast=BigInteger.valueOf(0);
		BigInteger L = BigInteger.valueOf(0);
		while(i < 2*N){
			h = BigInteger.valueOf(dex.get(i).getH());
			e = BigInteger.valueOf(dex.get(i).getE());
			if ((L.multiply(h.subtract(hlast))).compareTo(V) == -1) {
				V = V.subtract(L.multiply(h.subtract(hlast)));
				L = L.add(e);
				hlast = h;
			}
			else{
				if (L.compareTo(BigInteger.valueOf(0)) == 0){
					System.out.printf("%.2f\n", hlast);
				}
				else{
					try{
						V = (V.multiply(BigInteger.valueOf(1000))).divide(L);
					
						if (V.mod(BigInteger.TEN).compareTo(BigInteger.valueOf(5))==-1) {
							V = V.divide(BigInteger.TEN);
						}
						else {
							V = V.divide(BigInteger.TEN).add(BigInteger.ONE);
						}
						System.out.printf("%.2f", hlast.intValue()*1.0+V.intValue()/100.0);
					}
					catch (Exception ex){
						System.out.printf("Divide with 0.");
					}
				}
				return;
			}
			i++;
		}
		
		if (V.compareTo(BigInteger.valueOf(0))==1) System.out.println("Impossible");
		else System.out.printf("%.2f\n", dex.get(2*N-1).getH()*1.0);
		return;
		
	};
	
}