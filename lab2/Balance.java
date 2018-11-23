import java.math.*;
import java.util.Scanner;
import java.util.*;

public class Balance {

	public static void main(String[] args) {
		
		List<Integer> Weight = new ArrayList<Integer>();
		List<Integer> Left = new ArrayList<Integer>();
		List<Integer> Right = new ArrayList<Integer>();
		int N;
		BigInteger W;
		
		int length=0;
		
		try{
			Scanner reader = new Scanner(System.in);
			
			N = reader.nextInt();
			W = reader.nextBigInteger();
			reader.close();
		}
		catch (Exception ex){
			System.out.printf("Mufa\n");
			ex.printStackTrace();
			return;
		}
		BigInteger THREE = BigInteger.valueOf(3);
		while (W.compareTo(BigInteger.ZERO) == 1){
			length++;
			Weight.add(W.mod(THREE).intValue());
			W = W.divide(THREE);
		}
		
		int i = 0;
		int bit = 0;
		while(i<length){
			int r = Weight.get(i);
			if (bit == 0) {
				if (r==0) {
					bit = 0;
				}
				else if (r==1) {
					bit = 0;
					Right.add(i+1);
				}
				else{
					bit = 1;
					Left.add(i+1);
				}
			}
			else {
				if (r==0) {
					bit = 0;
					Right.add(i+1);
				}
				else if (r==1) {
					bit = 1;
					Left.add(i+1);
				}
				else{
					bit = 1;
				}
			}
			i++;
		}
		if (bit == 1) {
			Right.add(i+1);
			i++;
		}
		if (i <= N) {
			System.out.printf("[");
			int j = 0;
			while(j<Left.size()){
				System.out.printf("%d", Left.get(j));
				j++;
				if (j < Left.size()) System.out.printf(",");
			}
			System.out.printf("] [");
			j = 0;
			while(j<Right.size()){
				System.out.printf("%d", Right.get(j));
				j++;
				if (j < Right.size()) System.out.printf(",");
			}
			System.out.printf("]\n");
		}
		else {
			System.out.printf("[] []\n");
		}
		
		return;
		
	}

}
