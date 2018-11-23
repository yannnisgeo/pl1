import java.util.Comparator;

public class TupleComparator implements Comparator<Tuple>{
	@Override
	public int compare(Tuple A, Tuple B){
		return A.getH()-B.getH();
	};
}
