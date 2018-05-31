/*uses volatile accesses to array elements.
implement with get:Returns the current value of the element at index i
and set:Sets the element at index i to newValue method*/
import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSet implements State {
	private byte maxval; /*byte variable called maxval*/
	private AtomicIntegerArray aaVal; /*atomicintegerarray called aaVal*/

	/*creates an atomicintegerarray with same length and 
	same elements*/
	public void createIntArray(byte[] v){
		int[] intArray = new int[v.length]; 
		for (int i = 0; i < v.length; i++){
			intArray[i] = v[i];
		}

		aaVal = new AtomicIntegerArray(intArray); 
	}

	GetNSet(byte[] v) {
		createIntArray(v);
		maxval = 127; 
	}

	GetNSet(byte[] v, byte m){
		createIntArray(v);
		maxval = m; 
	}

	/*gets size of array*/
    public int size() { return aaVal.length(); }

    /*gets current value*/
    public byte[] current() { 
    	byte[] currnt = new byte[aaVal.length()]; /*create byte array to put 
    	back element from when it was put into an int array*/
    	for (int i = 0; i < currnt.length; i++)
    	{
    		currnt[i] = (byte) aaVal.get(i); /*makes sure its a byte since
    		get is get(int i)*/
    	}
    	return currnt; 
    }

    public boolean swap(int i, int j) {
	if (aaVal.get(i) <= 0 || aaVal.get(j) >= maxval) {
	    return false;
	}
	aaVal.set(i, aaVal.get(i)-1);
	aaVal.set(j, aaVal.get(j)+1);
	return true;
    }

}

