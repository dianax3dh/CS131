class SynchronizedState implements State {
    /*private variables*/
    private byte[] value; /*array of bytes called value*/
    private byte maxval; /*byte variable called maxval*/
    
    /*byte array called v*/
    SynchronizedState(byte[] v) { value = v; maxval = 127; }
  
    SynchronizedState(byte[] v, byte m) { value = v; maxval = m; }

    public int size() { return value.length; }

    public byte[] current() { return value; }

    public synchronized boolean swap(int i, int j) {
	if (value[i] <= 0 || value[j] >= maxval) {
	    return false;
	}
	value[i]--;
	value[j]++;
	return true;
    }
}
