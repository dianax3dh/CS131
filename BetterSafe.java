/*Design and implement a new class BetterSafe of your choice, which achieves 
better performance than Synchronized while retaining 100% reliability*/
/*use locks https://examples.javacodegeeks.com/core-java/util/concurrent
/locks-concurrent/reentrantlock/java-reentrantlock-example/ for understanding*/ 
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.locks.Lock;

class BetterSafe implements State {
    private byte[] value;
    private byte maxval;
    private final Lock rlock = new ReentrantLock(); 

    BetterSafe(byte[] v){
    	value = v;
    	maxval = 127;
    }
    BetterSafe(byte[] v, byte m){
    	value = v;
    	maxval = m; 
    }

    public int size() { 
    	return value.length; 
    }

    public byte[] current() { 
    	return value; 
    }

    public boolean swap(int i, int j) {
    rlock.lock(); //locks 
    try{
		if (value[i] <= 0 || value[j] >= maxval) {
		    return false;
		}	
	value[i]--;
	value[j]++;
	}
	finally{
		rlock.unlock();  //unlock
	}
	return true;
    }
}
