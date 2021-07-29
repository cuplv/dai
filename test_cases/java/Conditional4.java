import java.util.Random;
class Conditional {
    public static void main(String[] args) {
	Random r = new Random ();
	if (r.nextInt(100) > 42) {
	    System.out.println("Hello, world!");
	} else {
	    System.out.println("Farewell, world!");
	}
    }
}
