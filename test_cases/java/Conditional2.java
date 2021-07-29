import java.util.Random;

class Conditional {
    public static void main(String[] args) {
	Random r = new Random ();
	if (r.nextBoolean()) {
	    System.out.println("Hello, beautiful world!");
	} else {
	    System.out.println("Farewell, world!");
	}
    }
}
