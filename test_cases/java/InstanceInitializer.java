class InstanceInitializer {
    // Test for interactions between various ways to initialize a class instance.
    // x will be 0 first, then set to 10 by initializer, then to 50 by constructor.

    int x = 0; // field with default value

    { // instance initializer block
	x = x + 10;
    }

    InstanceInitializer() { // constructor
	x = x * 5;
    }

    public static void main(String[] args) {
	InstanceInitializer ii = new InstanceInitializer();
	System.out.println("Done with instantiation; ii.x = " + ii.x);
    }
}
