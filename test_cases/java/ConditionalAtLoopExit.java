class ConditionalAtLoopExit {

    public static void main(String[] args) {
	int x = 10;
	while (x > 0) {
	    x = x - 1;
	    if (x % 2 == 0)
		System.out.println("I'm even!");
	    else
		System.out.println("I'm odd!");
	}
    }
}
