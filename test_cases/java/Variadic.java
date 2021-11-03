class Variadic {
    static int sum(int... xs){
	int result = 0;
	for (int i = 0; i < xs.length ; i++)
	    result += xs[i];
	return result;
    }

    public static void main(String[] args) {
	int zero = sum();
	int one = sum(1);
	int three = sum(1,2);
	int ten = sum(1,2,3,4);
	System.out.println("zero: " + zero);
	System.out.println("one: " + one);
	System.out.println("three: " + three);
	System.out.println("ten: " + ten);
    }
}
