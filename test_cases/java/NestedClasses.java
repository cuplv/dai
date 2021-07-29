public class NestedClasses {
    class Inner {
	static int bar (int x) {
	    return x + x;
	}
    }
    int foo () {
	int y = 7;
	return Inner.bar(y);
    }
}
