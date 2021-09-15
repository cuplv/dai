class Exceptions {
    static class MyError extends Throwable {}

    static int foo(int x) {
	try {
	    x = baz(x);
	} catch (MyError e) {
	    x = 100;
	} finally {
	    x = x + 5;
	}
	return x;
    }

    static int bar(int y) {
	try{
	    return baz(y);
	}
	catch(MyError e) {
	    return 100;
	}
    }

    static int baz (int z) throws MyError {
	if (0 <= z && z < 10) return z;
	else throw new MyError();
    }

    public static void main (String[] args) {
	int a = foo(5);
	int b = bar(a);
	int c = foo(b);
	int d = bar(c);
    }
}
