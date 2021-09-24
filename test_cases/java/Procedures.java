public class Procedures {

    static class Inner {
	static int baz () { return 5; }
    }

    int n;

    Procedures(int nn) {
	n = nn;
    }

    int foo(int x) {
	return x * x + n;
    }

    int bar () {
	return 5 + n;
    }

    static int baz () {
	return 2 + Inner.baz();
    }

    int quux(int y) {
	return foo(y + bar()) * baz();
    }

    public static void main(String[] args){
	Procedures ps = new Procedures(10);
	int result = ps.quux(30);
	System.out.println(result);
    }
}
