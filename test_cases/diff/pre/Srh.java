package foo.bar;
// Example adapated from Sagiv,Reps,Horwitz '96
// "Precise interprocedural dataflow analysis with applications to constant propagation"
public class Srh {
    static int x = 0;
    public static void main(String[] args) {
	int n = 3;
	p(n);
	System.out.println(x);
    }
    static void p(int a) {
	if (a > 0) {
	    a -= 2;
	    p(a);
	    a += 2;
	}
	x = -2 * a + 5;
    }
}
    
