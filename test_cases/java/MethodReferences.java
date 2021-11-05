import java.util.Arrays;

public class MethodReferences {

    String fld;
    MethodReferences(String s){
	fld = s;
    }

    String concat(String s){
	return fld + s;
    }
    
    public static void main(String[] args) {
	// Per https://docs.oracle.com/javase/tutorial/java/javaOO/methodreferences.html,
	// there are four kinds of method reference:
	// 1. reference to a static method
	Arrays.stream(args).map(Integer::valueOf);
	// 2. reference to an instance method of a particular object
	MethodReferences mr = new MethodReferences("foobar");
	Arrays.stream(args).map(mr::concat);
	// 3. Reference to an instance method of an arbitrary object of a particular type
	Arrays.sort(args, String::compareToIgnoreCase);
	// 4. Reference to a constructor
	Arrays.stream(args).map(MethodReferences::new);
    }
}
