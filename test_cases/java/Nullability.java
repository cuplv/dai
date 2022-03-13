import java.util.Random;
// Some annotated test cases to understand the semantics of the nullability domain
public class Nullability {
    static class MyObj {
	MyObj f;
	MyObj g;

	void doThing() { /* skip */ };

	MyObj(MyObj ff, MyObj gg){
	    f = ff;
	    g = gg;
	}
    }
    
    public static void main(String[] args) {
	MyObj o1 = null;
	MyObj o2 = new MyObj(o1, null);
	// should have: { o1 -> NULL ; o2 -> NONNULL ; o2.f -> NULL ; o2.g -> NULL}

	foo(o1);
	foo(o2);

	// should have: { o1 -> NULL ; o2 -> NONNULL ; o2.f -> NULL ; o2.g -> NULL}

	bar(o2);

	// should have: { o1 -> NULL ; o2 -> NONNULL ; o2.f -> NULL ; o2.g -> NULL}

	MyObj o3 = new MyObj(o2, o2);

	// should have: { o1 -> NULL ; o2 -> NONNULL ; o2.f -> NULL ; o2.g -> NULL ; o3 -> NONNULL ; o3.f -> NONNULL ; o3.g -> NONNULL}

	bar(o1);

	// should have: bottom	
    }

    // assuming you ONLY pass the parameters' nullability and not their fields' nullability
    // at callsites, should have two summaries:
    // pre:{o -> NULL} post:{o -> NULL}
    // pre:{o -> NONNULL} post:{o -> NONNULL ; a -> TOP }
    // (let's do this for now, can look into passing fields' nullability if needed later)
    static void foo(MyObj o) {
	if (o != null) {
	    Object a = o.f;
	} else {
	    /*skip*/;
	}
    }

    //should have two summaries:
    // pre:{o -> NULL} post:bottom
    // pre:{o -> NONNULL} post:{o -> NONNULL}
    static void bar(MyObj o) {
	o.doThing();
    }
    
}
