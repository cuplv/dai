public class FieldInitializers {
    int x = 5;

    Object o1 = new Object();
    Object o2;
    static Object o3 = new Object();
    static Object o4;

    FieldInitializers () {
	o2 = new Object();
    }

    static { o4 = new Object(); }
}
