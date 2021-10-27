public class SuperMethodInvocation {

    public void callSuper() {
        System.out.println(super.hashCode());
    }

    public static void main(String[] args) {
        SuperMethodInvocation obj = new SuperMethodInvocation();
        obj.callSuper();
    }
}
