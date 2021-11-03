public class Break {
    public static void main(String[] args) {
outer: for (int i = 0; i < 3; i++) {
            System.out.println("Outer");

            for (int j = 0; j < 2; j++) {
                System.out.println("Inner");
                break outer;
            }
            
            System.out.println("Outer end");
        }

        System.out.println("Almost Done");
        System.out.println("Done");
    }
}

