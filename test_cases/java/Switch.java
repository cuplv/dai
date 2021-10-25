import java.util.Random;

class Switch {
    public static void main(String[] args) {
        Random r = new Random();
        int n = r.nextInt(3);
        switch (n) {
            case 0:
                System.out.println("Small number");
                break;
            case 1:
                System.out.println("Medium number");
                break;
            case 2:
                System.out.println("Big number");
        }
        switch (r.nextInt(10)) {
            case 0, 1:
                System.out.println("tiny");
            case 2, 3:
                System.out.println("number");
                break;
            case 4:
            case 5:
            default:
                System.out.println("middle number");
                break;
            case 9:
                System.out.println("biggest number");
        }
    }
}
