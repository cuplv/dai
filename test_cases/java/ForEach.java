import java.util.ArrayList;

public class ForEach {
    public static void main(String[] args) {
        ArrayList<Integer> myList = new ArrayList();
        myList.add(1);
        myList.add(2);
        myList.add(3);
        for (int num : myList) {
            System.out.println(num);
        }
        System.out.println("Done");
    }
}
