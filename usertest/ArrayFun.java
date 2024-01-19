public class ArrayFun {
		static int sum(int x, int y) {
				return x + y;
		}

    public static void main(String[] args) {
				int[] arr = {42, 10, -5};
				int good = arr[0]+arr[1];
				int bad  = arr[999];
				int square = arr[1]*arr[1];
				int c = 0;
				//if (foo) {
					c = sum(good, arr[2]);
				/*}
				else {
				  c = sum(-good, arr[2]);
				}*/
				int d = c*c;
    }
}
