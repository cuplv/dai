import java.io.FileOutputStream;
import java.io.IOException;

class TryWithResources {
    public static void main(String[] args) {
	try(FileOutputStream fos = new FileOutputStream("test_outputfile")) {
	    fos.write("Hello, world!".getBytes());
	} catch (IOException e) {
	    System.out.println("got an IOException");
	} finally {
	    System.out.println("in finally block");
	}

    }
}
