public class NestedLoops {
    int foo (int x) {
	for (int i = 0, result = 0; i < x; i++)
	    for(int j = i; j > 0; j--)
		result += j;
	return result;
    }
    int baz (int x) {
	int result = 0, i=0;
	while (i < x) {
	    int j = 0;
	    while (j < i) {
		result += j;
		j++;
	    }
	    i++;
	}
    }
    int quux (int x) {
	int result = 0, i=0;
	do{
	    int j = 0;
	    do {
		result += j;
		j++;
	    } while (j < i);
	    i++;
	} while  (i < x);
    }
}
