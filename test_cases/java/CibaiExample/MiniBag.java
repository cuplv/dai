class MiniBag {
    private int[] elements;
    private int top;

    class BoundError extends Throwable {}
    
    MiniBag(int initial) {
	top = 0;
	elements = new int[initial];
    }

    int remove () throws BoundError {
	int r;
	if (top > 0 ) {
	    top--;
	    r = elements[top];
	} else throw new BoundError();
	return r;
    }

    void add(int i) {
	if (top < elements.length) {
	    elements[top] = i;
	    top++;
	} else throw new BoundError();
    }
    void removeMinimum() {
	if(top > 0) {
	    int min = 0, i;
	    for(i=1;i<top;i++)
		if(elements[min] > elements[i])
		    min = i;
	    elements[min] = elements[i=1]; top--;
	}
    }
}
