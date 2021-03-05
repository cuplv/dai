// buckets.defaultEquals
function equalsFunction(a, b) {
    return a === b;
};

// buckets.arrays.equals, modified to remove dynamic "equalsFunction" equality and rewrite [for] to [while]
function equals (array1, array2) {
    var length = array1.length,
        i = 0;
    if (array1.length !== array2.length) {
        return false;
    }
//    for (i = 0; i < length; i += 1) {
//        if (!equalsFunction(array1[i], array2[i])) {
//            return false;
//        }
    //    }

    while ( i < length ) {
//	var tmp = equalsFunction(array1[i], array2[i]);
	if (array1[i] === array2[i]) {
	    return false;
	}
	i += 1;
    }
    return true;
};


//test cases "equals returns true for matching number arrays"
var a = [1, 8, 8, 8, 10, 10],
    b = [1, 8, 8, 8, 10, 10];

var test1 = equals(a, a);
var test2 = equals(a, b);

//test cases "equals returns false for non-matching number arrays"
var a = [1, 8, 8, 8, 10, 10],
    c = [1, 8, 5, 8, 10, 10],
    d = [1, 8, 8, 8, 10];

var test3 = equals(a, []);
var test4 = equals(a, c);
var test5 = equals(a, d);
var test6 = equals(a, []);

