// buckets.arrays.indexOf, modified to remove dynamic "equalsFunction" equality operator and rewrite [for] to [while]
function indexOf(array, item) {
    var length = array.length,
	i = 0;
//    for (i = 0; i < length; i += 1) {
//	if (equalsFunction(array[i], item)) {
//	    return i;
//	}
//    }
    while ( i < length ) {
	if (array[i] === item) {
	    return i;
	}
	i += 1;
    }
    return -1;
};

// Using singleton arrays instead of js objects, since we don't have objects in our toy language
var a = [1];
var b = [8];
var c = [10];
var customObjectArray = [a, a, b, c];
var numberArray = [1, 8, 8, 8, 10, 10];

// test cases "indexOf gives the right index for valid numbers"
var test1 = indexOf(numberArray, 1);
var test2 = indexOf(numberArray, 8);
var test3 = indexOf(numberArray, 10);

// test cases "indexOf returns -1 when not found in number array"
var test4 = indexOf(numberArray, 11);
var test5 = indexOf([], 8);

// test cases "indexOf with custom equals gives the right index for valid objects"
var test = [1];
var test6 = indexOf(customObjectArray, test);
test[0] = 8;
var test7 = indexOf(numberArray, test);
test[0] = 10;
var test8 = indexOf(numberArray, test);
