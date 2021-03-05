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

// buckets.arrays.contains, modified to remove dynamic "equalsFunction" equality operator
function contains(array, item) {
    var tmp = indexOf(array,item);
    return tmp >= 0;
};

var numberArray = [1, 8, 8, 8, 10, 10];

//test cases "contains returns true for existing numbers"
var test1 = contains(numberArray, 1);
var test2 = contains(numberArray, 8);
var test3 = contains(numberArray, 10);

//test cases "contains returns false for non existing numbers"
var test4 = contains(numberArray, 11);
var test5 = contains([], 8);
