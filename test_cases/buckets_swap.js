function swap(array, i, j) {
    var temp;

    if (i < 0 || i >= array.length || j < 0 || j >= array.length) {
        return false;
    }
    temp = array[i];
    array[i] = array[j];
    array[j] = temp;
    return true;
};

var numberArray = [1, 8, 8, 8, 10, 10];

// test cases "swap only accepts valid positions"
var test1 = swap(numberArray, 0, 5);
var test2 = swap(numberArray, 0, 6);
var test3 = swap(numberArray, 7, 2);
var test4 = swap(numberArray, -1, 9);
