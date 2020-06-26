var a = [1];
var b = [8];
var c = [10];

var obj_array = [a,a,b,c];
var num_array = [1,8,8,8,10,10];

//expect(buckets.arrays.indexOf(numberArray, 1)).toEqual(0);

var array = num_array;
var item = 1;

var length = array.length;

var i = 0;
while (i < length) {
    if (array[i] == item) {
	return i;
    }
    i += 1;
}
return -1;

