var a = [1];
var b = [8];
var c = [10];

var obj_array = [a,a,b,c];
var num_array = [1,8,8,8,10,10];
var test = [0];

test[0] = 8;
test[0] = 10;
//expect(buckets.arrays.indexOf(customObjectArray, test, eq)).toEqual(3);

var array = obj_array;
var item = test;

var length = array.length;

var i = 0;

while (i < length) {
    if (array[i][0] == item[0]) {
	return i;
    }
    i += 1;
}
return -1;
