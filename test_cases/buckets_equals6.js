var a = [1, 8, 8, 8, 10, 10],
    c = [1, 8, 5, 8, 10, 10],
    d = [1, 8, 8, 8, 10];

// expect(buckets.arrays.equals(a, [])).toBeFalsy();

var array1 = a;
var array2 = [];
    
var length = array1.length;

if (array1.length != array2.length) {
    return false;
}
var i = 0;
while (i < length) {
    if (array1[i] != array2[i]) {
        return false;
    }
    i += 1;
}
return true;
