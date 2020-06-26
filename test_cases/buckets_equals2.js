var a = [1, 8, 8, 8, 10, 10],
    b = [1, 8, 8, 8, 10, 10];
//        expect(buckets.arrays.equals(a, b)).toBeTruthy();;

var array1 = a;
var array2 = b;

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
