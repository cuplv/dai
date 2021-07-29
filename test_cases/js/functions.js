function double(x) {
    return x + x;
}

function square(x) {
    var irrelevant_local = 42;
    return x * x;
}

var x = 42
var five = 5;
var twentyfive = square(5);
var fifty = double(twentyfive);
