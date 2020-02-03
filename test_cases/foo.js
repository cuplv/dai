var foo = 5;
var bar = 6.2;
var baz = null,
    quux = "hello world",
    cond = true;

if (cond){
    foo = foo + bar
} else if (cond) {
    bar = foo * bar
} else {
    foo += bar
}
