var x = true;
var y = 10;
if ((x && false) || (x && y > 100)) {
    var x = 3;
    return x + y;
}
else {
    var y = false;
    return x == (y && (y != true));
}

return x;
return y;
return x - y;
