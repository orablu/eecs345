var x = 0;
var i = 0;
while (true) {
    if (i % 2 == 0) {
        i = i + 1;
        continue;
    }
    x = x + 15;
    i = i + 1;
    if (i >= 3)
        break;
}

if (x == 0)
    return -i;

return x;
