var x = 0;
var y = 0;
var z = 0;
var b;

while (true) {
    b = true;
    x = x + 1;
    while (true) {
        y = y + 1;

        if (y % 2 == 0) {
            continue;
        }
        
        if (b && y > 3) {
            b = false;
            break;
        }
    }
    
    if (b == false) {
        continue;
    }

    z = z + 1;

    if (z > 10) {
        return 10000 * x + 100 * y + z;
    }
}
