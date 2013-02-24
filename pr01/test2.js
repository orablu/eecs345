var x;
x = 10;
var y = 3 * x * 5;
var z = y / (x - 3);
if (z >= 0)
  if (x <= y + z)
    if (x == x + 1)
      z = 0;
    else
      z = 1;
  else
    z = 2;
else
  z = 3;
if (x > y)
  return x + z;
else if (x * x > y)
  return x * x + z;
else if (x * (x + x) > y)
  return x * (x + x) + z;
return y - 1 + z;
