var x;
x = 10;
var y = 3 * x * 5;
if (x > y)
  return x;
else if (x * x > y)
  return x * x;
else if (x * (x + x) > y) {
  var z = y + x - 3;
  return x * (x + x);
}
else 
  return y - 1;  
