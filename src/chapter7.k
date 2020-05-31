# Define ':' for sequencing: as a low-precedence operator that ignores operands
# and just returns the RHS.
def binary : 1 (x y) y;

# Recursive fib, we could do this before.
def fib(x)
  if (x < 3) then
    1
  else
    fib(x-1)+fib(x-2);

# Iterative fib.
def fibi(x)
  var a = 1, b = 1, c = 0 in
  (for i = 3, i < x, 1.0 in 
    c = (a + b) : 
    a = b : 
    b = c) :
  b;

def testing(x)
  var y = x, z = 0 in
  (for i = 0, i < 0, 1.0 in
    y = (y + 1) ) :
  y;

def newtest(x)
  (while (x < 10) in
    x = (x + 1)) :
  x;

# Call it. 
newtest(310101);
