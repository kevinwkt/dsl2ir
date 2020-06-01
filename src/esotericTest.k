# Define ':' for sequencing: as a low-precedence operator that ignores operands
# and just returns the RHS.
def binary : 1 (x y) y;

def newtest(x)
  var y = x in
    (y = (y * 10)): 
  y;

# Call it. 
newtest(28);
