4+5;

def foo(a b) a*a + 2*a*b + b*b;

def bar(a) foo(a, 4.0) + bar(31337);

foo(3, 2);
bar(1);