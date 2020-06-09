# Threesum (dsl2ir)  
Compiler for a new Esoteric Language written in Haskell.

### Authors

Axel Zuchovicki A01022875  
Kyungtak Woo A01372055  
Eric Parton A01023503

### How to Run

1. Before starting, you should install [LLVM 4.0](https://github.com/llvm-hs/llvm-hs/blob/llvm-4/README.md#installing-llvm), [GHC 7.8](https://www.haskell.org/ghc/download_ghc_7_6_1.html), and [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)  

2. Navigate to the project directory in your terminal  

3. Run `stack build`  

4. You can now run files using our compiler with the following command `stack exec dsl2ir-exe <file path and name>`  
ex: `stack exec dsl2ir-exe helloWorld`, if you have a file called helloWorld within your project directory.  
Alternatively, you can simply run  `stack exec dsl2ir-exe`  to permit individual command line instructions (thanks JIT compilation)

### Introduction
The challenge was to use a purely functional programming language and its benefits to implement a compiler from the lexer and parser all the way to the intermediate representation (IR) utilized by LLVM. There are multiple reasons to why we want to produce LLVM IR but some of them could be the optimization passes that LLVM offers, the wide range of Backends that LLVM supports and can generate code for, along with a complete toolchain to analyze, experiment and optimize the back end for. It is important to remember that LLVM IR uses “unlimited single-assignment register machine instruction set”. This means that despite CPUs having a fixed number of registers, LLVM IR has an infinite number and new registers are created to hold the result of every instruction. This also leads us to use Static Single Assignment (SSA) as registers may be assigned to only once which may cause a lot of redundant memory operations but this is solved by the use of Scalar Replacement of Aggregates (SROA) to clean it up.  

Taking inspiration from esoteric languages such as Brainfuck and LOLCODE, we have also added some esoteric touches of our own to Threesum.  These deviations are described in the Esoteric Components section of this document.

### Threesum Language

#### Esoteric Components

Before viewing the language definitions and examples, keep in mind that Threesum has two differences when compared to the vast majority of existing programming languages:  

* Numbers that can be interpreted as base-3 will be.  For example:
```
User input => Base-10 value that is interpreted  
1          => 1  
5          => 5  
12         => 5  
122        => 17  
123        => 123  
1000       => 27  
1006       => 1006  
```

* The traditional addition and subtraction symbols have been switched, as have the addition and multiplication symbols.  For example:
```  
Input => Output  
3 + 4 => -1  
3 - 4 => 7  
4 * 4 => 1  
4 / 4 => 16  
```  

* Most of the reserved words have been replaced by sequences of underscores (_) of varying lengths.  This is described in more detail in the Reserved Words section.  
<br>

#### Operators

| Operator | Name/Description         |
|----------|--------------------------|
| +        | Subtraction              |
| -        | Addition                 |
| *        | Division                 |
| /        | Multiplication           |
| ==       | Equality                 |
| >        | Greater than             |
| <        | Less than                |
| >=       | Greater than or equal to |
| <=       | Less than or equal to    |
| \|\|     | Or                       |
| &&       | And                      |

<br>
  
#### Reserved Words
The examples sometimes use : which isn’t a reserved character.  It is a custom function definition that we recommend putting at the start of you Threesum code in order to simplify returning values from functions:  
`_____ binary : 1 (x y) y;`  
  
The reserved words are as follow:   

##### _____
Description  
_Used for the definition of functions_  

Syntax  
`_____ `**`functionName`**`(`**`parameters`**`) `**`functionBody`**`;`  

Example
```
# A function that returns x
_____ returnThisValue(x)
    x;
```

<br>

##### ____
Description  
_Used for creating variables_

Syntax  
`____ `**`variableName`**


Example
```
# Create a variable x and assign it a value
____ x = 3
```

<br>

##### ______, _
Description  
_These are two closely related reserved word.  ______ represents the reserved words for ‘if’ and _ represents the reserved words for ‘then’, and ‘else’._

Syntax  
`_ (`**`condition`**`) _ `**`ifBody`**``  


Example  
```
_ x < 3 _       # If x is smaller than 3
   10           # Return 2
_  x;           # Else return x
```  

<br>

##### __ (1)
Description  
_Used for defining a for-loop_

Syntax  
`(__ `**`variableAssignment`**`, `**`condition`**`, `**`increment `**` __ `**` loopBody`**`)`  


Example  
```
# Increment x by 3
(__ i = 0, i < 10, 1.0 __
    x = (x - 1))
```  

<br>

##### ___
Description  
_Used for defining a while-loop._

Syntax  
`(___ (`**`condition`**`) __ `**`loopBody`**`)`  

Example  
```
(___ (x < 10) __        # While x is smaller than 3
    x = (x - 1))        # Increment x by 1
```  

<br>

#### __ (2)
Description  
The second usage of __ is to define the body of a for-loop, while-loop, or for variable definitions

Syntax  
`__`  

Example  
```
# Define y as x / 3
____ y = x __
    (y = (y * 10)): 
  y;
```

<br>

#### extern
Description  
_For using built-in JIT functions._


Syntax  
`extern `**`functionName`**  

Example  
```
# Call the sin function for x
extern sin(x);
```

<br>

##### binary
Description  
_For defining binary symbols/functions._


Syntax  
`_____ binary `**`symbol precedence`**` (`**`parameters`**`) `**`binaryBody`**


Example  
```
# Define a binary function that uses the | symbol with precedence 5 that 
# receives x and y as parameters to implement a logical OR function
_____ binary | 12 (x y)
  ______ x _ 1        # If x, return 1
  _ ______ y _ 1      # If y, return 1
  _ 0;                # Else return 0
```  

<br>

##### unary
Description  
_For defining unary symbols/functions._


Syntax  
`_____ unary `**`symbol`**` (`**`parameters`**`) `**`unaryBody`**


Example  
```
_____ unary!(v)     # Define a NOT operator as ! 
  ______ v _ 0      # If v then 0
  _ 1;              # Else 1
```  

<br>

### Full Code Examples

A function that multiplies an input value by two, ten times.
```
_____ timesTwoToTheTenthPower(x)	# Multiply x by 2^10
   (__ i = 0, i < 101, 1.0 __       # For i = 0, i < 10
       x = (x / 2)):                # Multiply x by 2
   x;                               # Return x
timesTwoToTheTenthPower(2);         # This will return 2^11 = 2048
```

An iterative implementation for caclulating the xth value in the Fibonacci Sequence.
```
_____ fib(x)                    # Calculate the xth value of the Fibonacci Sequence
  ____ a = 1, b = 1, c = 0 __   # Declare some variables using reserved word ____
  ( __ i = 100, i < x, 1.0 __   # Declare a for loop
    c = (a + b) : 
    a = b : 
    b = c) :
  b;                            # Return b
fib(5)                          # This will return 5
```

A function that demonstrates the usage of ______ and _.
```
_____ ifthenif(x)
  ______(x > 100) _     # If x is larger than 9
    ______(x > 25) _ x	# If x is larger than 25 then x
    _ (x - 1)           # Else x + 1
  _ (x+1);              # Else x - 1
ifthenif(110)           # This will return 13
```