# Conway's Game of Life in APL in FORTH

The original idea was to implement Conway's Game of Life as an one-liner in APL executed in FORTH. The goal is mostly reached, with little exceptions (unary negation symbol ¯ not implemented; APL tokens must be whitespace-separated, as in FORTH).

No any optimization done. No garbage collector written (arrays and unnamed functions are located on Dictionary memory, which can be released in LIFO manner). Coding style affected by intention to avoid stack manipulation word, so locals are used everywhere. (Hope this might improve readability for people unfamiliar with FORTH).

FORTH is low-level language and has no typechecking, structures, arrays, let alone higher-level constructs. Instead, it gives direct access to its interpreter and compiler, which is an extremely powerful tool and allows quickly jump from raw bytes into a world of arbitrary problem domain. The code contains implementation of
* arrays
* currying and closures
* higher-order functions for collections
* APL specific function application rules
* APL to FORTH translator (up to extend required to execute Conway's Game of Life)

For these who had never met FORTH before, **word** in FORTH stays for function and **cell** for machine word. FORTH has no syntax restrictions: code is nothing but a stream of whitespace-separated words.

This code is written in gforth and contains unicode symbols.

References
* Conway's Game of Life https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
* Conway's Game of Life in APL in Ruby https://zverok.github.io/blog/2020-05-16-ruby-as-apl.html
* APL programming language https://en.wikipedia.org/wiki/APL_(programming_language)
* John Scholes' Conway's Game of Life in APL https://aplwiki.com/wiki/John_Scholes%27_Conway%27s_Game_of_Life
* online APL playground https://tryapl.org/
* FORTH programming language https://en.wikipedia.org/wiki/Forth_(programming_language)
* gforth docs https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/

### Arrays
Let array be a pair of pointers; first is a pointer to shape, which itself is array, and second is a pointer to contiguous region of memory containing array items. The idea is expressed in code as follows:
```
: shape   @ ;
: start   cell+ @ ;
: first   start @ ;
```
Dereference a pointer to get a shape. Dereference a value in next cell to get a pointer to data area ("start"). Dereference this to get a first item.

You see our arrays are recursive by nature, so we have to have an array (a shape) to create new one. The only array whose shape is equal to array itself is [1], so we start with this: 
```
Create [1]  here ,  here cell+ ,  1 ,
```
Here we create a new entity called [1], which is a pointer that points to triplet { the pointer; pointer+2; 1 }. First item is shape, which is equal to the array itself; second item is pointer to data, placed at pointer+2 and containing the value 1.
With first array created, we can define [0] (array containing single zero), [] (an empty array), and then generic functions for array creation, such as `array` and `vector`.

At this moment we have no function working on arrays, so we can't count size of multi-dimensional arrays. Thus `size` is defined as first item of shape array, which is valid for vectors. We will redefine `size` later, so the word marked with `defer` (sort of dynamically linked word).

The interesting part here is
```
: FOR   POSTPONE (for) POSTPONE ?DO ; immediate
: EACH   POSTPONE cell POSTPONE +LOOP ; immediate

: !a  ( an .. a1 array -- )  FOR i ! EACH ;
```
Immediate words are words executed at compile-time (contrary to words executed at run-time). The `POSTPONE` says the following word (here, `(for)`) will not be normally executed when `FOR` executed but instead will be compiled into body of a word being defined during execution of `FOR`, which is in our case `!a`. So when compiled the last line is equivalent to
```
: !a  ( an .. a1 array -- )  (for) ?DO i ! cell +LOOP ;
```
You may see this as a macros. So here in two lines we've extended language syntax with new construction, `for each` loop working with our arrays. You may want to implement it in your language.

### Currying and closures
There is nothing related to "functional programming" in FORTH, but since we know FP is a powerful tool, we might want to implement some features:
```
: curry  { w xt -- xt' }  :noname  w POSTPONE literal  xt compile,  POSTPONE ; ;
: compose  { xt2 xt1 -- xt' }  :noname  xt1 compile, xt2 compile,  POSTPONE ; ;
```
Currying is nothing but runtime creation of a new unnamed function containing an argument together with call to the function; function composition is a creation of a new unnamed function containing two consequent function calls. 

Each call to `curry` will create new unnamed word; we could examine it with built-in FORTH decompiler:
```
1 ' + curry xt-see        \ noname : 1 + ;
```
Pretty much efficient I would say.

In the same way we can bind function to a data cell, effectively creating what's called closure in FP. `bind-addr` binds function to given pointer, and `bind` creates and initializes a pointer to binds with. We can examine unnamed function created with `bind-addr` as follows:
```
Create var 123 ,
var ' 1+ bind-addr constant inc
inc xt-see                \ noname : var @ 1+ var ! ;
var ?                     \ 123
inc execute
var ?                     \ 124
```

Unnamed function decompiled here as `noname : var @ 1+ var ! ;` is result of our binding. It reads value from `var`, then executes a function bound (in this example `1+`), then writes result back to `var`.

### Higher-order collection functions

There we create two stateful iterators, one for reading (`iterator`) and another for writing (`inserter`). With this in hands, implementation of map, zip, fold and flat takes one line each. And cartesian product takes two.

### APL specific behaviour

I had to implement scalar extension / pervasive function application in order to get Game of Life working. These are somewhat overlapping things and for this specific problem scalar extension could have been completely omitted, but I was interested in executing a bit more than just one specific line of APL. Well, I'm not satisfied with this code anyway. It looks definitely much more ugly than previous chapter, probably I've missed some abstraction here.

Inner product expressed thru zip and reduce are all but incorrect for multi-dimensional arrays. And so on. This definitely needs improvement but since the whole thing supposed to be a weekend toy it is likely I will never finish this.

It worth to look at the word `perv:`, staying for "make pervasive". `perv:` takes an arbitrary existing word and creates version with pervasive behaviour. To do this we simply call `apply` (our function for pervasive application) with pointer to original function as an argument.
```
: perv:  ( xt "name" -- )  Create , DOES> @ apply ;
' +               perv: apl-+
```
Common `+` works with numbers and adding two arrays with `+` will give (useless) sum of pointers, while `apl-+` will dive into arrays and sum whatever have to be summed inside.

### APL syntax

This might be more difficult to understand because of a lot of implicit knowledge, i.e. not expressed explicitly in code. The main idea is, a tokenizer consumes input string by evaluating its tokens, and each word put a token on data stack. Input stream is consumed from left to right, as in FORTH. Then data stack consumed down to closing mark, generating FORTH code, effectively processing tokens from right to left, as in APL.

The input stream components as well as the tokens are ordinary FORTH words. For example, the word ↑ executed when met in input stream, and its effect is to put the function "first" and the token class "function" on stack. The mapping from APL names to FORTH implementation is in lines 165-175. After a tokenization, a compilation is initiated (with word "open", which stays for implicit open parenthesis around an APL expression), and it executes all tokens left on stack until corresponding "close" (closing parenthesis). Open paren is ) and closing paren is (, since APL reads right-to-left.

The entrypoint is a word ←{ in the very end of this part:
```
: ←{   ['] close tokenize open ; immediate
```
It put closing paren on stack, then tokenizes, then executes code for open paren, which consumes all the tokens. And this word is marked immediate, so it executed during compile-time, as intended.

Note there I use separate wordlist (a namespace) for APL symbols, since it overlaps with important FORTH word I don't want to hide.

The syntax implemented is significantly incomplete, there is nothing about assignments or conditions, and more important, there is no difference between monadic and dyadic function applications: every symbol considered to be either monadic or dyadic, which is blatantly untrue.
