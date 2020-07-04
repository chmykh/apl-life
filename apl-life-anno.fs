#! /usr/bin/gforth
		
		\ I love FORTH and hope to present reasons with this code.

		\ The original idea was to implement Conway's Game of Life as a one-liner in APL executed in FORTH.
		\ (APL is another weird-looking programming language). The goal is mostly reached, with few exceptions.
		\ Unary negation symbol ¯ not implemented. APL tokens must be whitespace-separated, as in FORTH.

		\ FORTH is a low-level language and has no type checking, structures, arrays, let alone higher-level 
		\ constructs. Instead, it provides a direct access to its interpreter and compiler. This comes to be
		\ an extremely powerful tool. It allows us to quickly jump from raw bytes into a world of a problem 
		\ domain. The code below contains an implementation of 
		\ - arrays
		\ - currying and closures
		\ - higher-order functions for collections
		\ - APL specific function application rules, such as pervasion
		\ - APL to FORTH translator (up to extend required to execute Conway's Game of Life)
		\ All of that is done in ~200 LOC and tooks < 2.5K words of memory.

		\ For these who had never met FORTH before, "word" in FORTH stays for function and "cell" for machine
		\ word. FORTH has no syntax restrictions: code is nothing but a stream of whitespace-separated words. 
		\ FORTH is interactive: the code below is not only a complete program but also a history of a
		\ development session.
		
		\ A coding style is affected by an intention to avoid stack manipulation words. Locals are mostly used 
		\ instead. Hope this might improve readability for people unfamiliar with FORTH.

		\ This code is written in gforth and contains Unicode symbols.

		\ References
		\ Conway's Game of Life https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
		\ Conway's Game of Life in APL in Ruby https://zverok.github.io/blog/2020-05-16-ruby-as-apl.html
		\ APL programming language https://en.wikipedia.org/wiki/APL_(programming_language)
		\ John Scholes' Conway's Game of Life in APL https://aplwiki.com/wiki/John_Scholes%27_Conway%27s_Game_of_Life
		\ online APL playground https://tryapl.org/
		\ FORTH programming language https://en.wikipedia.org/wiki/Forth_(programming_language)
		\ gforth docs https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/

cr ." Available free space on the dictionary: " unused .   unused Constant unused0
cr ." Machine word size: " cell .

		\ 1. Arrays

		\ First we have to implement arrays. APL arrays are multi-dimensional and support nesting which is an 
		\ unrelated concept (a scalar is a 0-dimensional array, a vector is 1-dimensional, and matrix is 
		\ 2-dimensional, and a vector of vectors is not a matrix).

		\ The main idea of the implementation provided below is the fact that array's shape is itself an array,
		\ so any array can be represented as a reference to another array and a reference to raw data. 
		\ The FORTH word for dereferencing is "@" and for incrementing a pointer by one machine word is "cell+".

		\ Hereafter non-indented and single-idented lines contain the main program, while double-idented lines 
		\ contain examples and tests explaining what's going on, and these can be omitted. We will write code 
		\ in tiny steps and test every line just next to it. Sometimes we will first try things in interpreter 
		\ mode and then wrap a code in a word definition.

: shape   @ ;
: data   cell+ @ ;
: first   data @ ;

		\ We have no function for array creation right now, but as the whole thing is a matter of convention, 
		\ we can create the first array manually. Anywhere in a memory, suppose at address PTR, we will put a 
		\ triplet [PTR; PTR+2; 1], and according to the definition above (the words "shape", "data" and 
		\ "first"), this triplet can be understood as a 1-dimensional array with a single element, the 1.
		\ A triplet [1; PTR; PTR-1] would also do.

		\ In the line below, there are "Create", "here", and "," some of a most frequently used FORTH words. 
		\ There is a memory area called Dictionary hosting FORTH words and other program data. The Dictionary 
		\ is organized as a stack, so is filled continuously and incrementally. "here" is a pointer to free 
		\ space on top of the Dictionary. "," (comma) writes a machine word from the data stack to the 
		\ Dictionary and increments value of "here". "Create" introduces a new word (whatever is to the right 
		\ of it) whose meaning is the value of "here" at the moment of the definition).

		\ So the line below states the following:
		\ Create [1]		let the current value of "here" be called [1].
		\ here			put the value of "here" on the stack.
		\ ,			write the value from the stack to "here"; increment "here".
		\ here			put the value of "here" on the stack (now "here" is [1] + 1 machine word).
		\ cell+			increment the value on the stack by 1 machine word.
		\ ,			write the value from the stack to "here"; increment "here".
		\ 1			put 1 on the stack.
		\ ,			write the value from the stack to "here".
		\ Thus we have created the [PTR; PTR+2; 1] triplet and assign it a name, [1].

Create [1]  here ,  here cell+ ,  1 ,
		\ We can immediately examine this array with our accessor words:
		cr [1] .				\ PTR (some integer value of a pointer)
		cr [1] shape .				\ PTR (shape of [1] is [1])
		cr [1] data .				\ PTR+2
		cr [1] first .				\ 1

		\ Note the [1] is the only array whose shape is equal to the array itself, so it's naturally the only 
		\ choice for an array to be created first.

		\ The very important property of an array is its size. It is not very clear what is "size" in a general 
		\ case but what we need first is a size of memory chunk to allocate for array items. A scalar requires 
		\ 1 memory cell, a vector as many cells as there are items, and a matrix a product of rows and columns. 
		\ Right now we have no functions to iterate over shape, so let's define "size" for the simplest case of 
		\ 1-dimensional arrays, like this:
		: size   shape first ;

		\ FORTH allows us to redefine any word, and a new behavior hides an old one. But a words already 
		\ compiled still refer to the old implementation. In case we want not to hide but retroactively change 
		\ the meaning of a word, we have to declare at as "deferred", i.e. linked dynamically.

		\ The definition of deferred "size" looks like this:
defer size
:noname   shape first ;
is size

		\ Here is a word to create new arrays at run-time:
: array  { data shape -- a }  here  shape , data , ;
		\ Without locals, this could be written as:
		: array  ( data shape -- a )  here -rot , , ;
		\ Which is significantly shorter but not so explanatory.

		\ We can try it with following line:
here 0 ,  [1] array  Constant [0]
		\ Here we have first allocated a single cell of memory and saved 0 here (with a phrase "here 0 ,", 
		\ which is readable as well as plain English); then we've provided the array shape, [1]; then called 
		\ the constructor "array" and assigned a name "[0]". It is a one-dimensional array containing a single 
		\ value, 0.
		cr [0] .				\ some pointer
		cr [0] shape .				\ PTR (the pointer to [1])
		cr [0] first .				\ 0
		cr [0] size .				\ 1

		\ An empty array is even simplier, as it does not need allocated data so we can use 0 in place of data 
		\ pointer:
		0 [0] array  Constant []
		cr [] .					\ pointer to []
		cr [] shape .				\ pointer to [0]
		cr [] size .				\ 0
		\ But it is more convenient to reuse the data pointer of [1], so our (still incomplete) definition 
		\ of "size" will work properly:
here 1 ,  [0] array  Constant []
		cr [] first .				\ 1

 		\ Let's define some more convenient constructors.
		\ Scalar is a zero-dimensional array (i.e., its shape is an empty vector):
: scalar  { u -- a }  here u ,  []  array ;

		\ The following line creates a scalar with value "101":
		101 scalar  Constant tmp
		cr tmp shape .				\ pointer to []
		cr tmp first .				\ 101
		cr tmp size .				\ 1

		\ Vector is a one-dimensional array. Its shape is a single-element vector. A vector constructor accepts
		\ a pointer to a pre-allocated data area and a size. It creates a shape vector ("here u , [1] array") 
		\ and then calls "array":
: vector  { data u -- a }  data  here u ,  [1] array  array ;

		\ To see it working, we have to manually allocate data here and store some values:
		here  103 , 107 ,
		\ And then we call a vector constructor (the value of "here" is already on the stack):
		2 vector  Constant tmp
		cr tmp size .				\ 2
		cr tmp first .				\ 103
		cr tmp data cell+ @ .			\ 107

		\ Here is an utility word that allocates a data area for a given number of elements:
: new  { u -- data u }  here  u cells allot  u ;

		\ Examine it like this:
		5 new vector  Constant tmp
		cr tmp size .				\ 5
		cr tmp first .				\ (some value from an unassigned memory cell)
		\ The vector we've just created is ready but not populated, we can assign values with common memory 
		\ accessing words:
		109 tmp data !
		cr tmp first .				\ 109

		\ Now we can define how to iterate over items, that is, to implement the "for each" loop.

		\ There is several loop words in FORTH, such as "DO", "?DO", "LOOP", "+LOOP", and "I", which can be 
		\ used like this:
		cr 5 0 [DO] [i] . [LOOP]		\ 0 1 2 3 4
		cr 5 0 [?DO] [i] . 2 [+LOOP]		\ 0 2 4

		\ To iterate over our array items, we need to loop from data start to data end in one-machine-word 
		\ steps. Here is a word to get a data end:
: end  { a -- addr }  a data  a size cells + ;
		\ And here is a word to put on the stack both data end and data start, in this order:
: (for)  { a -- and data }  a end  a data ;
		\ Test it by creating two-element vector and definiting a printing function:
		here 113 , 127 , 2 vector  Constant tmp
		: fn   (for) ?DO i ? cell +LOOP ;
		cr tmp fn				\ 113 127

		\ This is not very readable, and is error-prone since we can easily forget the word "cell" before 
		\ "+LOOP", which would break the pointer arithmetic. But we can easily enhance the language syntax:
: FOR   POSTPONE (for) POSTPONE ?DO ; immediate
: EACH   POSTPONE cell POSTPONE +LOOP ; immediate

		\ And use it like this:
: .a   FOR i ? EACH ;

		cr tmp .a				\ 113 127
		
		\ What's going on here needs a more elaborate explanation. "immediate" words are words executed at 
		\ compile-time (contrary to words executed at run-time). The word "POSTPONE" says the following word 
		\ (e.g., "(for)") will not be normally executed when "FOR" executed but instead will be compiled into 
		\ the body of a word being defined during execution of "FOR", which is in our case ".a". So when 
		\ compiled the code of ".a" is exactly equivalent to this of "print". This can be seen with decompiler:
		cr see fn				\ : fn    (some code follows)
		cr see .a				\ : .a    (the same code)

		\ You may see this as a sort of macro. With two lines of code, we've extended language syntax with
		\ the new construction, "for each" loop working with our arrays. You may want to implement it in your 
		\ language.

		\ Here is a word to populate an existing array from the stack:
: !a  ( an .. a1 array -- )  FOR i ! EACH ;

		\ And a word to create an array and populate it from the stack:
: >a  ( an .. a1 u -- a )  new vector { a }  a !a  a ;

		4 3 2 1  4 >a  Constant 1_2_3_4
		cr 1_2_3_4 .a				\ 1 2 3 4
		\ Note the reversed element order.

		\ A word to create an array and fill it with single value:
: fill  { w u -- a }  u new vector { a }  a FOR w i ! EACH  a ;
		cr 5  4 fill .a				\ 5 5 5 5

		\ Some not-that-pretty-printer supporting recursive/nested arrays:
: number?   abs 32767 <= ;
: array?   number? 0= ;
: print  { a -- }
	a array? IF ." [" a shape .a ." | " a FOR i @ recurse EACH ." ] " ELSE a . THEN ;
		\ Here I assume our arrays will host only short ints and pointers to other arrays, and its ranges
		\ do not overlap.

		cr 131 print				\ 131
		cr 131 scalar print			\ [| 131]
		cr 131 1 >a print			\ [1| 131]
		cr 1_2_3_4 print			\ [4| 1 2 3 4]
		cr 4 3 2 2 >a 1 3 >a print		\ [3| 1 [2| 2 3] 4]

		\ Following functions perform partitioning/slicing, we will need it later.
		\ Note the data is not copied here, but pointers only.
: before  { a n -- a' }  a data  n vector ;
: after  { a n -- a' }  a data n cells +  a size n -  vector ;
: slice  { a from count -- a' }  a from after  count before ;
		cr 1_2_3_4  2 before .a			\ 1 2
		cr 1_2_3_4  2 after .a			\ 3 4
		cr 1_2_3_4  1 2 slice .a		\ 2 3




		\ 2. Currying and closures

		\ There is nothing related to functional programming in FORTH. But as we know the FP is a powerful 
		\ paradigm we will implement some of its features here. Namely: currying, runtime function composition,
		\ and closures.

		\ Currying is implemented by binding together a function and its last argument.

		\ Here is how such binding looks like if done at compile time:
		: 10+ 10 + ;
		cr see 10+				\ : 10+ 10 + ;
		cr 9 10+ .				\ 19
		\ Basically, this is nothing but creating a new short function. The main point is that we want this 
		\ to happens at run-time, not at compile-time.

		\ First, our run-time-created function should be unnamed, like this:
		:noname 10 + ;
		cr xt-see				\ noname: 10 + ;

		\ Second, the argument value and the function value has to be provided at run-time. Here we will 
		\ rewrite the previous definition, separating run-time-provided values from the rest of the function 
		\ body:
		:noname  [ 10 ] literal [ ' + compile, ] ;
		cr xt-see				\ noname: 10 + ;
		\ The result is equivalent but the source looks differently. The words [ and ] switch the FORTH state 
		\ from compilation to interpretation and back. So the phrase above could be read like:
		\ :noname		start the definition of a new unnamed word
		\ [ 10 ]		at compile-time, put 10 on the stack
		\ literal		what is on the stack at compile-time gets compiled into
		\			the unnamed word we are currently compiling
		\ [ ' +			at compile-time, put the function + on the stack
		\ compile,		at compile-time, compile what is on the stack
		\			into the unnamed word we are currenly compiling
		\ ]			...go back to compilation state
		\ ;			end of the word
		\ As you can see with xt-see, this word is decompiled extacly as the previous one.

		\ The word "'" (tick) is one of few so-called parsing words. It takes an input not from the data stack
		\ but from the input stream. In the code above, the tick consumes the following "+" from the input 
		\ stream, so instead of execute "+", the interpreter put reference to "+" on the stack.

		\ Values 10 and ' + could be moved out to some variables:
		10 Value w
		' + Value xt
		:noname  [ w ] literal [ xt compile, ] ;
		cr xt-see				\ noname: 10 +

		\ And the last step is to make it all to be performed at run-time. Keep in mind the run-time 
		\ of "curry" is a compile-time of its derived unnamed word.
		: curry { w xt -- xt' }
			:noname  w  POSTPONE literal  xt compile,  POSTPONE ;
		;
		\ What was previously done at compile-time ("w", "xt compile,") now is done at run-time, so not 
		\ enclosed in [ ]. What must be done at run-time of the derived unnamed word is POSTPONEd.

		w xt curry
		cr xt-see				\ noname: 10 +
		11 ' - curry
		cr xt-see				\ noname: 11 -
		\ It works!
		\ As our definition of "curry" is somewhat long, we will split into 3 words:
: literal,  ( w -- )  POSTPONE literal ;
: compile-curried,  { w xt -- }  w literal,  xt compile, ;
: curry  { w xt -- xt' }  :noname  w xt compile-curried,  POSTPONE ; ;

		\ In the same way we implement a function composition:
: compose  { xt2 xt1 -- xt' }  :noname  xt1 compile, xt2 compile,  POSTPONE ; ;
		\ Test it by composing the increment `1+` with the output '.'.
		cr ' . ' 1+ compose Constant tmp
		cr tmp xt-see				\ noname : 1+ . ;
		cr 211 tmp execute			\ 212

		\ In case we need to curry not the last but second-from-last argument, we can flip function arguments
		\ by composing it with the word "swap", and then bind the last argument as before:
: flip  ( xt -- xt' )  ['] swap compose ;
		\ Let's check it with the assymetrical two-argument function "divide":
		\ Divide by 10:
		10 ' / curry Constant /10
		cr 30 /10 execute .			\ 3
		\ Divide 10 by:
		10 ' / flip curry Constant 10/
		cr 5 10/ execute .			\ 2

		\ Closures bind a function to a variable. As we don't use variables, we will use pointers, i.e. bind 
		\ function directly to a memory cell. A bound function is an unnamed function which first reads a value
		\ from the cell, then executes a body of the target function, then writes a value back into the cell.
: bind-addr  { addr xt -- xt' }
	:noname  
		addr ['] @ compile-curried,  xt compile,  addr ['] ! compile-curried,
	POSTPONE ; ;
: bind  { w xt -- xt' }  here w , xt bind-addr ;

		\ "bind-addr" binds function to a provided memory cell.
		\ "bind" allocates a memory cell and writes an initial value here, then bounds.

		\ In the example below, we create a cell with a value "223" in it, then bind a function "1+" to it.
		\ The new function, referenced by "xt", will increment the value in the cell every time it is called.
		Create tmp 223 ,
		tmp ' 1+ bind-addr  Constant xt
		cr tmp ?				\ 223
		cr xt execute tmp ?			\ 224
		cr xt execute tmp ?			\ 225

		\ Note: implemented this way, closures require a function to left on top of the stack a value 
		\ semantically equivalent to a value on top of the stack at function start, i.e. the last argument 
		\ value. In the example above, binding works as expected because "1+" has the required stack effect
		\ ( w -- w ). It will also work with "+":
		tmp ' + bind-addr  Constant xt
		cr 100 xt execute tmp ?			\ 325
		cr 200 xt execute tmp ?			\ 525

		\ Words which don't follow this pattern must be wrapped accordingly. The "." word which is ( w -- )
		\ need to be wrapped like this:
		: fn  ( w -- w )  dup . ;
		tmp ' fn bind-addr  Constant xt
		cr xt execute				\ 525
		\ -- end of note.




		\ 3. Higher-order collection functions

		\ Now let's implement collection functions such as map. We already have a way to iterate over 
		\ a collection (the for each loop), but this seems to be not enough, as some functions have to iterate
		\ over two or more collections simultaneously. We will implement another well-known solution called 
		\ an iterator. Actually, the iterator's functionaly overlaps with this of the for each loop, so we 
		\ would not need the for each if we have full-scale iterators. But we will implement only very basic 
		\ iterators without any range checkings, and rely on loops for a counting.
		
		\ A simple iterator is a pointer that increases its value every time it is read.

		\ The reading word is "@", the writing is "!", and the pointer increment is "cell+". Here we define 
		\ words for "read and increment pointer" and "write and increment pointer":
: !+  { n addr -- addr' }  n addr !  addr cell+ ;
: @+  { addr -- n addr' }  addr @  addr cell+ ;
		\ (By the way, it can be written much simpler without locals, but stack manipulation words make code 
		\ look somewhat creepy, or at least cryptic:
		: !+  tuck ! cell+ ;
		: @+  dup @ swap cell+ ;

		\ Try it with the following code. Here we allocate two memory cells with values 101 and 103 and create 
		\ a pointer "tmp" to the first. After the call to "107 tmp !+" the value 107 is written over the 101, 
		\ and the incremented value of the pointer is left on the stack. After the next call, "109 swap !+",
		\ the value 109 is written over the 103.
		Create tmp 101 , 103 ,
		cr tmp ?  tmp cell+ ?			\ 101 103
		cr tmp .				\ some ptr
		107 tmp !+
		cr dup .				\ ptr to next cell
		109 swap !+
		cr .					\ ptr to yet next cell
		cr tmp ?  tmp cell+ ?			\ 107 109

		\ The iterator is made of "!+" or "@+" bound with a pointer, and the initial pointer value is the data
		\ pointer of the target array:
		1_2_3_4 data ' @+ bind  Constant tmp
		cr tmp execute .			\ 1
		cr tmp execute .			\ 2
		cr tmp execute .			\ 3
: iterator  ( a -- xt' )  data  ['] @+ bind ;
: inserter  ( a -- xt' )  data  ['] !+ bind ;

		\ As we need something with counter to provide range checking, we'll wrap the for each loop into 
		\ a functional-style iteration: let the word "iter" accept a function and an array and apply 
		\ the function to every item:
: iter  { xt a -- }  a FOR i @ xt execute EACH ;
		
		cr ' .  1_2_3_4 iter			\ 1 2 3 4
		\ We could define a "clone" function like this:
		: clone  { a -- a' }
			a size new vector { a' }
			a' inserter
			a iter
			a' ;
		cr 1_2_3_4 clone .a			\ 1 2 3 4
		\ Since many of collection functions we need (map, zip, product...) follow the pattern 
		\ "create array - create inserter - do something - return newly created array", we will extract
		\ the logic into a word:
		: construct  { u -- a' xt }  u new vector { a' }  a'  a' inserter ;
		\ The sequence "{ a' } a' a'" looks especially silly, as it just duplicates a value on the stack, so
		\ with your permission I'm going to use one of stack manipulation words, "DUP":
: construct  ( u -- a' xt)  new vector dup inserter ;

		\ Finally, in the definition of the "clone" above, the resulting array is always a vector, while 
		\ the function input can have some other shape.
		\ The word to change a shape of an existing array is trivial:
		: shape!  ( shape a -- )  ! ;
		\ But it is more suitable for our needs to left the array on the stack:
		: shape!  { a shape -- a }  shape a !  a ;
		\ Or without locals:
: shape!  ( a shape -- a)  over ! ;
		\ The word to copy shape from one array to another can looks like this:
: shape-as  ( a other -- a )  shape shape! ;

		\ So there are some basic collection functions.
: map  { a xt -- a' }  a size construct  xt compose  a iter  a shape-as ;
: zip  { al ar xt -- a' }
	al size construct  xt compose  al iterator compose  ar iter  al shape-as ;
: inject  { a xt zero -- w }  here { accum } zero ,  accum xt bind-addr  a iter  accum @ ;
: fold  { a xt -- w }  a 1 after  xt  a first  inject ;
: contains  { a w -- f }  a  w ['] = curry  map  ['] or  false inject ;
		\ Any function could be tested immediately:
		cr 1_2_3_4  ' 1+ map .a			\ 2 3 4 5
		cr 1_2_3_4 1_2_3_4 ' + zip .a		\ 2 4 6 8
		cr 1_2_3_4 ' + 10 inject .		\ 20
		cr 1_2_3_4 ' + fold .			\ 10
		cr 1_2_3_4 2 contains .			\ -1
		cr 1_2_3_4 5 contains .			\ 0

		\ Now we can provide the correct implementation for "size". Note we use "inject" and not "fold" because
		\ a shape of a scalar is an empty vector but its size is defined as 1.
		cr 2 3 2 >a  ' * 1 inject .		\ 6
		
		\ Also, we have to protect it from infinite recursion on [1]:
:noname  { a -- u }  a [1] = IF 1 ELSE a shape  ['] *  1 inject THEN ;  is size

		\ Check it:
		cr [1] size .				\ 1
		cr 1_2_3_4 size .			\ 4
		6 5 4 3 2 1 6 >a   3 2 2 >a shape!  Constant _m2x3
		cr _m2x3 size .				\ 6
		cr [] size .				\ 0
		cr 1 [] array size .			\ 1
		\ Note items of nested arrays doesn't count, as expected:
		cr 1_2_3_4 1_2_3_4 2 >a size .		\ 2

		\ Some more collection functions, implemented just for purpose of Game of Life.

		\ "flat" accepts a homogeneous vector of vectors, and returns a new vector:
: flat  { a -- a' }  a ['] size map ['] + fold  construct  ['] iter flip curry  a iter ;
		cr 1_2_3_4 1_2_3_4 2 >a flat print	\ [8| 1 2 3 4 1 2 3 4]

		\ "product" accepts two vectors and returns a matrix:
: (product)  { w xt al -- }  w xt curry  al iter ;
: product  { al ar xt -- a' }
	al size ar size * construct
	xt compose  al  ['] (product) curry curry  ar iter
	al size ar size 2 >a shape! ;
		cr 1_2_3_4 20 10 2 >a ' + product print	\ [2 4| 11 12 13 14 21 22 23 24]
		cr 20 10 2 >a 1_2_3_4 ' + product print	\ [4 2| 11 21 12 22 13 23 14 24]

		\ "rotate" rotates element of a vector:
: rotate  { a offset -- a' }  offset a size mod { n }  a n before  a n after  2 >a  flat ;
		cr 1_2_3_4 1 rotate .a			\ 2 3 4 1
		cr 1_2_3_4 -1 rotate .a			\ 4 1 2 3

		\ "integers" is a constructor that creates a sequence from 0 to u-1:
: integers  { u -- a }  u construct { xt }  u 0 ?DO i xt execute LOOP ;
		cr 4 integers .a			\ 0 1 2 3
		
		\ "rows" accepts a matrix and returns a vector of vectors:
: second   data cell+ @ ;
: height  shape first ;
: width  shape second ;
: rows  { a -- a' }
	a height integers  a width ['] * curry map
	a  a width ['] slice curry  flip curry  map ;
		cr _m2x3 print				\ [2 3| 1 2 3 4 5 6]
		cr _m2x3 rows print			\ [2| [3| 1 2 3 ] [3| 4 5 6]]




		\ 4. APL specific functions

		\ Here we get closer to the APL world. In APL, the array is a basic object. Any numeric value is 
		\ a scalar an therefore an array (of rank 0). Every item of every array, unless it is a scalar 
		\ containing a numeric value, is also an array. This is not true in our model. Our arrays can contain 
		\ not only arrays (including scalars) but also naked numeric values, like this:
		cr 1_2_3_4 print			\ [4| 1 2 3 4]
		\ While the array-only model would represent the same value as follows:
		cr 1_2_3_4 ' scalar map print		\ [4| [| 1] [| 2] [| 3] [| 4]]

		\ Here we update some array functions to work properly with plain numbers.
: size  { a -- u }     a array? IF a size   ELSE 1  THEN ;
: shape  { a -- a' }   a array? IF a shape  ELSE [] THEN ;

		\ According to APL rules, we can wrap any array into a scalar, but a number wrapped in a scalar is
		\ equal to the number itself.
: wrap  { a -- a' }    a array? IF a scalar ELSE a  THEN ;
: unwrap  { a -- a' }  a array? IF a first  ELSE a  THEN ;

		\ Here are definitions for basic array properties "rank" and "depth":
: rank   shape shape first ;
: 'recurse   latestxt literal, ; immediate
: depth  { a -- a' }   a array? IF a 'recurse map ['] max 0 inject 1+ ELSE 0 THEN ;
: scalar?   rank 0= ;

		cr 1 wrap print				\ 1
		cr [1] wrap print			\ [| [1| 1]]
		cr 1 unwrap print			\ 1
		cr 1 scalar unwrap print		\ 1
		cr 1 rank .				\ 0
		cr [1] scalar rank .			\ 0
		cr [1] rank .				\ 1
		cr _m2x3 rank .				\ 2
	
		cr 1 depth .				\ 0
		cr [1] depth .				\ 1
		cr _m2x3 depth .			\ 1
		cr [1] scalar depth .			\ 2
		cr [1] scalar scalar depth .		\ 3

		\ Conversion from an arbitrary-shaped array into a 1-dimensional vector:
: ravel  { a -- a' }  a data  a size vector ;

		\ One of APL features important for our task is the pervasive function application. If there is 
		\ a function defined on scalars, such as +, it can be applied to arrays, and its behavior is
		\ to traverse thru array elements and apply to each.
		\ First, we implement the pervasive application of unary function:
: uperv  ( a xt -- a' )  over number? IF execute ELSE 'recurse curry map THEN ;
		
		cr 1 ' 1+ uperv print			\ 2
		cr [1] ' 1+ uperv print			\ [1| 2]
		cr 1_2_3_4 ' 1+ uperv print		\ [4| 2 3 4 5]
		4  3 2 2 >a  1 3 >a  Constant tmp
		cr tmp ' 1+ uperv print			\ [3| 2 [2| 3 4] 5]

		\ The binary pervasive application is a bit more complicated. It both arguments are arrays, 
		\ the function is applied to corresponding pairs (as "zip"). If one argument is an array and another 
		\ is a scalar, the scalar is "extended" as if it is an array of a required size. In the our case we 
		\ don't have to create a new array representing this "extended" scalar but will curry the function 
		\ with the scalar and then "map" over the array.

		\ The implementation going to take more than one word and contain a mutual recursion, so "defer":
defer perv

		\ If both arguments are numbers,
: both-numbers?  { al ar -- f }  al number? ar number? and ;
		\ then simply "execute", and this is our first runnable version of "perv":
		:noname  { al ar xt -- a' }
			al ar both-numbers? IF al ar xt execute EXIT THEN
			s" not implemented yet" exception throw ;
		is perv

		cr 1 2 ' + perv print			\ 3

		\ If both arguments can be iterated over,
: both-iterable?  { al ar -- f }  al array? ar array? and al rank ar rank = and ;
		\ then zip:
: pairwise  ( al ar xt -- a' )  ['] perv curry zip ;
		\ Update the definition of "perv" to test it:
		:noname  { al ar xt -- a' }
			al ar both-numbers? IF al ar xt execute EXIT THEN
			al ar both-iterable? IF al ar xt pairwise EXIT THEN
			s" not implemented yet" exception throw ;
		is perv

		cr 1_2_3_4 1_2_3_4 ' + perv print	\ [4| 2 4 6 8]

		\ If the right argument is a scalar and the left is iterable,
: left-iterable?  { al ar -- f }  al array? ar scalar? and ;
		\ then curry xt with the scalar and then map over the array:
: extend  { al ar xt -- a' }  al ar unwrap xt ['] perv curry curry map ;
		\ Update the "perv":
		:noname  { al ar xt -- a' }
			al ar both-numbers? IF al ar xt execute EXIT THEN
			al ar both-iterable? IF al ar xt pairwise EXIT THEN
			al ar left-iterable? IF al ar xt extend EXIT THEN
			s" not implemented yet" exception throw ;
		is perv
	
		cr 1_2_3_4 4 ' + perv print		\ [4| 5 6 7 8]
		\ Finally, if the left argument is a scalar and the right is iterable, just swap the arguments and flip
		\ the "xt", then fall to "extend" as above.

		\ The complete code follows:
:noname  { al ar xt -- a' }
	al ar both-numbers? IF al ar xt execute EXIT THEN
	al ar both-iterable? IF al ar xt pairwise EXIT THEN
	al ar left-iterable? 0= IF ar al xt flip ELSE al ar xt THEN extend ;
	is perv

		\ Examine its behavior:
		cr 1 2 ' + perv print			\ 3
		cr 1  1_2_3_4  ' + perv print		\ [4| 2 3 4 5]
		cr 1  1_2_3_4 wrap ' + perv print	\ [| [4| 2 3 4 5]]
		cr [1]  1_2_3_4 wrap ' + perv print	\ [1| [4| 2 3 4 5]]
		cr 2 1 2 >a  1_2_3_4 wrap ' + perv print		\ [2| [4| 2 3 4 5] [4| 3 4 5 6]]
		cr 1_2_3_4  1_2_3_4  ' + perv print			\ [4| 2 4 6 8]
		cr 1_2_3_4  1_2_3_4 wrap ' + perv print			\ [4| [4| 2 3 4 5] [4| 3 4 5 6] [4| 4 5 6 7] [4| 5 6 7 8]]
		cr [1] 2 ' + perv print			\ [1| 3]
		cr 2 [1] ' + perv print			\ [1| 3]
		cr [1] [1] ' + perv print		\ [1| 2]
		cr 2 1 2 >a  1 ' + perv print		\ [2| 2 3]
		cr 2 1 2 >a  4 3 2 >a  ' + perv print	\ [2| 4 6]
		cr 2 1 2 >a  4 3 2 >a wrap  ' + perv print		\ [2| [2| 4 5] [2| 5 6]]
		cr 2 1 2 >a wrap  4 3 2 >a wrap  ' + perv print		\ [| [2| 4 6]]
		cr 2 1 2 >a wrap  4 3 2 >a wrap wrap  ' + perv print	\ [| [2| [2| 4 5] [2| 5 6]]]
		cr 2 1 2 >a wrap  1 ' + perv print			\ [| [2| 2 3]]
		cr 2 1 2 >a wrap wrap  1 ' + perv print			\ [| [| [2| 2 3]]]
		cr 2 1 2 >a  4 3 2 >a wrap wrap  ' + perv print		\ [2| [| [2| 4 5]] [| [2| 5 6]]]
		\ and so on.

		\ Finally, to simplify calls, we can curry the last argument of "perv":
: perv-+   ['] + perv ;
		cr 1 2 perv-+ print			\ 3
		cr [1] [1] perv-+ print			\ [1| 2]

		\ Some other functions we need pervasive:
: perv-and   ['] and perv ;
: perv-or   ['] or perv ;
: perv-*   ['] * perv ;

		\ The test for equality returns 0|1 in APL but 0|-1 in FORTH, so we have to modify it first:
: apl-=   = 1 and ;
: perv-=   ['] apl-= perv ;

		\ Some more trivial APL array functions:
: hrotate  { a u -- a' }  a rows  u ['] rotate curry map  flat  a shape-as ;
: vrotate  { a u -- a' }  a rows  u rotate flat  a shape-as ;
: reduce  ( a xt -- a' )  fold wrap ;
: hreduce  { a xt -- a' }  a rows xt ['] reduce curry map ;
: vreduce  { a xt -- a' }  a rows xt fold ;
: vector?   rank 1 = ;
: hrotate  { a u -- a' }  a u  a vector? IF rotate ELSE hrotate THEN ;
: hreduce  { a xt -- a' }  a xt  a vector? IF reduce ELSE hreduce THEN ;

		\ The simplified version of the inner product:
: inner-product  { * + -- a' }  * zip + reduce ;

		\ The generic inner/outer product whose behavior depends on the value of a right-hand function:
: apl-product  { al * + ar -- }
	+ ['] noop = IF al ar * product ELSE al ar * + inner-product THEN ;

		cr _m2x3 ' perv-+ hreduce print		\ [2 |6 15 ]
		cr _m2x3 ' perv-+ vreduce print		\ [3 |5 7 9 ]
		2 1 2 >a Constant _v12
		4 3 2 >a Constant _v34
		cr _v12  ' perv-+  reduce print		\ 3
		cr _v12 _v34 ' perv-+ execute print	\ [2| 4 6 ]
		cr _v12 _v34  2 >a ' perv-+  reduce print	\ [| [2| 4 6 ] ]

		\ At this point we are very close to the Game of Life itself, let's prepare the grid and nice output:
0 0 0 0 0 0
0 0 1 0 0 0
0 0 0 1 0 0
0 1 1 1 0 0
0 0 0 0 0 0
0 0 0 0 0 0 36 >a Constant grid
6 6 2 >a  grid !
		\ This configuration is called a "glider".

: show  { a -- }
	a array? 0= IF a . EXIT THEN
	a scalar? IF a unwrap recurse EXIT THEN
	a vector? IF a FOR i @ recurse EACH EXIT THEN
	'recurse ['] cr compose  a rows iter ;



		\ 5. APL to FORTH translator

		\ Our final part is a translator for the APL syntax. We want to convert an input string
		\ "↑ 1 ⍵ ∨ . ∧ 3 4 = + / , -1 0 1 ∘ . ⊖  -1 0 1 ∘ . ⌽ ⊂ ⍵"
		\ into a FORTH code equivalent to following:
		grid 
		wrap
		-1 0 1 3 >a  ' hrotate product
		-1 0 1 3 >a  ' vrotate product
		ravel
		' perv-+ reduce
		3 4 2 >a perv-=
		1 grid 2 >a  ' perv-and ' perv-or  inner-product
		first
		\ This code performs a single step of Game of Life, and
		show
		\ shows a second stage of a glider evolution.

		\ There is little job to be done during the translation phase. We have to change the execution order 
		\ from APL (right-to-left with infix functions and operators) into FORTH (left-to-right with postfix 
		\ functions). Also, we need to append array size to every array literal. -1 0 1 in APL is represented 
		\ as 1 0 -1 3 in our program.

		\ The translator work is two-phase. First, it tokenizes the input string: words are consumed from left 
		\ to right and every word put a token on the stack. Second, it compiles the token sequence starting 
		\ from the top, emitting a FORTH code for every token. For infix functions/operators, the translator 
		\ will look ahead for one or two lexemes.

		\ We will represent most tokens with a pair {token value, token class}. The token value is a function 
		\ implementing the operation and token class is a function that compiles the operation into a FORTH 
		\ code. E.g.,
: ⍵   ['] @local0 ['] compile, ; 
		\ So the omega (stays for a right operand in APL) is a token, its value is "@local0" (the FORTH word 
		\ to access an argument of a word with single local), and its class is "compile,". Being executed, 
		\ the class will compile token value into the current definition:

		: _test_dup { _ } [ ⍵ execute ⍵ execute ] ;
		cr see _test_dup			\ : _test_dup >l @local0 @local0 lp+ ; 
		cr 3 _test_dup . .			\ 3 3

		\ The token representing number is the number itself, see "number?" above.
		
		\ Below is the core translator function.
		\ If a token is a number, compile it into a current word as a literal.
		\ Otherwise, execute the token class and let it do what it wants.
: continue  ( token -- )  dup number? IF literal, ELSE execute THEN ;
		\ Apart from numbers, we'll define the following token classes:
		\ function
		\ dyadic-op
		\ monadic-op
		\ open			), an opening parenthesis, start of a sub-expression
		\ close			(, a closing parenthesis, end of a sub-expression
		\ compile,		special symbols directly mapped to FORTH, such as identifier ⍵

		\ Let's implement the abovementioned "-1 0 1" -> "1 0 -1 3" conversion.

		\ The definition of syntax is often recursive, so define a placeholder for mutually-recursive functions
		\ below:
defer open

		\ What considered to be a "value" is a number, or a subexpression, or an identifier:
: subexpression?   ['] open = ;
: identifier?   ['] compile, = ;
: value?  { t -- f }  t number?  t subexpression? or  t identifier? or ;

		\ A strand is a sequence of values:
: strand  ( .. -- u )  0  BEGIN { t cnt } t value? WHILE t continue cnt 1+ REPEAT t cnt ;
		\ Here we iterate over values, incrementing a counter in progress. Examine it:

		Create mark
		: _t [ mark 5 4 3 2 1 strand ] literal [ drop ] ;
		cr see _t				\ : _t 1 2 3 4 5 5 ;
		\ Here, "5 4 3 2 1 strand" compiled into "1 2 3 4 5 5" (what is sufficient for our array creation). 
		\ The "[ drop ]" in the end is required because the value of "mark" was left on the stack and we need 
		\ to throw it away before the end of the definition.

		\ To complete the array creation, we have to add the call to array constructor, ">a":
: value   strand  { size }  size 1 > IF size ['] >a compile-curried, THEN ;

		\ Check it works:
		: _t [ mark 5 4 3 2 1 value drop ] ;
		cr see _t 				\ : _t 1 2 3 4 5 5 >a ;
		: _t [ mark 1 value drop ] ;
		cr see _t 				\ : _t 1 ;
		: _t [ mark value drop ] ;
		cr see _t				\ : _t ;
		\ The multiple-element array had been wrapped as an array creation. The single number interpreted 
		\ as a scalar. An empty strand emits nothing.

		\ Below is the rest of the supported token classes. Note we have to differ between function references
		\ (tokens left and right from operator symbol) and function applications (all other cases). For 
		\ function references, we simply compile the reference into the current word:
: function-ref  { t -- }  literal, ;

		\ For an operator, we read the next function-ref (one to the left from an operator), then a value 
		\ (which may be empty in case of monadic operator), then we "compile," an operator (the "xt"), then 
		\ we continue:
: operator  { xt -- }  function-ref  value  xt compile,  continue ;
: dyadic-op   operator ;
: monadic-op   operator ;

		\ For a function, we check if there is a dyadic operator to the left, and then either compile the 
		\ reference ("literal,"), or read the next value and then compile an application ("compile,"), and 
		\ then continue:
: function  { t xt -- }
	t ['] dyadic-op = IF t  xt literal, ELSE t value  xt compile, THEN  continue ;
		\ Note: here we assume all functions are either unary or binary (APL monadic or dyadic) which is 
		\ not true in APL. Information about the arity of any given call is lost precisely here.

		\ An opening parenthesis (an expression) is a value and then optional anything:
:noname   value continue ;  is open

		\ A closing parenthesis is a no-op.
: close ;

		\ Now the fragment can be tested as a whole; in the following example, we put on the stack the list 
		\ of tokens: "close", "1", "function +", "2", "open", and then call "continue" to initiate
		\ the compilation:
		: _t [ ' close  1  ' + ' function  2  ' open  continue ] ;
		cr see _t				\ _t : 2 1 + ;

		\ As effect of "open" is to "continue", we need no explicit call the latter:
		: _t [ ' close  1 2 3 4  open ] ;
		cr see _t				\ _t : 4 3 2 1 4 >a ;

		\ We're almost there, now let's define APL symbols for tokens. We have already defined ⍵, others are 
		\ very similar, e.g. there is a ⊂ which is a "wrap" function:
		: ⊂  ['] wrap ['] function ;
		\ To avoid repetitions, we create a new word to define such tokens:

: apl:  { xt -- }  : xt literal, ['] function literal, POSTPONE ; ;
		\ We can use it as follows:
		' size apl: ≢
		: _t [ ' close  ≢ 1 2 3 4  open ] ;
		cr see _t				\ _t : 4 3 2 1 4 >a size ;
		cr _t .					\ 4
		\ (Really, APL ≢ is not "size").

		\ We don't want our APL symbols to hide useful FORTH words, so let them live in its own namespace:
wordlist Constant apl
		\ The phrase above just creates a new namespace but does not "opens" it. In FORTH there are separated 
		\ concept of "current" wordlist (the namespace to where newly created words go) and "search order" 
		\ (the namespace sequence to perform word search). The current wordlist accessed/changed with 
		\ "get-current" and "set-current", the search order with ">order" and "previous".

		\ So first we'll save the default current wordlist:
get-current 

		\ And then set the current wordlist to be "apl":
apl set-current

		\ And now define APL symbols.
		\ Symbols for functions are:
	' first apl: ↑		' perv-or apl: ∨	' perv-and apl: ∧	
	' perv-= apl: =		' perv-+ apl: +		' ravel apl: ,	
	' noop apl: ∘		' vrotate apl: ⊖	' hrotate apl: ⌽	
	' wrap apl: ⊂		' perv-* apl: ×		' size apl: ≢
	
		\ Symbols for operators:
	: / ['] hreduce ['] monadic-op ;
	: . ['] apl-product ['] dyadic-op ;

		\ And the parenthesis, most simple:
	' close Constant (
	' open Constant )

		\ And switch back to the default wordlist:
set-current

		\ Now, in the beginning of an APL section, we will add the apl wordlist to the search order, and in
		\ the end of section will drop it with "previous":
		: _t [ apl >order ( -1 0 1 ∘ . ⌽ ⊂ 0 1 0 ) continue previous ] ;
		cr _t print				\ [3 1 |[3 |0 0 1 ] [3 |0 1 0 ] [3 |1 0 0 ] ]

		\ In the code above, starting from "apl >order" the words "(", ")" and "." (and others) have APL
		\ meaning, and after "previous" the FORTH meaning restored. Both APL and FORTH parts of the function
		\ compiled into FORTH code; FORTH part by FORTH rules, APL part by APL rules.

		\ As all our APL-syntax function will have the same prefix and postfix, make it a words:
: ←{   apl >order  ['] close  POSTPONE [ ; immediate
: }   open  previous  ] ; immediate

		: _t ←{ -1 0 1 ∘ . ⌽ ⊂ 0 1 0 } ;

		cr _t print				\ [3 1 |[3 |0 0 1 ] [3 |0 1 0 ] [3 |1 0 0 ] ]
		\ Take a moment to decompile the function and look at its FORTH code:
		cr see _t
		\ (two big integers here are pointers to "rotate" and "noop").

		\ So, naturally we've translated APL into FORTH.




		\ Playground

		\ Please note an APL wordlist is just an ordinary FORTH wordlist, so we can extend it incrementally
		\ as we define new functions.

		\ Just to be sure every APL word will go to the apl wordlist, let's add a wordlist switching into 
		\ the definition of "apl:"
		: apl:  ( xt "name" -- )  { xt }  get-current apl set-current  xt apl:  set-current ;

		\ Let's do some experiments, probably memory-consuming, so mark a memory area to be thrown away later:
		marker gc

		\ Some examples from tryapl.org.
		\ The pervasive behavior of addition:
		: _t ←{ 4 2 3 + 8 5 7 } ;
		cr _t print				\ [3 |12 7 10 ]

		\ To implement "fac", we first need to implement "iota". We have "integers" which creates integers
		\ from 0 to n-1 but iota must create integers from 1 to n:
		: iota  integers  1 ['] + curry map ;
		cr 4 iota .a				\ 1 2 3 4

		\ Alternatively, we could implement "iota" in APL:
		' integers  apl: integers
		: iota ←{ 1 + integers } ;
		' iota  apl: ⍳

		: fac { _ } ←{ × / ⍳ ⍵ } ;
		cr 5 fac print				\ 120
		\ The ugly thing above is "{ _ }". This is required to make function argument accessible thru locals.
		\ Of course this can be automated but a chase for perfection would never ends.

		\ To implement "avg", we need a division with correct argument order:
		:noname  swap / ;
		\ Make it pervasive:
		' perv curry
		\ Make it available in APL:
		apl: ÷
		
		: avg { _ } ←{ ( + / ⍵ ) ÷ ≢ ⍵ } ;
		cr 40 30 20 10 4 >a avg print		\ 25

		\ To implement the frequency counter, we need a pseudorandom number generator:
		variable (rnd)
		utime drop (rnd) !
		: rnd   (rnd) @ dup 13 lshift xor dup 17 rshift xor dup dup 5 lshift xor (rnd) ! ;
		cr rnd . rnd . rnd .			\ (3 pseudorandom numbers)

		\ A word to return a pseudorandom in range:
		:noname  ( n -- n )  rnd swap mod 1+ ;
		\ Make it pervasive:
		' uperv curry
		\ Make it available in APL:
		apl: ?

		\ APL rho stays for shape, but in this specific example the constructor "fill" will do:
		' fill apl: ⍴

		: dices ←{ + / ( ⍳ 6 ) ∘ . = ? 1000 ⍴ 6 } ;
		cr dices print				\ [6| (6 numbers, totals to 1000)]
		gc




		\ The Game of Life

: life { _ } ←{ ↑ 1 ⍵ ∨ . ∧ 3 4 = + / , -1 0 1 ∘ . ⊖  -1 0 1 ∘ . ⌽ ⊂ ⍵ } ;

cr ." Memory used by code: " unused0 unused - cell / . ." words"

marker gc
cr ." The glider:" grid show
cr ." The glider after 4 steps:" grid life life life life show  
cr ." Free space on dictionary after a run: " unused .
gc
cr ." Free space on dictionary after gc: " unused .

bye

\ apl-life-anno.fs, Conway's Game of Life in APL in FORTH
\ Copyright (c) 2020 Alexander Serkov

\ This program is free software; you can redistribute it and/or modify it under the terms of
\ the GNU General Public License version 2 as published by the Free Software Foundation.
\ This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
\ without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
\ See the GNU General Public License for more details.

