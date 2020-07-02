#! /usr/bin/gforth

cr ." Available free space on the dictionary: " unused .   unused Constant unused0
cr ." Machine word size: " cell .

\ 1. Arrays
: shape   @ ;
: data   cell+ @ ;
: first   data @ ;
Create [1]  here ,  here cell+ ,  1 ,
defer size   :noname   shape first ;   is size

: array  { data shape -- a }  here  shape , data , ;
here 0 ,  [1] array  Constant [0]
here 1 ,  [0] array  Constant []
: scalar  { u -- a }  here u ,  []  array ;
: vector  { data u -- a }  data  here u ,  [1] array  array ;
: new  { u -- data u }  here  u cells allot  u ;

: end  { a -- addr }  a data  a size cells + ;
: (for)  { a -- and data }  a end  a data ;
: FOR   POSTPONE (for) POSTPONE ?DO ; immediate
: EACH   POSTPONE cell POSTPONE +LOOP ; immediate

: .a   FOR i ? EACH ;
: !a  ( an .. a1 array -- )  FOR i ! EACH ;
: >a  ( an .. a1 u -- a )  new vector { a }  a !a  a ;
: fill  { w u -- a }  u new vector { a }  a FOR w i ! EACH  a ;

: number?   abs 32767 <= ;
: array?   number? 0= ;
: print  { a -- }
	a array? IF ." [" a shape .a ." | " a FOR i @ recurse EACH ." ] " ELSE a . THEN ;

: before  { a n -- a' }  a data  n vector ;
: after  { a n -- a' }  a data n cells +  a size n -  vector ;
: slice  { a from count -- a' }  a from after  count before ;

\ 2. Currying and closures
: literal,  ( w -- )  POSTPONE literal ;
: compile-curried,  { w xt -- }  w literal,  xt compile, ;
: curry  { w xt -- xt' }  :noname  w xt compile-curried,  POSTPONE ; ;
: compose  { xt2 xt1 -- xt' }  :noname  xt1 compile, xt2 compile,  POSTPONE ; ;
: flip  ( xt -- xt' )  ['] swap compose ;

: bind-addr  { addr xt -- xt' }
	:noname  
		addr ['] @ compile-curried,  xt compile,  addr ['] ! compile-curried,
	POSTPONE ; ;
: bind  { w xt -- xt' }  here w , xt bind-addr ;

\ 3. Higher-order collection functions
: !+  { n addr -- addr' }  n addr !  addr cell+ ;
: @+  { addr -- n addr' }  addr @  addr cell+ ;
: iterator  ( a -- xt' )  data  ['] @+ bind ;
: inserter  ( a -- xt' )  data  ['] !+ bind ;

: iter  { xt a -- }  a FOR i @ xt execute EACH ;
: construct  ( u -- a' xt)  new vector dup inserter ;
: shape!  ( a shape -- a)  over ! ;
: shape-as  ( a other -- a )  shape shape! ;

: map  { a xt -- a' }  a size construct  xt compose  a iter  a shape-as ;
: zip  { al ar xt -- a' }
	al size construct  xt compose  al iterator compose  ar iter  al shape-as ;
: inject  { a xt zero -- w }  here { accum } zero ,  accum xt bind-addr  a iter  accum @ ;
: fold  { a xt -- w }  a 1 after  xt  a first  inject ;
: contains  { a w -- f }  a  w ['] = curry  map  ['] or  false inject ;

:noname  { a -- u }  a [1] = IF 1 ELSE a shape  ['] *  1 inject THEN ;   is size

: flat  { a -- a' }  a ['] size map ['] + fold  construct  ['] iter flip curry  a iter ;
: (product)  { w xt al -- }  w xt curry  al iter ;
: product  { al ar xt -- a' }
	al size ar size * construct
	xt compose  al  ['] (product) curry curry  ar iter
	al size ar size 2 >a shape! ;
: rotate  { a offset -- a' }  offset a size mod { n }  a n before  a n after  2 >a  flat ;
: integers  { u -- a }  u construct { xt }  u 0 ?DO i xt execute LOOP ;

: second   data cell+ @ ;
: height  shape first ;
: width  shape second ;
: rows  { a -- a' }
	a height integers  a width ['] * curry map
	a  a width ['] slice curry  flip curry  map ;

\ 4. APL specific functions
: size  { a -- u }     a array? IF a size   ELSE 1  THEN ;
: shape  { a -- a' }   a array? IF a shape  ELSE [] THEN ;
: wrap  { a -- a' }    a array? IF a scalar ELSE a  THEN ;
: unwrap  { a -- a' }  a array? IF a first  ELSE a  THEN ;

: rank   shape shape first ;
: 'recurse   latestxt literal, ; immediate
: depth  { a -- a' }   a array? IF a 'recurse map ['] max 0 inject 1+ ELSE 0 THEN ;
: scalar?   rank 0= ;

: ravel  { a -- a' }  a data  a size vector ;
: uperv  ( a xt -- a' )  over number? IF execute ELSE 'recurse curry map THEN ;

defer perv
: both-numbers?  { al ar -- f }  al number? ar number? and ;
: both-iterable?  { al ar -- f }  al array? ar array? and al rank ar rank = and ;
: pairwise  ( al ar xt -- a' )  ['] perv curry zip ;
: left-iterable?  { al ar -- f }  al array? ar scalar? and ;
: extend  { al ar xt -- a' }  al ar unwrap xt ['] perv curry curry map ;
:noname  { al ar xt -- a' }
	al ar both-numbers? IF al ar xt execute EXIT THEN
	al ar both-iterable? IF al ar xt pairwise EXIT THEN
	al ar left-iterable? 0= IF ar al xt flip ELSE al ar xt THEN extend ;
	latestxt is perv

: perv-+   ['] + perv ;
: perv-and   ['] and perv ;
: perv-or   ['] or perv ;
: perv-*   ['] * perv ;
: apl-=   = 1 and ;   : perv-=   ['] apl-= perv ;

: hrotate  { a u -- a' }  a rows  u ['] rotate curry map  flat  a shape-as ;
: vrotate  { a u -- a' }  a rows  u rotate flat  a shape-as ;
: reduce  ( a xt -- a' )  fold wrap ;
: hreduce  { a xt -- a' }  a rows xt ['] reduce curry map ;
: vreduce  { a xt -- a' }  a rows xt fold ;
: vector?   rank 1 = ;
: hrotate  { a u -- a' }  a u  a vector? IF rotate ELSE hrotate THEN ;
: hreduce  { a xt -- a' }  a xt  a vector? IF reduce ELSE hreduce THEN ;

: inner-product  { * + -- a' }  * zip + reduce ;
: apl-product  { al * + ar -- }
	+ ['] noop = IF al ar * product ELSE al ar * + inner-product THEN ;

0 0 0 0 0 0
0 0 1 0 0 0
0 0 0 1 0 0
0 1 1 1 0 0
0 0 0 0 0 0
0 0 0 0 0 0 36 >a Constant grid
6 6 2 >a  grid !

: show  { a -- }
	a array? 0= IF a . EXIT THEN
	a scalar? IF a unwrap recurse EXIT THEN
	a vector? IF a FOR i @ recurse EACH EXIT THEN
	'recurse ['] cr compose  a rows iter ;

\ 5. APL to FORTH translator
: ⍵   ['] @local0 ['] compile, ; 
: continue  ( token -- )  dup number? IF literal, ELSE execute THEN ;
defer open

: subexpression?   ['] open = ;
: identifier?   ['] compile, = ;
: value?  { t -- f }  t number?  t subexpression? or  t identifier? or ;

: strand  ( .. -- u )  0  BEGIN { t cnt } t value? WHILE t continue cnt 1+ REPEAT t cnt ;
: value   strand  { size }  size 1 > IF size ['] >a compile-curried, THEN ;

: function-ref  { t -- }  literal, ;
: operator  { xt -- }  function-ref  value  xt compile,  continue ;
: dyadic-op   operator ;
: monadic-op   operator ;
: function  { t xt -- }
	t ['] dyadic-op = IF t  xt literal, ELSE t value  xt compile, THEN  continue ;

:noname   value continue ;  is open
: close ;

: apl:  { xt -- }  : xt literal, ['] function literal, POSTPONE ; ;
wordlist Constant apl   get-current   apl set-current
	' first apl: ↑		' perv-or apl: ∨	' perv-and apl: ∧	
	' perv-= apl: =		' perv-+ apl: +		' ravel apl: ,	
	' noop apl: ∘		' vrotate apl: ⊖	' hrotate apl: ⌽	
	' wrap apl: ⊂		' perv-* apl: ×		' size apl: ≢
	: / ['] hreduce ['] monadic-op ;
	: . ['] apl-product ['] dyadic-op ;
	' close Constant (
	' open Constant )
set-current

: ←{   apl >order  ['] close  POSTPONE [ ; immediate
: }   open  previous  ] ; immediate

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

\ apl-life.fs, Conway's Game of Life in APL in FORTH
\ Copyright (c) 2020 Alexander Serkov

\ This program is free software; you can redistribute it and/or modify it under the terms of
\ the GNU General Public License version 2 as published by the Free Software Foundation.
\ This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
\ without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
\ See the GNU General Public License for more details.

