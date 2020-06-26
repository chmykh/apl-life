#! /usr/bin/gforth

cr ." Available free space on dictionary: " unused .   unused Constant unused0
cr ." Machine word size: " cell .

\ Arrays

: shape   @ ;
: start   cell+ @ ;
: first   start @ ;

Create [1]  here ,  here cell+ ,  1 ,
Create [0]  [1] ,  here cell+ ,  0 ,
Create []  [0] ,

defer size  :noname shape first ;  is size

: array  { data shape -- a }  here { a }  shape , data ,  a ;
: store  { w -- data }  here  w , ;
: vector  { data u -- a }  data  u store  [1] array  array ;
: reserve  { u -- data }  here  u cells allot ;
: new  { u -- data u }  u reserve  u ;

: second   start cell+ @ ;
: scalar  { w -- a }  w store  []  array ;
: rank   shape shape first ;
: end  { a -- addr }  a start  a size cells + ;
: last   end cell - @ ;

: (for)  { a -- end start }  a end  a start ; 
: FOR   POSTPONE (for) POSTPONE ?DO ; immediate
: EACH   POSTPONE cell POSTPONE +LOOP ; immediate

: !a  ( an .. a1 array -- )  FOR i ! EACH ;
: >a  ( an .. a1 u -- a )  new vector { a }  a !a  a ;
: fill  { w u -- a }  u new vector { a }  a FOR w i ! EACH  a ;
: new-matrix  { c r -- a }  r c * reserve  c r 2 >a  array ;
: to-shape  { a other -- a' }  a start  other shape  array ;

: .a  { a -- }  a FOR i ? EACH ;
: is-array?  { a -- f }  a [1] >=  a here <  and ;
: print  { a -- }
	a is-array? IF ." [" a shape .a ." |" a FOR i @ recurse EACH ." ] " ELSE a . THEN ;

: width   shape second ;
: height   shape first ;
: at  { a r c -- addr }  a start r a width * c + cells + ;
: transpose { a -- a' }
	a height a width new-matrix { a' }
	a width 0 ?DO a height 0 ?DO a i j at @  a' j i at ! LOOP LOOP a' ;

\ Currying and closures

: literal,  { w -- }  w POSTPONE literal ;
: compile-curried,  { w xt -- }  w literal,  xt compile, ;
: curry  { w xt -- xt' }  :noname  w xt compile-curried,  POSTPONE ; ;
: compose  { xt2 xt1 -- xt' }  :noname  xt1 compile, xt2 compile,  POSTPONE ; ;
: flip  { xt -- xt' }  xt ['] swap compose ;

: bind-addr  { addr xt -- xt' }
	:noname  
		addr ['] @ compile-curried,  xt compile,  addr ['] ! compile-curried,
	POSTPONE ; ;
: bind  { w xt -- addr xt' }  here { addr } w ,  addr xt bind-addr { xt' }  addr xt' ;
: drop-addr  { addr xt -- xt }  xt ;

\ Higher-order collection functions

: !+  { n addr -- addr' }  n addr !  addr cell+ ;
: @+  { addr -- n addr' }  addr @  addr cell+ ;	
: iterator  { a -- xt' }  a start  ['] @+ bind  drop-addr ;
: inserter  { a -- xt' }  a start  ['] !+ bind  drop-addr ;

: construct  { u -- a' xt }  u new vector { a' }  a'  a' inserter ;

: iter  { xt a -- }  a FOR i @ xt execute EACH ;

: integers  { u -- a }  u construct { xt }  u 0 ?DO i xt execute LOOP ;

: map  { a xt -- a' }  a size construct  xt compose  a iter  a to-shape ;
: zip  { a1 a2 xt -- a' }
	a2 size construct  xt compose  a2 iterator compose  a1 iter  a1 to-shape ;
: (product)  { w xt a1 -- }  w xt curry  a1 iter ;
: product  { a1 a2 xt -- a' }
	a1 size a2 size new-matrix { a' } a' inserter
	xt compose  a1  ['] (product) curry curry  a2 iter  a' ;

: before  { a n -- a' }  a start  n vector ;
: after  { a n -- a' }  a start n cells +  a size n - vector ;
: slice  { a from count -- a' }  a from after  count before ;
: inject  { a xt zero -- w }  zero xt bind  a iter  @ ;
: fold  { a xt -- w }  a 1 after  xt  a first  inject ;
: contains  { a w -- f }  a  w ['] = curry  map  ['] or  false inject ;
: flat  { a -- a' }  a ['] size map ['] + fold  construct  ['] iter flip curry  a iter ;

: rotate  { a offset -- a' }  offset a size mod { n }  a n before  a n after  2 >a  flat ;
: rows  { a -- a' }
	a height integers  a width ['] * curry map
	a  a width ['] slice curry  flip curry  map ;
: columns  { a -- a' }  a transpose rows ;
\ : hrotate  { a offset -- m' }  a columns  offset rotate flat  a to-shape  transpose ; \ ?
: hrotate  { a offset -- m' }  a rows  offset ['] rotate curry map  flat  a to-shape ;
: vrotate  { a offset -- a' }  a rows  offset rotate flat  a to-shape ;

:noname  { a -- u }  a [1] = IF 1 ELSE a shape  ['] *  1 inject THEN ;  is size

\ APL specific behavior

: wrap  { a -- a' }  a is-array? IF a scalar ELSE a THEN ;
: unwrap  { a -- a' }  a is-array? IF a first ELSE a THEN ;
: rank  { a -- u }  a is-array? IF a rank ELSE 0 THEN ;
: 'recurse   latestxt POSTPONE literal ; immediate
: depth  { a -- u }  a is-array? IF a 'recurse map ['] max 0 inject 1+ ELSE 0 THEN ;
: is-scalar?  { a -- f }  a rank 0= ;

: uapply  { a xt -- a' }
	a rank 0 > IF a xt 'recurse curry map EXIT THEN
	a depth 0 > IF a unwrap xt recurse wrap EXIT THEN
	a xt execute ;
: extend  { other a -- a' }  other is-scalar? IF a ELSE a unwrap  other size  fill THEN ;
: apply  { a1 a2 xt -- a' }
	a1 rank 0 > a2 rank 0 > and IF a1 a2 xt 'recurse curry zip EXIT THEN
	a1 rank 0 > a2 rank 0 > or IF a2 a1 extend  a1 a2 extend  xt recurse EXIT THEN
	a1 depth 0 > a2 depth 0 > or IF a1 unwrap a2 unwrap xt recurse wrap EXIT THEN
	a1 a2 xt execute ;

: axis  { a -- a' }  a rank 2 < IF a 1 ELSE a columns  a rows  2 THEN >a ;
: ravel  { a -- a' }  a start  a size vector ;
: (reduce)  { a xt -- a' }  a xt fold wrap ;
: reduce  { a xt -- a' }  a axis last xt (reduce) ;
: inner-product  { * + -- a' }  * zip + reduce ;
: apl-product  { a aa ww w -- }
	ww ['] noop = IF a w aa product ELSE a w aa ww inner-product THEN ;

: uperv:  ( xt "name" -- )  Create , DOES> @ uapply ;
: perv:  ( xt "name" -- )  Create , DOES> @ apply ;

' + 			perv: apl-+
:noname = 1 and ;	perv: apl-=
' and 			perv: apl-and
' or 			perv: apl-or
' * 			perv: apl-*

\ APL syntax

defer value
: continue   execute ;
: open   value continue ;
: close ;
: is-value?  { t -- f }  t ['] open =  t ['] literal, = or  t ['] compile, = or ;
: strand  { -- u }  0  BEGIN { t cnt } t is-value? WHILE t execute cnt 1+ REPEAT t cnt ;
: as-array  { size -- }  size 1 > IF size ['] >a compile-curried, THEN ;
:noname   strand as-array ;  is value

: function-name  { t -- }  literal, ;
: dyadic-op  { xt -- }  function-name  value  xt compile,  continue ;
: monadic-op  { xt -- }  function-name  xt compile,  continue ;
: function  { t xt -- }
	t ['] dyadic-op = IF t  xt literal, ELSE t value  xt compile, THEN  continue ;

wordlist Constant apl
: apl:  ( xt xt "name" -- )  Create , , DOES> 2@ ;
: aplf:  ( xt "name" -- )  ['] function apl: ;
get-current apl set-current 
	' first aplf: ↑		' apl-or aplf: ∨	' apl-and aplf: ∧	
	' apl-= aplf: =		' apl-+ aplf: +		' ravel aplf: ,	
	' noop aplf: ∘		' vrotate aplf: ⊖	' hrotate aplf: ⌽	
	' wrap aplf: ⊂		' apl-* aplf: ×		' size aplf: ≢

	' reduce ' monadic-op apl: /
	' apl-product ' dyadic-op apl: .
	' @local0 ' compile, apl: ⍵

	' close Constant (
	' open Constant ) 
set-current
: aplf:  ( xt "name" -- )  { xt }  get-current apl set-current  xt aplf:  set-current ;

: not-}  ( c-addr u -- c-addr u f )  over c@ [char] } <> ;
: is-number?  ( c-addr u -- c-addr u f ) 2dup s>number? nip nip ;
: token  ( c-addr u -- )  is-number? { n? }  POSTPONE [ evaluate ]  n? IF ['] literal, THEN ;
: tokenize  ( -- .. )  apl >order  BEGIN name not-} WHILE token REPEAT 2drop  previous ;
: ←{   ['] close tokenize open ; immediate

\ Playground

marker gc
: test1 ←{ 1 2 3 + 4 ( 5 6 ) 7 } ;
cr ." 1 2 3 + 4 ( 5 6 ) 7 = " test1 print
:noname  swap / ;   perv: apl-/   ' apl-/ aplf: ÷
: avg { _ } ←{ ( + / ⍵ ) ÷ ≢ ⍵ } ;
cr ." 10 20 30 40 avg = " 10 20 30 40 4 >a avg print
:noname  integers  1 ['] + curry map ;   aplf: ⍳
: fac { _ } ←{ × / ⍳ ⍵ } ;
cr ." 5! = " 5 fac print
variable (rnd)
utime drop (rnd) !
: rnd   (rnd) @ dup 13 lshift xor dup 17 rshift xor dup dup 5 lshift xor (rnd) ! ;
:noname  rnd swap mod 1+ ; uperv: apl-roll   ' apl-roll aplf: ?
' fill aplf: ⍴
: dices ←{ + / ( ⍳ 6 ) ∘ . = ? 1000 ⍴ 6 } ;
cr ." dices = " dices print
gc

\ Game of Life

6 6 new-matrix Constant grid
0 0 0 0 0 0
0 0 1 0 0 0
0 0 0 1 0 0
0 1 1 1 0 0
0 0 0 0 0 0
0 0 0 0 0 0 grid !a

: life2 { _ } ←{ ↑ 1 ⍵ ∨ . ∧ 3 4 = + / , -1 0 1 ∘ . ⊖  -1 0 1 ∘ . ⌽ ⊂ ⍵ } ;

: show  { a -- }
	a is-array? 0= IF a . ELSE
	a rank 0= IF a unwrap recurse ELSE
	a rank 1 = IF a FOR i @ recurse EACH ELSE
	'recurse ['] cr compose  a rows iter THEN THEN THEN ;

cr ." Memory used by code: " unused0 unused - cell / . ." words"

marker gc
cr ." The glider:" grid show
cr ." The glider after 4 steps:" grid life2 life2 life2 life2 show  
cr ." Free space on dictionary after a run: " unused .
gc
cr ." Free space on dictionary after gc: " unused .

0 [IF]
grid 
wrap
-1 0 1 3 >a  ' hrotate product
-1 0 1 3 >a  ' vrotate product
ravel
' apl-+ reduce
3 4 2 >a apl-=
1 grid 2 >a  ' apl-and ' apl-or  inner-product
first
show
[THEN]

bye

