(*section4: what is type inference*)
fun f x=(*infer val f: int-> int*)
   if x>3
   then 42
   else x*2
   (*
   fun g x=(*report type error*)
   if x>3
   then true
   else x*2
   *)



(*ml type inference*)
val x=42(*val x: int *)
fun f(y,z,w)=
    if y(*y must be bool*)
    then z+x (*z must be int*)
    else 0 (*both branches have same type*)
 (*f must return an int
   f must take a bool *int *ANYTHING
   so val f:bool *int * 'a ->int
 *)   




(*type inference example *)
(*
f: T1->T2 [must be a function ;all functions take one argument]
x: T1 [must be a value]
y:T3
z：T4
T1 = T3 * T4 [else pattern-match in val-binding doesn't type-check ]
T3 = int [because (abs y) where abs : int ->int]
T4 = int [beacuse add z to a int]
So T1 = int* int
So (abs y )+ z: int, so let-expression :int ,so body :int ,so T2=int
So f : int * int -> int
*)
fun f x=
   let val (y,z)= x in
          (abs y)+z
    end

(*
sum: T1 - T2 [must be a function : all functions take one argument]
xs : T1 [must have type of f's argument]

x : T3 [pattern match against T3 list]
xs': T3 list [pattern match against T3 list]

T1 =T3 list [else pattern-match on doesn't type-check]
0: int,so case-expression :int ,so body :int,so T2=int
T3 = int [because x :T3 and argument to addition]
T2 = int [because result of recursive call is argument to addition]
sum xs' type-checks because xs' has type T3 lsit and T1 =T3 list
case-expression  type-checks because both branches have type int

from T1 =T3 list and T3 =int , we know T1= int list
from that and T2 = int ,we know f : int list -> int
*)

fun sum xs =
   case xs of 
     []=> 0
    |x::xs' => x+(sum xs')


(*
type inference proceeds exactly like for sum for most of it:
broken_sum :T1 -T2 [must be a function : all functions take one argument]
xs : T1 [must have type of f' argument]

x:T3[pattern match against T3 list]
xs':T3 list [pattern match against T3 list]

T1=T3 list [else pattern-match on doesn't type-check]
0:int,so case-expression :int,so body :int,so T2=int
T3=int [because x:T3 and argument to addition]
T2=int [because result of recursive call is argument to addition]

but now to type-check (broken_sum x), we need T3 =T1 and T1=T3 list
so we need T3 =T3 list ,which is impossible for any T3

Note :the actual type-checker might gather facts in a different order and
therefore report a different error , but it will report an error.
    
fun broken_sum xs =
   case xs of
     []=> 0
    |x::xs' => x+(broken_sum x)

 *)
(*
来自课程的视频

多态实例*)
fun length xs=
    case xs of 
         []=>0
      | x::xs'=>1+(length xs');

(*x,y的类型必须一样*)
fun f(x,y,z)=
if true
then (x,y,z)
else (z,y,x);
(*
函数组合 
compose:T1 * T2-> T3
f:T1
g:T2
x:T4
body being a function has type T3=T4->T5
from g being passed x,T2=T4->T6 for some T6
from f being passed the result of g,T1=T6->T7 for some T7
from call to f being body of anonymous function T7=T5 
put it all together T1=T6->T5,T2=T4->T6, T3=T4->T5
                    f =  ->  ,g =x ->  ,   =x-> 
                      (T6->T5)*(T4->T6)->(T4->T5)
                      ('a->'b)*('c->'a)->('c->'b)
*)
fun compose (f,g)=fn x => f(g x);

(*相互递归 mutual_recursion  *)
fun match xs=
    let fun s_need_one xs=
            case xs of
                 [] => true
               | 1::xs' => s_need_two xs'
               | _ => false
         and s_need_two xs=
             case xs of
                  []=> false
               | 2::xs'=>s_need_one xs'
               | _ => false
      in 
         s_need_one xs
      end ;
(*
在 ML 语言中，相互递归可以正常工作，
前提是可以将这些函数放在彼此相邻的位置。否则，就会有一些变通方法。
*)

datatype t1 =Foo of int | Bar of t2 
and t2 =Baz of string |Quux of t1;

fun no_zeros_or_empty_strings_t1 x=
    case x of
         Foo i=> i<>0
         |Bar y=> no_zeros_or_empty_strings_t2 y
    and no_zeros_or_empty_strings_t2 x=
        case x of
            Baz s=>size s>0
            |Quux y=>no_zeros_or_empty_strings_t1 y;

(* *)
fun no_zeros_or_empty_strings_tl_alternate(f,x)=
    case x of 
         Foo i=> i<> 0
      |  Bar y=> f y

fun no_zeros_or_empty_strings_t2_alternate x=
    case x of 
         Baz s=> size s>0
       | Quux y=> no_zeros_or_empty_strings_tl_alternate (no_zeros_or_empty_strings_t2_alternate ,y);
fun doubler y=y+y
(*名称空间模块*)
structure MyMathLib =
struct 
fun fact x=
    if x=0
   then 1
   else x*fact(x-1)

val half_pi=Math.pi/2.0
fun doubler y=y+y
end;
val pi=MyMathLib.half_pi+MyMathLib.half_pi;
val twenty_eight =MyMathLib.doubler 14;

(*签名和藏东西*)
signature  MATHLIB =
sig
  val fact:int->int
  val half_pi:real
end;
structure MyMathLib :>MATHLIB=
struct 
fun fact x=
    if x=0
    then 1
    else x*fact(x-1)

val half_pi=Math.pi/2.0

fun doubler y=y+y 
end;
val pi=MyMathLib.half_pi+MyMathLib.half_pi;
 
 
 (*模块实例*)

structure Rational1=
struct 
(*


*)
datatype rational= Whole of int | Frac of int*int
exception BadFrac
(*
gcd and reduce help keep fractions reduced,
but they clients need not know about them
*)
(*
they _assume_ their inputs are not negative
*)
fun gcd (x,y)=
     if x=y 
     then x
     else if x<y
     then gcd(x,x-y)
     else gcd(y,x)
fun reduce r =
       case r of
	   Whole _ => r
	 | Frac(x,y) => 
	   if x=0
	   then Whole 0
	   else let val d = gcd(abs x,y) in (* using invariant 1 *)
		    if d=y 
		    then Whole(x div d) 
		    else Frac(x div d, y div d) 
		end;

(*构造分数，禁止分母为0*)
fun make_frac(x,y)=
    if y=0
    then raise BadFrac
    else if y<0
    then reduce(Frac(~x,~y))
    else reduce(Frac(x,y));
    
(* 利用数学性质，假设这些不变量对参数成立，那么结果也满足这两个不变量。*)
fun add(r1,r2)=
    case (r1,r2) of
         (Whole(i),Whole(j))=>Whole(i+j)
         |(Whole(i),Frac(j,k))=>Frac(j+k*i,k)
         |(Frac(j,k),Whole(i))=>Frac(j+k*i,k)
         |(Frac(a,b),Frac(c,d))=>reduce(Frac(a*d+b*c,b*d))
fun toString r=
    case r of 
         Whole i=>Int.toString i
        |Frac(a,b)=>(Int.toString a)^"/"^(Int.toString b)
        
end;      








(* Section 4: An Equivalent Structure *)

(* this signature hides gcd and reduce.  
That way clients cannot assume they exist or 
call them with unexpected inputs. *)
signature RATIONAL_A = 
sig
datatype rational = Frac of int * int | Whole of int
exception BadFrac
val make_frac : int * int -> rational
val add : rational * rational -> rational
val toString : rational -> string
end

(* the previous signature lets clients build 
 any value of type rational they
 want by exposing the Frac and Whole constructors.
 This makes it impossible to maintain invariants 
 about rationals, so we might have negative denominators,
 which some functions do not handle, 
 and print_rat may print a non-reduced fraction.  
 We fix this by making rational abstract. *)
signature RATIONAL_B =
sig
type rational (* type now abstract *)
exception BadFrac
val make_frac : int * int -> rational
val add : rational * rational -> rational
val toString : rational -> string
end
	
(* as a cute trick, it is actually okay to expose
   the Whole function since no value breaks
   our invariants, and different implementations
   can still implement Whole differently.
*)
signature RATIONAL_C =
sig
type rational (* type still abstract *)
exception BadFrac
val Whole : int -> rational 
   (* client knows only that Whole is a function *)
val make_frac : int * int -> rational
val add : rational * rational -> rational
val toString : rational -> string
end 

(* this structure can have all three signatures we gave
   Rationa1 in previous segments, and/but it is 
   /equivalent/ under signatures RATIONAL_B and RATIONAL_C 

   this structure does not reduce fractions until printing
*)
       
structure Rational2 :> RATIONAL_A (* or B or C *) =
struct
  datatype rational = Whole of int | Frac of int*int
  exception BadFrac

   fun make_frac (x,y) =
       if y = 0
       then raise BadFrac
       else if y < 0
       then Frac(~x,~y)
       else Frac(x,y)

   fun add (r1,r2) = 
       case (r1,r2) of
	   (Whole(i),Whole(j))   => Whole(i+j)
	 | (Whole(i),Frac(j,k))  => Frac(j+k*i,k)
	 | (Frac(j,k),Whole(i))  => Frac(j+k*i,k)
	 | (Frac(a,b),Frac(c,d)) => Frac(a*d + b*c, b*d)

   fun toString r =
       let fun gcd (x,y) =
	       if x=y
	       then x
	       else if x < y
	       then gcd(x,y-x)
	       else gcd(y,x)

	   fun reduce r =
	       case r of
		   Whole _ => r
		 | Frac(x,y) => 
		   if x=0
		   then Whole 0
		   else
		       let val d = gcd(abs x,y) in 
			   if d=y 
			   then Whole(x div d) 
			   else Frac(x div d, y div d) 
		       end
       in 
	   case reduce r of
	       Whole i   => Int.toString i
	     | Frac(a,b) => (Int.toString a) ^ "/" ^ (Int.toString b)
       end
end;
    

(* Programming Languages, Dan Grossman *)
(* Section 4: Another Equivalent Structure *)

(* this signature hides gcd and reduce.  
That way clients cannot assume they exist or 
call them with unexpected inputs. *)
signature RATIONAL_A = 
sig
datatype rational = Frac of int * int | Whole of int
exception BadFrac
val make_frac : int * int -> rational
val add : rational * rational -> rational
val toString : rational -> string
end

(* the previous signature lets clients build 
 any value of type rational they
 want by exposing the Frac and Whole constructors.
 This makes it impossible to maintain invariants 
 about rationals, so we might have negative denominators,
 which some functions do not handle, 
 and print_rat may print a non-reduced fraction.  
 We fix this by making rational abstract. *)
signature RATIONAL_B =
sig
type rational (* type now abstract *)
exception BadFrac
val make_frac : int * int -> rational
val add : rational * rational -> rational
val toString : rational -> string
end
	
(* as a cute trick, it is actually okay to expose
   the Whole function since no value breaks
   our invariants, and different implementations
   can still implement Whole differently.
*)
signature RATIONAL_C =
sig
type rational (* type still abstract *)
exception BadFrac
val Whole : int -> rational 
   (* client knows only that Whole is a function *)
val make_frac : int * int -> rational
val add : rational * rational -> rational
val toString : rational -> string
end 

(* this structure uses a different abstract type.  
   It does not even have signature RATIONAL_A.  
   For RATIONAL_C, we need a function Whole.  
*) 
structure Rational3 :> RATIONAL_B (* or C *)= 
struct 
   type rational = int * int
   exception BadFrac
	     
   fun make_frac (x,y) = 
       if y = 0
       then raise BadFrac
       else if y < 0
       then (~x,~y)
       else (x,y)

   fun Whole i = (i,1)

   fun add ((a,b),(c,d)) = (a*d + c*b, b*d)

   fun toString (x,y) =
       if x=0
       then "0"
       else
	   let fun gcd (x,y) =
		   if x=y
		   then x
		   else if x < y
		   then gcd(x,y-x)
		   else gcd(y,x)
	       val d = gcd (abs x,y)
	       val num = x div d
	       val denom = y div d
	   in
	       Int.toString num ^ (if denom=1 
				   then "" 
				   else "/" ^ (Int.toString denom))
	   end
end


(* Programming Languages, Dan Grossman *)
(* Section 4: Different Modules Define Different Types *)

signature RATIONAL_A = 
sig
datatype rational = Frac of int * int | Whole of int
exception BadFrac
val make_frac : int * int -> rational
val add : rational * rational -> rational
val toString : rational -> string
end

signature RATIONAL_B =
sig
type rational (* type now abstract *)
exception BadFrac
val make_frac : int * int -> rational
val add : rational * rational -> rational
val toString : rational -> string
end

signature RATIONAL_C =
sig
type rational (* type still abstract *)
exception BadFrac
val Whole : int -> rational (* client knows only that Whole is a function *)
val make_frac : int * int -> rational
val add : rational * rational -> rational
val toString : rational -> string
end 

structure Rational1 :> RATIONAL_C = 
struct

(* Invariant 1: all denominators > 0
   Invariant 2: rationals kept in reduced form *)

  datatype rational = Whole of int | Frac of int*int
  exception BadFrac

(* gcd and reduce help keep fractions reduced, 
   but clients need not know about them *)
(* they _assume_ their inputs are not negative *)
  fun gcd (x,y) =
       if x=y
       then x
       else if x < y
       then gcd(x,y-x)
       else gcd(y,x)

  fun reduce r =
      case r of
	  Whole _ => r
	| Frac(x,y) => 
	  if x=0
	  then Whole 0
	  else let val d = gcd(abs x,y) in (* using invariant 1 *)
		   if d=y 
		   then Whole(x div d) 
		   else Frac(x div d, y div d) 
	       end

(* when making a frac, we ban zero denominators *)
  fun make_frac (x,y) =
      if y = 0
      then raise BadFrac
      else if y < 0
      then reduce(Frac(~x,~y))
      else reduce(Frac(x,y))

 (* using math properties, both invariants hold of the result
   assuming they hold of the arguments *)
  fun add (r1,r2) = 
      case (r1,r2) of
	  (Whole(i),Whole(j))   => Whole(i+j)
	| (Whole(i),Frac(j,k))  => Frac(j+k*i,k)
	| (Frac(j,k),Whole(i))  => Frac(j+k*i,k)
	| (Frac(a,b),Frac(c,d)) => reduce (Frac(a*d + b*c, b*d))

(* given invariant, prints in reduced form *)
  fun toString r =
      case r of
	  Whole i => Int.toString i
	| Frac(a,b) => (Int.toString a) ^ "/" ^ (Int.toString b)

end

structure Rational2 :> RATIONAL_C =
struct
  datatype rational = Whole of int | Frac of int*int
  exception BadFrac

   fun make_frac (x,y) =
       if y = 0
       then raise BadFrac
       else if y < 0
       then Frac(~x,~y)
       else Frac(x,y)

   fun add (r1,r2) = 
       case (r1,r2) of
	   (Whole(i),Whole(j))   => Whole(i+j)
	 | (Whole(i),Frac(j,k))  => Frac(j+k*i,k)
	 | (Frac(j,k),Whole(i))  => Frac(j+k*i,k)
	 | (Frac(a,b),Frac(c,d)) => Frac(a*d + b*c, b*d)

   fun toString r =
       let fun gcd (x,y) =
	       if x=y
	       then x
	       else if x < y
	       then gcd(x,y-x)
	       else gcd(y,x)

	   fun reduce r =
	       case r of
		   Whole _ => r
		 | Frac(x,y) => 
		   if x=0
		   then Whole 0
		   else
		       let val d = gcd(abs x,y) in 
			   if d=y 
			   then Whole(x div d) 
			   else Frac(x div d, y div d) 
		       end
       in 
	   case reduce r of
	       Whole i   => Int.toString i
	     | Frac(a,b) => (Int.toString a) ^ "/" ^ (Int.toString b)
       end
end

structure Rational3 :> RATIONAL_C = 
struct 
   type rational = int * int
   exception BadFrac
	     
   fun make_frac (x,y) = 
       if y = 0
       then raise BadFrac
       else if y < 0
       then (~x,~y)
       else (x,y)

   fun add ((a,b),(c,d)) = (a*d + c*b, b*d)

   fun toString (x,y) =
       if x=0
       then "0"
       else
	   let fun gcd (x,y) =
		   if x=y
		   then x
		   else if x < y
		   then gcd(x,y-x)
		   else gcd(y,x)
	       val d = gcd (abs x,y)
	       val num = x div d
	       val denom = y div d
	   in
	       Int.toString num ^ (if denom=1 
				   then "" 
				   else "/" ^ (Int.toString denom))
	   end

   fun Whole i = (i,1) 

end

