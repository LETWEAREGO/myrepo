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
*)    
fun broken_sum xs =
   case xs of
     []=> 0
    |x::xs' => x+(broken_sum x)






(*          *)





    






