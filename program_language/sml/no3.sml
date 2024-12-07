(*(*this *)
val x=34;
(*this is a comment .*)
val y=56; 
(* 你好*)
val test=12+(if 28>34 then x else 0);
val a=0-5;
val b= ~5;
(*除法 div *)
val c=10 div (0+1);
(*取余 mod *)
val d=10 mod 2;
fun pow(x: int, y :int)=
if  y=0
then 1
else x*pow(x,y-1);
fun cube(x:int)=pow(x,3);
 fun swap (pr :int *bool)=
 (#2 pr ,#1 pr)
 (*(int *int)*(int*int)->int*)
 fun sum_two_pairs (pr1 :int *int,pr2:int *int) =
 (#1 pr1)+(#2 pr1)+(#1 pr2)+(#2 pr2)
 (*int* int -> int *int *)
 fun div_mod(x:int,y:int)=
 (x div y,x mod y)
 (*int *int -> int *int*)
 fun sort_pair(pr :int *int)=
 if (#1 pr)<(#2 pr)
 then pr 
 else (#2 pr ,#1 pr)
  (*NO.3 列表功能*)
fun sum_list (xs: int list)= (*求和*)
if null xs
then 0
else hd xs + sum_list(tl xs) 
fun list_product (xs: int list)= (*求积*)
if null xs
then 1
else hd xs *list_product(tl xs) 
fun countdown(x:int)= (*倒序*)
if x=0
then []
else x::countdown(x-1)
 fun append(xs :int list , ys: int list )=(*合并*)
 if null xs
 then ys
 else (hd xs) :: append((tl xs),ys)
 (*(int list)(int list)->int list*)
 (*functions over pairs of lists*)
 fun sum_pairs_list(xs:(int*int )list )=
 if null xs
 then 0
 else #1 (hd xs)+ #2 (hd xs)+sum_pairs_list(tl xs)
 (*sum_pair_list [(3,4),(5,6)]*)
 fun firsts(xs:(int*int) list)=(*[3,5]*)
 if null xs
 then []
 else (#1 (hd xs))::firsts(tl xs)

 fun seconds(xs:(int*int) list)=(*[3,5]*)
 if null xs
 then []
 else (#2 (hd xs))::seconds(tl xs)
 
 fun sum_pairs_list2(xs:(int*int) list)=(*  *)
 (sum_list(firsts xs)+sum_list(seconds xs))




 (**let表达式**)

fun silly(z:int)=
let val x= if z>0 then z else 34
     val y=x+z+9
 in 
     if x>y then x*2 else y*y
 end;
(*nestfunction嵌套函数*)
fun count_from1(x:int)=(*输入数组*)
let 
fun count (from : int ,x :int)=
if from=x
then x::[]
else from :: count(from+1,x)
in count(1,x)
end

(*let and efficiency*)
fun bad_max(xs:int list)=
if null xs
then 0
else if null (tl xs)
then hd xs
else if hd xs > bad_max(tl xs)
then hd xs 
else bad_max(tl xs)
(*badly named:evaluates to 0 on empty list*)
fun good_max(xs:int list)=
if null xs
then 0
else if null (tl xs)
then hd xs
else 
(*for style, could also use a let-binding for (hd xs)*)
let val tl_ans=good_max(tl xs)
in 
if hd xs > tl_ans
then hd xs
else tl_ans
end

fun countup(from :int ,to :int)=
if from=to
then to::[]
else from::countup(from+1,to)

fun countdown(from :int,to :int)=
if from=to
then to::[]
else from::countdown(from-1,to)


(*option*)
(*badly named: evaluates to 0 on empty list*)
fun old_max(xs: int list )=(*找最大值*)
if null xs 
then 0
else if null (tl xs)
then hd xs
else 
let val tl_ans = old_max(tl xs)
in 
   if hd xs > tl_ans
   then hd xs
   else tl_ans
   end

(*better :return an int option*)
(*fn : int list -> int option*)
fun max1 (xs :int list )=
if null xs
then NONE
else
     let val tl_ans = max1(tl xs)
     in if isSome tl_ans andalso valOf tl_ans > hd xs
     then tl_ans 
     else SOME (hd xs)
     end

fun max2(xs :int list)=
  if null xs
  then NONE
  else let
      fun max_nonempty (xs :int list)=
      if null (tl xs)
      then hd xs
      else let val tl_ans=max_nonempty(tl xs)
           in 
           if hd xs > tl_ans
           then hd xs
           else tl_ans
           end
        in 
          SOME (max_nonempty xs)
 end

 
*)
val my_tuple=(1,"hello",true);

fun is_older(date1: int * int * int, date2: int * int * int) =

  let

    val (y1, m1, d1) = date1

    val (y2, m2, d2) = date2

  in

    y1 < y2 orelse (y1 = y2 andalso m1 < m2) orelse (y1 = y2 andalso m1 = m2 andalso d1 < d2)

  end






