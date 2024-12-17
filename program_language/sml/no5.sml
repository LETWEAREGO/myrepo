


(*)
(*作为参数的函数*)
fun increment_n_times_lame(n,x)=
if n=0
then x
else 1+increment_n_times_lame(n-1,x)

fun double_n_times_lame(n,x)=
if n=0
then x
else 2*double_n_times_lame(n-1,x)
   
fun nth_tail_lame(n,xs)=(*example 3,[1,2,3,4]-> 4*)
if n=0
then xs
else tl(nth_tail_lame(n-1,xs))

(*
2,[2 3 4]
1,[3,4]
0,[4]
*)
fun n_times (f,n,x) = 
    if n=0
    then x
    else f (n_times(f,n-1,x))

fun increment x = x+1

fun double x = x+x

val x1 = n_times(double,4,7)
val x2 = n_times(increment,4,7)
val x3 = n_times(tl,2,[4,8,12,16]) 
fun addition(n,x) =n_times(increment,n,x)
fun double_n_times(n,x)=n_times(double,n,x)
fun nth_tail(n,x)=n_times(tl,n,x)

fun triple x=3*x
fun triple_n_times(n,x)=n_times(triple,n,x)
(*3,1 ;2,3;1,9;0,27*)
*)





(*
(*多态类型和函数作为参数*)
(*higher-order functions are often polymorphic based on "Whatever type of function is passed " but not always:*)
(*note :a better  implementation would be tail-recurisive*)
fun times_until_zero(f,x)=
 if x=0 then 0 else 1+times_until_zero(f,f x)
 (*f(f(f(f...x)))*)
 (*(int-> int )* int -> int *)
 (*conversely ,some polymorphic functions that are not higher-order*)
 (*'a list -> int*)
 fun len xs =
             case xs of 
             [] =>0
             |_ ::xs' =>1+len xs'

*)




(*)
(*anonymous-functions 匿名功能 *)
(*programing languages ,Dan Grossman ,Jan-Mar 2013  *)
(*Section 3: Anonymous Functions*)
fun n_times (f,n,x) =
     if n=0
     then x
     else f(n_times(f,n-1,x))
    
fun triple x=3*x
fun triple_n_times(n,x) =n_times(triple,n,x)

fun triple_n_times1 (n,x)=
 n_times(let fun triple x=3*x in triple end ,n,x)     

fun triple_n_times2 (n,x)=
 n_times((fn x=>3*x),n,x)     

fun triple_n_times3 (n,x)=
 n_times((fn  y=>3*y),n,x)     
(*
 匿名函数是没有名字的函数
 匿名函数是函数的函数
 匿名函数是函数的参数
 匿名函数是函数的返回值
 匿名函数是函数的参数
递归函数需要用名字调用
 匿名函数不需要名字调用

*)
*)



(*
(*unnecessary Function warpping 不必要的函数包*)

fun n_times (f,n,x)=
    if n=0
    then x
    else  f(n_times(f,n-1,x))
    

fun nth_tail(n,xs)=n_times(tl,n,xs)
(*)    n_times((fn y=> t1 y),n,xs)*)
fun rev xs =fn xs => list.rev xs
val  rev =fn xs=>list.rev xs
val rev =list.rev
*)



(*map-and-filter地图和过滤器*)
(*
fun map(f,xs)=
    case xs of 
    []=> []
    |x::xs' =>(f x)::map(f,xs')

val x1=map((fn x=>x+1),[4,8,12,16])
val x2=map(hd,[[1,2],[3,4],[5,6,7]])

fun filter(f,xs)=
    case xs of 
    []=>[]
    |x::xs'=> if f x
              then x::(filter(f,xs'))
              else filter(f,xs')
fun is_even v=  (v mod 2=0)
fun all_even xs=filter(is_even,xs)
fun all_even_snd xs = filter((fn (_,v)=>is_even v),xs)
*)





(*归纳先前的主题 generalizing-prior-topics*)
(*
fun double_or_triple f=
   if f 7
   then fn x=>2*x(*fn x=>2*x 是一个匿名函数*)
   else fn x=>3*x
(* (int -> bool)-> (int -> int) *)
val double=double_or_triple(fn x=>x -3=4)
val  nine=(double_or_triple(fn x=>x=42)) 3
(*Higher-order functions over our own datatype bindings*)
datatype exp =Constant of int 
              | Negate of  exp
              | Add of exp*exp
              | Multiply of exp*exp

fun true_of_all_constants(f,e)=
    case e of
    Constant i=> f i
    |Negate e1 =>true_of_all_constants(f,e1)
    |Add(e1,e2)=>true_of_all_constants(f,e1) 
                 andalso true_of_all_constants(f,e2)
    |Multiply(e1,e2)=>true_of_all_constants(f,e1)
                    andalso true_of_all_constants(f,e2)

fun  all_even e = true_of_all_constants(( fn i=> i mod 2=0),e)
*)





(*词汇范围  *)
(*
(* Programming Languages, Dan Grossman *)
(* Section 3: Lexical Scope *)
(* 1 *) val x = 1
(* 2 *) fun f y = x + y
(* 3 *) val x = 2
(* 4 *) val y = 3
(* 5 *) val z = f (x + y)
*)

(*词法范围和高阶函数  *)
(*
(*first example*)
val x=1
fun f y=
    let 
         val x=y+1
    in 
        fn z=>x+y+z 
    end
 val x=3 (*irrelevant 无关急要的*)
 val g=f 4
 val y=5
 val z=g 6
(*second example*)
fun f g=
    let 
         val x=3
    in 
        g 2
    end 
val x=4
fun h y=x+y
val z=f h    
*)


(*为什么是词汇范围 *)
(*
(* Section 3: Why Lexical Scope *)
(* f1 and f2 are always the same, no matter where the result is used *)
fun f1 y =
    let 
	val x = y + 1
    in
	fn z => x + y + z
    end
fun f2 y =
    let 
	val q = y + 1
    in
	fn z => q + y + z
    end
val x = 17 (* irrelevant *)
val a1 = (f1 7) 4
val a2 = (f2 7) 4
(*f3 and f4 are always the same ,no matter what argument is passed in*)
fun  f3 g=
     let 
         val x=3
    in 
        g 2
    end
fun f4 g=
  g 2
  val x=17
  val a3= f3 (fn y=>x+y)
  val a4= f4 (fn y=>17+y)  
(* under dynamic scope, the call "g 6" below would try to add a string
(from looking up x) and would have an unbound variable (looking up y),
even though f1 type-checked with type int -> (int -> int) *)
val x = "hi"
val g = f1 7
val z = g 4  
(* Being able to pass closures that have free variables (private data)
   makes higher-order functions /much/ more useful *)
fun filter (f,xs) =
    case xs of
	[] => []
      | x::xs' => if f x then x::(filter(f,xs')) else filter(f,xs')
fun greaterThanX x = fn y => y > x
fun noNegatives xs = filter(greaterThanX ~1, xs)
fun allGreater (xs,n) = filter (fn x => x > n, xs)
*)

(*关闭和重新计算 *)
(*
fun filter (f,xs)=
    case xs of 
        []=>[]
        |x::xs'=> if f x then x::(filter(f,xs'))else filter(f,xs')

fun allShorterThan1 (xs,s)=
    filter(fn x=> String.size x< (print "!" ; String.size s),xs)

fun allShorterThan2 (xs,s)=
    let 
        val i=(print "!"; String.size s)
    in
        filter(fn x=>String.size x<i,xs)
    end

val _=print"\nwithAllShorterThan1:"
val x1=allShorterThan1(["1,","333","22","4444"],"xxx")
val _=print"\nwithAllShorterThan2:"
val x2=allShorterThan2(["1,","333","22","4444"],"xxx")
val _=print"\n"
*)



(*折叠和更多闭合*)
(*Another hall-of-fame higher-order function *)
(*note this is "fold left" if order matters
  can also do " fold right"*)
(*
fun fold (f,acc,xs)=
case xs of 
    []=> acc
    |x::xs => fold (f,f(acc,x),xs)
(*example not using private data*)
fun f1 xs =fold((fn(x,y)=> x+y),0,xs) (*sum list*)
(* are all list element non-negative*)
fun f2 xs =fold((fn(x,y)=> x andalso y>=0),true ,xs)    
(*example using private data*)

(**)
fun f3 (xs,hi,lo)=
    fold((fn(x,y)=>
            x+(if y>=lo andalso y<=hi 
                then 1
                else 0)),
                0,xs)

fun f4(xs,s)=
    let 
        val i=String.size s 
    in
        fold((fn(x,y)=>x andalso String.size y<i),true,xs)
    end

fun f5 (g,xs)= fold ((fn (x,y)=>x andalso g y),true ,xs)                
fun f4again (xs,s)=
    let 
        val i=String.size s 
    in 
        f5(fn y=>String.size y<i ,xs)
    end
*)
                
(*封闭成语：组合函数*)
(*
fun compose (f,g) = fn x=> f (g x)
fun sqrt_of_abs i =Math.sqrt(Real.fromInt(abs i))
fun sqrt_of_abs i =(Math.sqrt o Real.fromInt o abs)i 
val sqrt_of_abs =Math.sqrt o Real.fromInt o abs
val sqrt_of_abs =Math.sqrt o Real.fromInt o abs
fun backup1 (f,g) =
    fn x=> case f x of 
           NONE => g x
           |SOME y=> y 
(*fun backup1 (f,g) = fn x => case f x of NONE => g x | SOME y => y*)
fun backup2 (f,g) = fn x => f x handle _ => g x
*)


(*闭塞成语currying*)
(*
(*old way to get the effect of multiple arguments*)
 fun sorted3_tupled (x,y,z)=z>=y andalso y>=x
 val t1=sorted3_tupled(7,9,11)
(*new way:currying*) 
fun sorted3 x = fn y => fn z => z>=y andalso y>=x
(*alternately: fun sorted3 x= fn y => fn z => z>=y andalso y>=x*)
val t2 =(( sorted3  7 )9) 11
(*systactic sugar for calling curried functions :optional parenteses*)
val t3 = sorted3 7 9 11
(*syntactic sugar for defining curried functions: space between arguments*)
fun sorted3_nicer x y z = z>=y andalso y>=x
(*more calls that work:*)
val t4 = sorted3_nicer 7 9 11
val t5 = ((sorted3_nicer 7) 9) 11
(*calls that do not work : cannot mix tupling and currying*)
(*a more useful example*)
fun  fold f acc xs =(*means fun fold f =fn acc=> fn xs =>*)
  case xs of 
   [] => acc
   |x:: xs' => fold f (f(acc,x)) xs'
fun  sum xs = fold (fn (x,y)=> x+y ) 0 xs
*)

(*部分应用 partial application *)
(*    
fun sorted3 x y z=z >= y andalso y>=x
fun fold f acc xs=(*means fun fold f =fn acc => fn xs =>*)
case xs of 
[] => acc
|x::xs' => fold f (f(acc,x)) xs'
(*if a curried function is appied to "too few" agruments,that just returns 
   a closure, which is often useful -- a powerful idiom (no new semantics)*)

val is_nonnegative =sorted3 0 0
val sum = fold (fn( x,y)=> x+y) 0
(*in fact ,not doing this often a harder-to-notice version of 
  unnecessary function warpping ,as in these inferior versions*)

fun is_nonnegative_inferior x = sorted3 0 0 x
fun sum_inferiorn xs=fold (fn (x,y) => x+y) 0 xs
(*anthor example *)
fun range i j= if i>j then [] else i::range (i+1) j
(*range 3 6 [3,4,5,6]*)
val countup =range 1
(*range 6 [1,2,3,4,5,6]*)
fun countup_inferior x =range 1 x
(*common style is to curry higher-order functions with function arguments
  first to enable convenient partial application*)
  fun exist predicate xs=
      case xs of 
      [] => false
      |x::xs' => predicate x orelse exist predicate xs'

val no =exist(fn x=> x=7 )[4,11,23]
val hasZero=exist(fn x=> x=0)
val incrementAll=List.map(fn x=>x+1)
(*library functions foldl,list.filter,etc,also generally curried:*)
val removeZero =List.filter(fn x=> x<>0)
(* but if you get a strange message about "value restriction", just put back
   in the actually-necessary wrapping or an explicit non-polymorphic type *)
(* does not work for reasons we will not explain here (more later) *)
(* (only an issue will polymorphic functions) *)
(* val pairWithOne = List.map (fn x => (x,1)) *)
(* workarounds: *)
fun pairWithOne xs =List.map(fn x=>(x,1)) xs
val pairWithOne : string list ->(string*int )list =List.map(fn x=>(x,1))
(*this different function works fine because result is not polymorphic *)
val incrementAndPairWithOne =List.map(fn x=>(x+1,1))
*)

(*currying wrapup咖喱总结 *)
(*genertic functions to switch how/whether currying is used*)
(*in each case ,the type tells you a lot *)
fun curry f x y= f(x,y)
fun uncurry f(x,y)= f x y 
fun other_curry1 f = fn x=> fn y=> f y x
fun other_curry2 f  x y = f y x
(*example *)
(*tupled but we wish it were curried*)
fun range (i,j)=if i>j then [] else i::range(i+1,j)
(*no problem*)
val  countup = curry range 1
val xs =countup 7



