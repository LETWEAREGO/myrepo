(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(*字符串列表处理函数*)

(*
接受字符串列表，返回只包含大写字母的字符串列表
String.sub 函数获取字符串的第n个字符
Char.isUpper 函数判断字符是否大写
List.filter 返回列表元素的函数  
*)
fun only_capitals1(xs:string list)  =
List.filter (fn (v)=> Char.isUpper(String.sub(v,0))) xs


fun only_capitals (xs:string list )=
    List.filter (fn (v)=> Char.isUpper(String.sub(v,0))) xs
(*
foldl 从左向右折叠的高阶函数
  基本形式： foldl f z list z是初始值，list是列表，
  f是函数，f函数接受两个参数，第一个参数是上一次调用f函数的结果，第二个参数是当前元素		
String.size 获取字符长度

fun longest_string3 (xs:string list)=
    foldl (fn(x,y)=> if String.size(x)>=String.size(y) then x else y)"" xs
fun longest_string4 (xs:string list)=
     foldl(fn(x,y)=> if String.size(x)>=String.size(y) then x else y )"" xs

上面的函数和下面的longest_string1和longest_string2是一样的

*)
fun longest_string1 (xs : string list) =
    foldl (fn(x,y) => if String.size(x)>=String.size(y) then x else y ) "" xs
fun longest_string2 (xs : string list) =
    foldl (fn(x,y) => if String.size(x)>=String.size(y) then x else y ) "" xs

(*
	longest_string_helper、longest_string3和longest_string4函数：
	longest_string3与longest_string1行为相同，
	longest_string4与longest_string2行为相同。
	longest_string_helper类型为(int * int -> bool) -> string list -> string，
	更通用，接受一个函数作为参数，根据传入不同比较函数实现不同行为，
	longest_string3和longest_string4通过对longest_string_helper的部分应用定义。
      	


*)
fun longest_string_helper f( li: string list) s=
foldl(fn(x,y)=>if f(String.size(x),String.size(y)) then x else y) s li  
fun longest_string3  (xs: string list)=
    longest_string_helper(fn(a:int,b:int)=>if a>b then true else false) xs""
    
fun longest_string4(xs:string list)=
    longest_string_helper(fn(a:int,b:int)=> if a>=b 
	                       then true else false) xs ""


(* 
val-binding 值绑定 
ml库的o运算

ai的解释
1. `is_capitalized`函数用于判断一个字符串是否以大写字母开头。
2. `filter_capitalized`通过`List.filter`和`is_capitalized`组合，
       用于从字符串列表中筛选出以大写字母开头的字符串。
3. `longest_string_helper`是一个辅助函数，它接受一个比较函数`f`，
并使用`foldl`来找到列表中的最长字符串（根据比较函数`f`的规则）。
4. 最后，`longest_capitalized`通过组合`longest_string_helper`
（传入`>`作为比较函数）和`filter_capitalized`来实现从输入字符串列表中
找到以大写字母开头的最长字符串的功能。如果没有符合条件的字符串，
根据`foldl`的初始值`""`，会返回空字符串。
在处理平局情况时，由于`longest_string_helper`中`foldl`的比较逻辑（`>`），
会返回最接近列表开头的最长字符串（与`longest_string1`的平局处理逻辑一致）。

*)
fun is_capitalized s = Char.isUpper(String.sub(s, 0))
val filter_capitalized = List.filter is_capitalized
fun longest_string_helper f =
    foldl (fn (x, y) => if f(String.size x, String.size y) then x else y) ""
val longest_capitalized = longest_string_helper (op >) o filter_capitalized 
(*


*)
 fun rev_string1 str =
    let
        val char_list = String.explode str
        val reversed_char_list = rev char_list
        val reversed_str = String.implode reversed_char_list
    in
        reversed_str
    end

fun rev_string str=(implode o rev o explode )str

(*part 2*)

fun first_anwer ()=












