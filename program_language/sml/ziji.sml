(*notes on programming *)
 if true then 0 else 1;
val a=1;
~4;(*负数*)
val z: int = 1 (* 某个整数值 *)
val y: int = 5 (* 某个整数值 *)
val zReal: real = Real.fromInt z
val yReal: real = Real.fromInt y
val result: real = zReal + yReal
(*X 的y次方函数*)
(*用sml的交互模式做*)
fun power (x:int ,y:int ):int =if(y=0) then 1 else x*power(x,y-1);
Real.fromInt(power(3,5));

fun double (x:int):int = 2 * x;
fun square (x:int):int = x * x;
fun power (x:int,y:int):int = if (y=0) then 1 else x * power (x,y-1);

(*得到当前的工作目录 *)
OS.FileSys.getDir();
(* 交互界面可以使用
 OS.Process.system "dir";
*)
val p=3141592e~6;
#2 (1,3,0,true);
(*tuple and records 元组和记录*)
 
#y {x=3,y=1,z=true};
(*declarations*)
val x=3;
val y=3+3;
val a=1 val b=2;
let 
 val a=3+4
 val b=8*8
in 
(a*a)-b
end;



(*type declarations*)
type pair_of_ints=int*int;
val a=(3,3);
val a:pair_of_ints=(3,3);
(*定义新的类型*)
datatype color= Red |Blue|Yellow;
val  elmColor=Red;
val groverColor=Blue;
(*
抽象类型 
*)
abstype abs_color = Red | Blue | Yellow
with
   val elmoColor =Red
   val groverColor =Blue
end;


elmColor;
(*local declaration 本地声明*)
local 
    val a1=3
    val b=4
in 
    val x=a1+b
    val y=a1+b
end;

(*pattern matching模式匹配*)
val (x,y)=(3,4);
val {first=x,second=y}={second=4,first=3};
val {second=y,...}={first=3,second=4};

datatype int_or_real = I of int | R of real;
val a= I 3;

let 
   val (x,y)=(4,5)
in 
    x+y
end;


(*fuctions *)
val (x as (y,z),w)=((3,4),5);

fn (x:int)=>x+1;
(fn (x:int)=>x+1) 3;
val add1=fn (x:int)=> x+1;
add1 3;
add1 100;
fun add1(x:int):int=x+1;
(*    *)
fun foo(x:int,y:int,z:int):int=x+y+z;
foo (3,4,5);
(*tuples 元组 作为参数*)
fun sum_prod(x:int,y:int):int *int =(x+y,x*y);
sum_prod(3,4);
(*records 作为参数 *)
fun full_name{first:string,last:string}:string=first^" "^last;
full_name{last="Pucella",first="Riccardo"};
fun add1(x:int):int=x+1;
fun times2 (x:int):int =x*2;
fun  tuple_add1(x:int,y:int):int *int =(add1 (x),add1 (y));
fun  tuple_times2(x:int,y:int):int*int =(times2 (x),times2 (y));
tuple_times2(10,15);
(*函数里面嵌套一个函数，这个函数可以是已经存在的也可以是匿名函数*)
fun tuple_apply(f:int->int,(x,y):int*int):int*int =(f(x),f(y));
tuple_apply(add1,(3,4));
tuple_apply(times2,(10,15));
tuple_apply(fn(x:int)=>x*3+2,(10,15));
 tuple_apply (fn (x:int) => x * 3 + 2, (10,15));

val foo=let 
           val y=3  
        in 
           fn (x:int)=>x+y
        end;
let val y=1000
    in tuple_apply(foo,(10,20))
    end;

 fun tuple_apply2(f:int->int):(int*int)->(int*int)=
    fn (x:int,y:int)=>tuple_apply(f,(x,y));
val tuple_apply_times2=tuple_apply2 times2;
tuple_apply_times2 (3,4);
val tuple_square=tuple_apply2 (fn (x:int)=>x*x);(* 等价于调用tuple_apply2函数*)
tuple_square(7,8);
 
 (* 书的45页
 fun tuple_apply2 (f:int->int) (t:int*int):(int->int) = tuple_apply (f,t);
 val tuple_add1 = tuple_apply2 add1;
tuple_add1(3,4); 
*)
fun add(x:int,y:int):int=x+y;
(*柯里化函数*)
fun add'
 (x:int)(y:int):int=x+y;
add' (3)(5);

(*交互环境里面可以这样
Standard ML of New Jersey [Version 110.99.6.1; 32-bit; October 25, 2024]
- fun a' (x:int)(y:int):int=x+y;
val a' = fn : int -> int -> int
- a' 3;
val it = fn : int -> int
- it 5;
val it = 8 : int
*)


(*polymorphism 多态性*)
fun identity(x:'a):'a=x;
identity 4;

identity (true);
fun curry2 (f:'a * 'b-> 'c)=fn (x:'a)=>fn (y:'b)=>f(x,y);
fun uncurry2 (g:'a ->'b->'c):('a*'b->'c)=
          fn(x:'a,y:'b)=>g x y;
fun curry3 (f:'a *'b*'c->'d):('a->'b->'c->'d)=
  fn (x:'a)=>fn (y:'b)=>fn (z:'c)=>f(x,y,z);

(*
这段代码定义了一个名为“datatype one_or_two_ints”的数据类型，
它可以是“OneInt”类型，包含一个整数；或者是“TwoInts”类型，
包含两个整数组成的元组。
*)
datatype one_or_two_ints=OneInt of int|TwoInts of int*int;
datatype 'a one_or_two = One of 'a | Two of 'a *'a;
 fun howMany (x: 'a one_or_two):int =
     (case (x)
       of (One _) => 1
|  (Two _) => 2);
howMany (One 3);
howMany (Two (true,false));

fun sum (x:int one_or_two):int=
  (case x
    of One a=>a
     | Two (a,b)=>a+b);
 
datatype 'a option =SOME of 'a |NONE;
(*递归 recursion*)
fun sumUpTo (n:int):int=  (*算总值*)
    (case (n)
       of 0=> 0
       | n=>n+sumUpTo(n-1));


fun fib (n:int):int=
   (case (n)
     of  0=>0
     |   1=>1
     |   n=> fib(n-1)+fib(n-2));
fun sumUpTo' (n:int):int =let
    fun sumUpToIter(n:int,acc:int):int=
        (case n
           of 0=>acc
           | _=> sumUpToIter(n-1,acc+n))
    in 
        sumUpToIter(n,0)
    end;

fun fib' (n) =let
  fun fib_iter (0,a,b)=b
    | fib_iter (n,a,b)=fib_iter(n-1,a+b,a)
  in 
      fib_iter(n,1,0)
end; 

(*
相互递归函数
奇，偶函数
*)
fun even (n:int):bool=
if (n=0) then true else odd (n-1)
and odd (n:int):bool=
if (n=0) then false else even(n-1);

datatype 'a mylist=Empty |Element of 'a * 'a mylist;
Element(10,Element(20,Empty));

fun length (l:'a mylist):int =
    (case l
       of Empty=>0
       |Element(x,xs)=>1+length(xs));
(*    *)       
fun sum (l:int mylist):int=
    (case l
       of Empty=>0
       |Element (x,xs)=> x+sum(xs));
(*        *)
fun map (f:'a->'b)(l:'a mylist):'b mylist=
    (case l
       of Empty=>Empty
       |Element (x,xs)=>Element (f(x),map f xs));
(*

Lists

*)
fun map (f:'a->'b) (l:'a list):'b list=
   (case (l)
      of nil =>nil
      | x::xs=> (f(x))::(map f xs));

(*Equality*) 
 

(*References*)
val a= ref(0);
!a;
a:=3;
!a;
fun count (el,list)=
    let 
    val cnt=ref 0
    fun loop []=()
      | loop(x::xs)=(if (x=el) then cnt :=(!cnt)+1 else();
                     loop(xs))
    in
      loop (list);
      !cnt
    end;

fun count2(el ,list)=let
    fun loop([],cnt)=cnt
    |   loop(x::xs,cnt)=if (x=el) then loop(xs,cnt+1) else loop (xs,cnt)
    in 
        loop(list,0)
    end;
(* 示例用法 *)
val list = [1, 2, 3, 2, 4, 2,2];
val element_to_count = 1;
val result = count2 (element_to_count, list);
print (Int.toString result ^ "\n");
       
(*Exceptions  例外情况     *)
exception MyException of string
exception MyException2
(*Notes*)




(*the module system*)
structure RevStack= struct
  type 'a stack ='a list
  exception Empty
  val empty = []
   fun isEmpty (s:'a stack ):bool=
     (case s
        of []=> true 
        | _=> false)
    fun top (s:'a stack):'a=
      (case s
         of []=> raise Empty 
         | x::xs=>x)
    fun pop (s:'a stack):'a stack=
    (case s
    of [] => raise Empty
    | x::xs=>xs)
    fun push (s:'a stack ,x:'a):'a stack=x:: s
    fun rev (s:'a stack): 'a stack=rev (s)
    end;
(*val s : int RevStack.stack = empty;不知道为啥不行

structure AlsoRevStack = Revstack;
*)
structure Foo = struct
  structure Bar = struct
    val x =0
    end
  end;
structure A= struct
  val x=10
  val y=20
end;  
open A;
(*
open的用法 


*)
x+y;
let open A in x+y end;
(*
Queue1的结构体 
定义一个参数为 'a 的队列 
定义一个异常Empty
一个empty的空队列
isEmpty函数判断队列是否为空
enqueue函数用于向队列添加元素
head函数用于获取队列的头部元素
dequeue函数用于从队列中移除元素
*)

structure Queue1 = struct 
  type 'a queue = 'a list
   exception Empty
  val empty =[]
  fun isEmpty (q: 'a queue):bool =
  (case q
    of []=> true 
    |   _=> false)
  fun enqueue (q:'a queue ,e:'a):'a queue=
  (
    case q 
      of []=> [e]
      |(x::xs)=>x::enqueue(xs,e))
fun  head (q:'a queue):'a =
(case q
   of []=> raise Empty
    | (x::xs)=>x)
 fun dequeue (q: 'a queue): 'a queue =
    (case q
      of [] => raise Empty
      | (x::xs) =>xs)
end;
  
(*
Queue2的结构体
定义一个参数为 'a 的队列
这段代码是用某种编程语言可能是函数式编程语言）实现了一个名为 Queue2 的结构体，
内部使用了另一个名为 RevStack 的结构体 S 和一个类型为 ’a queue 的数据类型，
代表队列，它是由两个 S.stack 组成的元组。定义了一些函数，
如判断队列是否为空的 isEmpty 函数、向队列中添加元素的 enqueue 函数、
获取队列头部元素的 head 函数以及从队列中取出头部元素的 dequeue 函数。
如果队列为空时尝试获取头部元素或取出元素会抛出 Empty 异常。
整体实现了一个基本的队列数据结构的操作。
*)
structure Queue2 = struct
  structure S = RevStack
  exception Empty
  type 'a queue = ('a S.stack * 'a S.stack)
  val empty =(S.empty,S.empty)
  fun isEmpty ((inS,outS): 'a queue):bool =
    S.isEmpty (inS) andalso S.isEmpty (outS)
  fun enqueue ((inS,outS): 'a queue ,x: 'a):'a queue =
    if (S.isEmpty(outS))
     then (inS,S.push(outS,x))
    else (S.push (inS,x),outS)
  fun head ((inS,outS): 'a queue):'a =
    if (S.isEmpty(outS))
      then raise Empty
      else S.top(outS)

  fun dequeue ((inS,outS): 'a queue): 'a queue=
    if(S.isEmpty(outS))
      then  raise Empty
    else  let 
       val _ =S.top (outS)
      val xs= S.pop(outS)
     in 
       if (S.isEmpty(xs))
         then (S.empty,S.rev(inS))
       else 
       (inS,xs)
      end
  end;



(*Signatures 签名*)
signature REV_STACK =sig
  type 'a stack
  exception Empty
  val empty: 'a stack
  val isEmpty: 'a stack ->bool
  val push :'a stack * 'a -> 'a stack
  val pop :'a stack -> 'a stack
  val top : 'a stack -> 'a
  val rev : 'a stack -> 'a stack
  end;

  signature STACK =sig
  type 'a stack
  exception Empty
  val empty : 'a stack
  val isEmpty: 'a stack ->bool
  val push : 'a stack * 'a->'a stack
  val pop : 'a stack ->'a stack
  val top : 'a stack -> 'a 
  end;


signature QUEUE =sig
  type 'a queue
  exception Empty
  val empty:'a queue
  val isEmpty:'a queue->bool
  val enqueue:'a queue *'a ->'a queue
  val head :'a queue->'a 
  val dequeue:'a queue->'a queue
  end;

structure Stack :sig
  exception Empty
  type 'a stack
  val empty :'a stack 
  val push :'a stack * 'a-> 'a stack
  val pop: 'a stack->'a stack
  val top : 'a stack ->'a
  end =struct
    type 'a stack='a list
    exception Empty
    val empty=[]
    fun isEmpty (s:'a stack) :bool=
    (case s
       of []=>true
       |_=>false)
    fun top (s:'a stack):'a=
    (case s
       of []=>raise Empty
       | x::xs=>x)
    fun pop (s:'a stack): 'a stack=
    (case s
       of []=>raise Empty
         | x::xs=>xs)
    fun push (s:'a stack ,x:'a):'a stack=x::s
    fun rev (s:'a stack):'a stack =rev(s)
    end;

structure Stack :STACK =RevStack;

structure Stack :STACK =struct 
   type 'a stack='a list
  exception Empty
  val empty=[]
  fun isEmpty(s:'a stack ):bool=
  (case s
     of []=>true
      |_=>false)
  fun top (s:'a stack):'a=
  (case s
     of []=>raise Empty
      | x::xs=>x)
  fun pop (s:'a stack ):'a stack=
  (case s
     of []=> raise Empty
      | x::xs=>xs)
  fun push (s:'a stack ,x:'a):'a stack=x::s
  fun rev (s:'a stack):'a stack=rev(s)
  end;


structure Stack :STACK =RevStack;

structure A= struct 
  val a=ref(0)
  val b=true 
  end;

structure B: sig val a:int ref end =A;
structure C=struct 
  val a=ref(0)
  val b=true 
  end;

structure D=struct
  val a=A.a
  val b=true 
  end;

structure R=
struct 
   type hidden=int
   val a=10
end;

(*
structure F:sig
   type hidden
   val a :hidden 
   end=E;

 
structure G:>sig
  type hidden 
  val a:hidden 
  end=E;

*)

signature HIDDEN =sig
  type hidden 
  val a :hidden
  end;

signature HIDDEN_IMPL=HIDDEN where type hidden=int;
(*)
structure H':>HIDDEN where type hidden =int=E;
*)
signature HIDDEN2=sig
  type hidden 
   val a:hidden
  end where type hidden=int;


signature HIDDEN2=sig
  include HIDDEN 
  val b:hidden
  end;

signature test=sig
   type s
   type t
   end;


structure Stack2: STACK =struct
  datatype 'a stack =St of (unit ->('a * 'a stack))
  exception Empty
  val empty=St(fn()=>raise Empty)
  fun isEmpty (s:'a stack):bool= let 
  val St(f)=s
  in
   (f(); false) handle _=> true
  end
  fun push (s:'a stack ,x:'a):'a stack=
   St (fn()=>(x,s))
   fun top (s:'a stack):'a =let 
    val St(f)=s 
    val (x,_)=f()
  in
   x 
  end
  fun pop (s:'a stack):'a stack =let
  val St(f)= s 
  val (_,s')=f()
  in 
   s'
   end
   end;

   functor ReVStackFun (structure S:STACK):REV_STACK =struct
     type 'a stack = 'a S.stack
     exception Empty=S.Empty
     val empty=S.empty
     val isEmpty=S.isEmpty
     val push=S.push
     val pop=S.pop
     val top=S.top
     fun rev(s:'a stack):'a stack =let 
       fun pop_all(s:'a stack):'a list =if (S.isEmpty(s))
       then []
       else (S.top(s))::pop_all(S.pop(s))
     fun push_all(l:'a list ,s:'a stack): 'a stack=
      (case l 
         of  []=>s 
         | e::es=>push_all(es,S.push(s,e)))
      in 
      push_all(pop_all(s),S.empty)
      end
      end;
































   























  









  







       









































(*

(*
从官网https://smlfamily.github.io/Basis/char.html#SIG:CHAR.chr:VAL:SPEC开始看
*)
(*string*)
(*检查implode函数是否内置*)
exception UnboundVariable
fun checkDefined id =
  (id (); true) handle _ => false
val isImplodeDefined = checkDefined (fn () => implode []);
val _ = TextIO.print (Bool.toString isImplodeDefined);


(*implode 将字符列表元素转换为一个字符串。*) 
val charList = [#"a", #"b", #"c"];
val str = implode charList;





(*explode 将字符串转换为字符列表。*)
val charList1 = explode str;





(*map f s *) 
val str = "hello";
val charlist = explode str;
val uppercharlist = List.map toUpper charList;
val result = implode uppercharlist;
print(result ^ "\n");
val str = "hello";
(* 使用map结合toUpper函数来处理字符串str，将每个字符转换为大写 *)
val result = implode (List.map toUpper (explode str));
(* 打印输出结果 *)
print (result ^ "\n");






(*translate f s*)(*没有成功*)
(* 定义一个函数，将数字字符转换为对应的中文数字字符 *)
(* 定义一个函数，将数字字符转换为对应的中文数字字符串表示 *)
fun digitToChinese (c : char) =
    case c of
          #"0" => "z"
        | #"1" => "一"
        | #"2" => "二"
        | #"3" => "三"
        | #"4" => "四"
        | #"5" => "五"
        | #"6" => "六"
        | #"7" => "七"
        | #"8" => "八"
        | #"9" => "九"
        | _ => String.str(c);  (* 将非数字字符转换为长度为1的字符串 *)



(* fields f  s*)
     fun is_delimiter (c : char) = c = #" "
     val input_str = "hello world how are you"
     val fields_list = String.tokens is_delimiter input_str     


(*)
(*isSuffix s1 s2*)
(*isSuffix 没有被定义*)

fun isSuffix s1 s2 =
    let
        val len1 = size s1
        val len2 = size s2
    in
        if len1 > len2 then
            false
        else
            substring (s2, len2 - len1, len1) = s1
    end
(* 假设 isSuffix 函数已经被定义 *)
(* 正确的调用方式如下： *)
val result1 = isSuffix "ing" "ighting"; (* 应该返回 true *)
val result2 = isSuffix "" "hello";       (* 应该返回 true *)
val result3 = isSuffix "world" "hello world"; (* 应该返回 true *)
val result4 = isSuffix "world" "hello";    (* 应该返回 false *)
val result5 = isSuffix "abc" "abc";        (* 应该返回 true *)
*)





(*compare (s,t) *)

(*int*)
open Int;
(* 使用 compare 函数比较两个整数 *)
val resultInt1 = compare (10, 20); (* 应该返回 LESS *)
val resultInt2 = compare (30, 30); (* 应该返回 EQUAL *)
val resultInt3 = compare (40, 25); (* 应该返回 GREATER *)
(* 打印比较结果 *)
val _ = print (case resultInt1
                of LESS => "10 is less than 20\n"
                 | EQUAL => "10 is equal to 20\n"
                 | GREATER => "10 is greater than 20\n");
val _ = print (case resultInt2
                of LESS => "30 is less than 30\n"
                 | EQUAL => "30 is equal to 30\n"
                 | GREATER => "30 is greater than 30\n");
val _ = print (case resultInt3
                of LESS => "40 is less than 25\n"
                 | EQUAL => "40 is equal to 25\n"
                 | GREATER => "40 is greater than 25\n");

(* 打开 Real 库 *)
open Real;

(* 使用 compare 函数比较两个实数 *)
val resultReal1 = compare (3.14, 2.71); (* 应该返回 GREATER *)
val resultReal2 = compare (2.71, 2.71); (* 应该返回 EQUAL *)
val resultReal3 = compare (1.41, 1.62); (* 应该返回 LESS *)

(* 打印比较结果 *)
val _ = print (case resultReal1
                of LESS => "3.14 is less than 2.71\n"
                 | EQUAL => "3.14 is equal to 2.71\n"
                 | GREATER => "3.14 is greater than 2.71\n");

val _ = print (case resultReal2
                of LESS => "2.71 is less than 2.71\n"
                 | EQUAL => "2.71 is equal to 2.71\n"
                 | GREATER => "2.71 is greater than 2.71\n");

val _ = print (case resultReal3
                of LESS => "1.41 is less than 1.62\n"
                 | EQUAL => "1.41 is equal to 1.62\n"
                 | GREATER => "1.41 is greater than 1.62\n");
 
(* 打开 String 库 *)
open String;

(* 使用 compare 函数比较两个字符串 *)
val resultString1 = compare ("hello", "world"); (* 可能返回 LESS 或 GREATER，取决于字典序 *)
val resultString2 = compare ("same", "same");   (* 应该返回 EQUAL *)
val resultString3 = compare ("apple", "banana");(* 应该返回 LESS *)

(* 打印比较结果 *)
val _ = print (case resultString1
                of LESS => "\"hello\" is less than \"world\"\n"
                 | EQUAL => "\"hello\" is equal to \"world\"\n"
                 | GREATER => "\"hello\" is greater than \"world\"\n");

val _ = print (case resultString2
                of LESS => "\"same\" is less than \"same\"\n"
                 | EQUAL => "\"same\" is equal to \"same\"\n"
                 | GREATER => "\"same\" is greater than \"same\"\n");

val _ = print (case resultString3
                of LESS => "\"apple\" is less than \"banana\"\n"
                 | EQUAL => "\"apple\" is equal to \"banana\"\n"
                 | GREATER => "\"apple\" is greater than \"banana\"\n");


(*collate f(s,t) *)
(*只是几个例子没有collate*)
(* 比较字符串 "apple" 和 "banana" *)
val isAppleLessThanBanana = "apple" < "banana"; (* 应该返回 true *)
val isAppleLessThanOrEqualToBanana = "apple" <= "banana"; (* 应该返回 true *)
val isAppleGreaterThanBanana = "apple" > "banana"; (* 应该返回 false *)
val isAppleGreaterThanOrEqualToBanana = "apple" >= "banana"; (* 应该返回 false *)

(* 打印比较结果 *)
val _ = print (Bool.toString isAppleLessThanBanana ^ "\n"); (* 打印 "true" *)
val _ = print (Bool.toString isAppleLessThanOrEqualToBanana ^ "\n"); (* 打印 "true" *)
val _ = print (Bool.toString isAppleGreaterThanBanana ^ "\n"); (* 打印 "false" *)
val _ = print (Bool.toString isAppleGreaterThanOrEqualToBanana ^ "\n"); (* 打印 "false" *)

(* 比较字符串 "apple" 和 "apple" *)
val isAppleEqualToApple = "apple" = "apple"; (* 应该返回 true *)
val isAppleNotEqualToApple = "apple" <> "apple"; (* 应该返回 false *)

(* 打印比较结果 *)
val _ = print (Bool.toString isAppleEqualToApple ^ "\n"); (* 打印 "true" *)
val _ = print (Bool.toString isAppleNotEqualToApple ^ "\n"); (* 打印 "false" *)


*)