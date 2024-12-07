(*record 记录*)
val x={bar =(3,true),foo=3+4,baz=(false,9)};
val my_niece={id =41111,name="Amelia"};
val brain_part= {id =true,ego=false,superego=false};
(*tuples-as-syntactic-sugar 语法糖*)
val a_pairs=(3+1,4+2);
val  a_record={second=6,first=5};
val another_pair={2=5,1=6};
val x={1=true,3="hi"};
val y={3="hi",1=true,2=3+2};
(*datatype_binding 数据类型绑定*)
datatype mytype =TwoInts of int* int
                 |Str of string
                 |Pizza;

val a= Str "hi"
val b= Str 
val c= Pizza
val d=TwoInts(1+2,3+4)
val e=a


(*case-expressions案例表达*)
datatype mytype=TwoInts of int*int
 |Str of string
 |Pizza
(*mytype -> int*)
fun f(x :mytype)=
 case x of
     Pizza=>3
     |Str s=>8
     |TwoInts(i1,i2)=>i1+i2

(*useful-datatypes 实用数据类型*)
datatype suit =Club |Diamod|Heart |Spade
datatype rank =Jack|Queen|King|Ace|Num of int
datatype id =StudentNum of int 
         |Name of string * (string option)*string
datatype exp =Constant of int
              |Negate of exp
              |Add of exp*exp
              |Multiply of exp*exp

fun eval e=
    case e of
    Constant i=>i
    |Negate e2 => ~(eval e2)
    |Add (e1,e2)=>(eval e1)+(eval e2)
    |Multiply(e1,e2)=>(eval e1)*(eval e2)

fun number_of_adds e=
    case e of 
    Constant i=>0
    |Negate e2=> number_of_adds e2
    |Add(e1,e2)=>1+number_of_adds e1+number_of_adds e2
    |Multiply(e1,e2)=>number_of_adds e1+number_of_adds e2


val example_exp =Add(Constant 19,Negate(Constant 4))
val example_ans = eval example_exp
val example_addcount=number_of_adds (Multiply(example_exp,example_exp))



(*another-expression-example 另一个表达实例 no408.sml*)
datatype exp =Constant of int
              |Negate of exp
              |Add of exp*exp
              |Multiply of exp*exp
            
fun max_constant e=
   let fun max_of_two (e1,e2)=
        let val m1 =max_constant e1   
            val m2 =max_constant e2
        in
           if m1>m2 then m1 else m2
        
        end
  in 
    case e of 
    Constant i => i
    |Negate e2 => max_constant e2
    |Add(e1,e2)=> max_of_two (e1,e2)
    |Multiply(e1,e2)=>max_of_two(e1,e2)
  end  

val test_exp =Add(Constant 19,Negate(Constant 4))
val nineteen = max_constant test_exp

fun max constant2 e=
   case e of 
   Constant i => i
   |Negate e2 => max constant2 e2
   |Add(e1,e2)=> Int.max(max constant2 e1,max constant2 e2)
   |Multiply(e1,e2)=>Int.max(max constant2 e1,max constant2 e2)



(*类型同义词*)
datatype suit =Club |Diamod|Heart |Spade
datatype rank =Jack|Queen|King|Ace|Num of int
type card =suit*rank
type name_record ={student_num:int option,
                   first      :string,
                   middle     :string option,
                   last       :string}
                   
 fun is_Queen_of_Spades(c:card)=
     #1 c=Spade andalso #2 c = Queen
     val c1 :card =(Diamod,Ace)
     val c2 :suit *rank =(Heart,Ace)
     val c3  =(Spade,Ace)
















   


     





