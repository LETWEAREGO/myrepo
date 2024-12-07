datatype exp = Constant of int 
             | Negate of exp 
             | Add of exp * exp
             | Multiply of exp * exp

(* Note: example as explained in video assumes there is no library function
   for max of two ints.  There is: Int.max *)

fun max_constant e =
	case e of
	    Constant i      => i
	  | Negate e2       => max_constant e2
	  | Add(e1,e2)      => let(*比大小*)
                            val m1=max_constant e1
                            val m2=max_constant e2
	                          in if m1 >m2 then m1 else m2 end
	  | Multiply(e1,e2) => let
                            val m1=max_constant e1
                            val m2=max_constant e2
	                     in if m1>m2 then m1 else m2 end
val test_exp = Add (Constant 19, Negate (Constant 4))
val nineteen = max_constant test_exp
