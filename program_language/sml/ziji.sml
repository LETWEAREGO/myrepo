(*
从官网https://smlfamily.github.io/Basis/char.html#SIG:CHAR.chr:VAL:SPEC开始看
*)
(*string*)
 
(*implode 将字符列表元素转换为一个字符串。*) 
val charList = [#"a", #"b", #"c"];
val str = implode charList;
