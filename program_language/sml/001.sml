
(* 定义一个函数 remove_string，它接受一个字符串和一个字符串列表作为参数 *)
fun remove_string (str: string, str_list: string list) : string list option =
    let
        (* 定义一个辅助函数，用于递归地构建不包含特定字符串的新列表 *)
        fun removeAux (str, []) = []
          | removeAux (str, x::xs) =
            if x = str then removeAux(str, xs)
            else x :: removeAux(str, xs)
    in
        (* 检查原始列表是否包含字符串 *)
        if List.exists (fn x => x = str) str_list
        then SOME (removeAux(str, str_list))
        else NONE
    end


(* 使用 remove_string 函数 *)
val result1 = remove_string("apple", ["banana", "apple", "cherry"]); (* 应该返回 SOME ["banana", "cherry"] *)
val result2 = remove_string("orange", ["banana", "apple", "cherry"]); (* 应该返回 NONE *)

(*)
(* 定义比较字符串是否相等的函数same_string *)
fun same_string(s1, s2) = s1 = s2;

(* 定义主函数remove_string *)
fun remove_string(str, str_list) =
    case str_list of
         [] => NONE
       | x::xs => if same_string(str, x) then SOME(xs) else
                   (case remove_string(str, xs) of
                        NONE => NONE
                      | SOME(new_list) => SOME(x :: new_list))


*)


   