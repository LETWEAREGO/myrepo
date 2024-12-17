(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

	      (* put your solutions for problem 2 here *)


(*名字替换函数*)
(*
  all_except_option函数：接受一个字符串和字符串列表，
  若字符串在列表中则返回不含该字符串的新列表的SOME，
  否则返回NONE。需使用same_string比较字符串，
  假设字符串最多在列表中出现一次，示例代码约8行。
  *)
  
fun all_except_option2 (str, str_list) =
    let
        fun remove_str (str_list) =
            case str_list of
                [] => []
              | x::xs => if x = str then xs else x :: remove_str(xs)
    in
        if List.exists (fn x => x = str) str_list
        then SOME (remove_str str_list)
        else NONE
    end



fun all_except_option(str, str_list) =
    case str_list of
        [] => NONE
      | x::xs => if same_string(x, str) then SOME xs else
                    case all_except_option(str, xs) of
                        NONE => NONE
                      | SOME new_list => SOME (x::new_list)       


(*
  get_substitutions1函数：接受一个字符串列表列表和字符串，
  返回该字符串在列表列表中出现的所有字符串列表，
  若字符串不在列表列表中则返回空列表，示例代码约10行。
  *)
fun get_substitutions12 (str_list_list, str) =
    case str_list_list of
        [] => []
      | x::xs => case all_except_option(str, x) of
                    NONE => get_substitutions1(xs, str)
                  | SOME new_list => new_list @ get_substitutions1(xs, str)


fun get_substitutions1(li:(string list)list,s) =
    case li of
	[] => []
      | y::ys' =>let val tmp = all_except_option(s,y)
		 in
		     if isSome tmp
		     then (valOf tmp)@get_substitutions1(ys',s)
		     else []@get_substitutions1(ys',s)
		 end 
		     
(*
  get_substitutions2函数：接受一个字符串列表列表和字符串，
  返回该字符串在列表列表中出现的所有字符串列表，
  若字符串不在列表列表中则返回空列表，示例代码约10行。
  *)
fun get_substitutions22 (str_list_list, str) =
    let
        fun get_substitutions (str_list_list, str, acc) =
            case str_list_list of
                [] => acc
              | x::xs => case all_except_option(str, x) of
                            NONE => get_substitutions(xs, str, acc)
                          | SOME new_list => get_substitutions(xs, str, acc @ new_list)
    in
        get_substitutions(str_list_list, str, [])
    end

fun get_substitutions2(lli:(string list)list,ss:string) =
    let fun helper(li,s)=
	case li of
		      [] => (tl s)
		    | y::ys' => let val tmp = all_except_option(hd s,y)
				in
				    if isSome tmp
				    then helper(ys',s@(valOf tmp))
				    else helper(ys',s)
				end
				    
     in
	 helper(lli,[ss])
     end


(*
  similar_names函数：接受一个字符串列表列表、{first=first_name, middle=middle_name, last=last_name}，
  返回该名字和其所有替换名的列表，示例代码约10行。
  *)


fun similar_names(li,s:{first:string, last:string, middle:string})=
    let fun fix(xs,s:{first:string, last:string, middle:string}) =
	    case xs of
		[] => []
	      | x::xs' => [case s of {first=_,last=b,middle=c}=>{first=x,last=b,middle=c}]@fix(xs',s)
    in
	[s]@fix(get_substitutions1(li,(case s of {first=a,last=_,middle=_}=>a)),s)
    end
	

(*单人纸牌游戏相关函数（7个）*)
(*
  card_color函数：接受一张牌，返回其颜色（黑：黑桃和梅花；红：方块和红桃），用一个case-expression实现。
*)
fun card_color(cd:card) =
    case cd of
	      (Spades,_) => Black
      | (Clubs,_) => Black
      | (Diamonds,_) => Red
      | (Hearts,_) => Red


    
(*
  card_value函数：接受一张牌，返回其点数（A：1，2-10：其点数，J：11，Q：12，K：13），用一个case-expression实现。
*)        
fun card_value(cd:card):int =
    case cd of
	(_,Num(a)) => a
     | (_,Ace) => 11
     | (_,_) => 10


(*
  remove_card函数：接受一张牌和一张牌列表，返回移除该牌后的新列表，
  若该牌不在列表中则返回原列表，示例代码约10行。
  *)  
fun remove_card(xs:card list,c,e)=
    let fun check(xs:card list,c)=
	    case xs of
		[] => false
	      | y::ys' => if y=c then true
			  else check(ys',c);
	fun getValue(xs:card list,c)=
	    case xs of
		[] => []
	      | y::ys' => if y=c then ys'
  
			  else
			      y::getValue(ys',c)
    in
	let val flag = check(xs,c)
	in
	    if false=flag then
		raise e
	    else getValue(xs,c)
	end
    end
(*
  all_same_color函数：接受一张牌列表，
  若该列表中的所有牌颜色相同则返回true，否则返回false，示例代码约10行。
  *)
fun all_same_color(xs:card list)=
    case xs of
	[] => true
      | y::ys' => case ys' of
		      [] => true
		    | k::ks' => if  card_color(y)=card_color(k)
			       then
				   all_same_color(ys')
			       else
				   false;


(*
  sum_cards函数：接受一张牌列表，返回该列表中所有牌点数之和，示例代码约10行。
  *)
fun sum_cards(li:card list) =
    let fun helper(li:card list,sum:int)=
	    case li of
		[] => sum
	      | y::ys' => helper(ys',sum+(card_value(y)))
    in
	helper(li,0)
    end;

(*
  score函数：接受一张牌列表和一个int，
  若该列表中所有牌之和小于等于该int则返回该int减去该列表中所有牌之和，
  否则若该列表中所有牌颜色相同则返回3 * (该列表中所有牌之和)，
  否则返回该列表中所有牌之和，示例代码约10行。
  *)
fun score(li:card list,goal:int ) =
    let val sum = sum_cards(li);val jud = all_same_color(li)
    in
	let val pre_score =(if sum>goal then 3*(sum-goal) else (goal-sum))
	in
	    case jud of
		false => pre_score
	      | true =>  pre_score div 2
	end
    end
; 

(**
  officiate函数：接受一个卡牌列表、一个移动列表和一个int，
  模拟游戏过程，返回最终分数和剩余卡牌列表，
  若移动列表为空则返回当前分数和卡牌列表，
  若移动为Draw且没有剩余卡牌则返回当前分数和空列表，
  若移动为Draw且有剩余卡牌则返回当前分数和剩余卡牌列表，*)
  
fun officiate(li:card list,nx:move list,goal : int) =
    let
	fun helper(tmpList:card list,append:card) = tmpList@[append];
	fun runGame(lli:card list,nnx:move list,myList:card list) =
	    if sum_cards(myList)>goal then score(myList,goal)
	    else
	      case nnx of
		 [] =>  score(myList,goal)(*no operation*)
	       | y::ys' => case y of
			       Draw =>  (case lli of
					  [] => score(myList,goal) (*no card*)
					| z :: zs' => runGame(zs',ys',myList@[(z:card)]) (*continue*))
	                     | Discard(a,b) =>  runGame(lli,ys',remove_card(myList,(a,b),IllegalMove))
      in
	  runGame(li,nx,[])
      end
