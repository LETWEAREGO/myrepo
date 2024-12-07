fun find_element (element:int, int_list:int list) =
    case int_list of
        [] => NONE
      | x::xs => if x = element then SOME x else find_element(element, xs)




