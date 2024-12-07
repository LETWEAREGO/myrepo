(*名字替换函数*)
(*)
  all_except_option函数：接受一个字符串和字符串列表，
  若字符串在列表中则返回不含该字符串的新列表的SOME，
  否则返回NONE。需使用same_string比较字符串，
  假设字符串最多在列表中出现一次，示例代码约8行。
  

  
  
  
  *)

fun all_except_option(str:string ,str_list:string list)=
 let 



