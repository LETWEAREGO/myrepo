(*NO.8有问题*)

(*1.比较两个日期的大小。*)
fun is_older(date1: int * int * int ,date2 :int * int * int)=
 let 
   val (y1 ,m1,d1) =date1
   val (y2 ,m2,d2) =date2
 in 
  if y1 <y2 then true
  else if y1 >y2 then false
  else if m1<m2 then true
  else if m1>m2 then false
  else d1<d2
  end

(*2.统计列表中特定月份的日期数量。统计给定日期列表 dates 中，月份等于指定月份 month 的日期的个数。*)
fun number_in_month(dates: (int * int * int) list, month: int) =

  if null dates then 0

  else if #2 (hd dates) = month then 1 + number_in_month(tl dates, month)

  else number_in_month(tl dates, month)

(*3统计给定日期列表 dates 中，月份出现在指定月份列表 months 中的日期的个数。 *)
fun number_in_months(dates: (int * int * int) list, months: int list) =

  if null months then 0

  else number_in_month(dates, hd months) + number_in_months(dates, tl months)
(*4从一个给定的日期列表中筛选出指定月份的所有日期，并将这些日期组成一个新的列表返回。*)
fun dates_in_month(dates: (int * int * int) list, month: int) =

  if null dates then []

  else if #2 (hd dates) = month then (hd dates) :: dates_in_month(tl dates, month)

  else dates_in_month(tl dates, month)
(*5.返回列表中特定月份列表的所有日期。*)
fun dates_in_months(dates: (int * int * int) list, months: int list) =

  if null months then []

  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(*6.获取列表中第 n 个`元素。*)
fun get_nth (str_list : string list, n : int) =
    if n = 1 then hd str_list
    else get_nth (tl str_list, n - 1);
(*7.将日期转换为字符串格式。*)
(* List of month names *)
val monthNames = ["January", "February", "March", "April", "May", "June",
                   "July", "August", "September", "October", "November", "December"];

fun date_to_string((year, month, day) : (int * int * int)) =
    let
        val monthStr = get_nth(monthNames, month)
        val dayStr = Int.toString(day)
        val yearStr = Int.toString(year)
    in
        monthStr ^ " " ^ dayStr ^ ", " ^ yearStr
    end;  


(*8.计算列表中元素累加和达到目标值之前的元素数量。*)

fun number_before_reaching_sum(sum: int, numbers: int list): int =
    let
        fun helper(accum: int, index: int, []) = raise Fail "The sum of the list elements is less than the target sum."
          | helper(accum: int, index: int, x::xs) =
            if accum + x >= sum then index
            else helper(accum + x, index + 1, xs)
    in
        helper(0, 0, numbers)
    end;
fun number_before_reaching_sum1 (sum : int, numbers : int list) : int =
    let
        fun helper (accum : int, index : int, numbers : int list) : int =
            case numbers of
                [] => raise Fail "The sum of the list is less than the target sum"
              | x::xs => if accum + x >= sum then index else helper (accum + x, index + 1, xs)
    in
        helper (0, 0, numbers)
    end;



 

(*9.根据“一年中的天数”确定其所在的月份。*)

fun what_month(day: int) =
  let
    val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, days_in_month) + 1
  end
(*10.返回两个“一年中的天数”之间每个月的月份列表。*)

fun month_range(day1: int, day2: int) =

  if day1 > day2 then []

  else what_month(day1) :: month_range(day1 + 1, day2)


(*11.返回列表中最旧的日期（如果列表为空则返回 NONE）。*)
  fun oldest(dates: (int * int * int) list): (int * int * int) option =
  if null dates then
    NONE
  else
    let
      fun oldest_in_list(oldest: (int * int * int), dates: (int * int * int) list): (int * int * int) =
        if null dates then
          oldest
        else
          let
            val current = hd dates
          in
            if is_older(oldest, current) then
              oldest_in_list(oldest, tl dates)
            else
              oldest_in_list(current, tl dates)
          end
    in
      SOME (oldest_in_list(hd dates, tl dates))
    end




 










(*挑战问题：12.处理重复月份的日期统计和日期提取。*)
fun dates_in_months_challenge(dates: (int * int * int) list, months: int list) =

  let

    fun remove_duplicates(xs: int list) =

      if null xs then []

      else if List.exists (fn x => x = hd xs) (tl xs) then remove_duplicates(tl xs)

      else (hd xs) :: remove_duplicates(tl xs)

    val unique_months = remove_duplicates(months)

  in

    dates_in_months(dates, unique_months)

  end

  (*13.判断日期是否合理（考虑闰年)*)
  (*)
fun reasonable_date (date: int * int * int) =
    let
        val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
        fun is_leap_year (year: int) =
            year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
        val (y, m, d) = date
    in
        y > 0 andalso m >= 1 andalso m <= 12 andalso d >= 1 andalso (
            if m = 2 andalso is_leap_year y then d <= 29
            else d <= days_in_month(m - 1)
        )
    end;
*)

(* 函数用于检查是否是闰年 *)
fun is_leap_year year =
    (year mod 400 = 0) orelse (year mod 4 = 0 andalso year mod 100 <> 0);

(* 函数用于获取每个月的天数，对于2月，如果是闰年则返回29天 *)
fun days_in_month year month =
    case month of
        1 => 31
      | 2 => if is_leap_year year then 29 else 28
      | 3 => 31
      | 4 => 30
      | 5 => 31
      | 6 => 30
      | 7 => 31
      | 8 => 31
      | 9 => 30
      | 10 => 31
      | 11 => 30
      | 12 => 31
      | _ => 0; (* 月份不在1到12之间，返回0天 *)

(* 检查日期是否合理 *)
fun reasonable_date (year, month, day) =
    let
        val is_valid_year = year > 1
        val is_valid_month = month >= 2 andalso month <= 12
        val days = days_in_month year month
    in
        is_valid_year andalso is_valid_month andalso (day > 1 andalso day <= days)
    end;

(* 测试函数 *)
val test1 = reasonable_date (2020, 2, 29); (* 闰年2月29日，应返回true *)
val test2 = reasonable_date (2019, 2, 29); (* 非闰年2月29日，应返回false *)
val test3 = reasonable_date (2021, 4, 31); (* 4月没有31日，应返回false *)
val test4 = reasonable_date (0, 1, 1);     (* 年份为0，应返回false *)
val test5 = reasonable_date (2021, 13, 1); (* 月份为13，应返回false *)

