input =  19
case input
# 检查一个整数，19
when 19
  puts "It's 19"
  # 检查浮点数，33.3
when 33.3
  puts "It's 33.3"
  # 检查一个确切的字符串，“Zaman”
when "Zaman"
  puts "Hi Zaman"
when 10
  puts "It's 10"
  # 检查范围
when 7..11
  puts "It's between 7 and 11"
  # 检查多个值，“咖啡”
when "tea", "coffee"
  puts "Happy days"
  # 检查正则表达式“aA6”
when /^a[A-Z]+[0-6]+$/
  puts "It's a valid match"
  # 通过与 String 类“任何字符串”
  # 进行比较来检查任何字符串
when String
  puts "It's a String"
end