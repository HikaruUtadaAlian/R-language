rotate <- function(text, key) {
  chars <- strsplit(text, "")[[1]]
  #核心代码，必背,通过逻辑匹配找到大写字母
  is_upper <- grepl("^[A-Z]$", chars)
  is_lower <- grepl("^[a-z]$", chars)
  if (any(is_upper)){
    #通过刚刚得到的逻辑判断矩阵找到原来的大写字母用match函数匹配它在LETTERS中的数字位置
    nums <- match(chars[is_upper], LETTERS)
    #计算新的索引
    nums_new <- (nums + key -1) %% 26 + 1
    #对于原索引中的字母可以逻辑判断矩阵找到，然后用新索引找到LETTERS中转化的字母，赋值
    chars[is_upper] <- LETTERS[nums_new]
  }
  if (any(is_lower)){
    nums <- match(chars[is_lower], letters)
    nums_new <- (nums + key - 1) %% 26 + 1
    chars[is_lower] <- letters[nums_new]
  }    
  paste(chars,collapse = "")
}
