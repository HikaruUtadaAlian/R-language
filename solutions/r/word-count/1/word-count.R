word_count <- function(input) {
  input <- tolower(input)
#正则表达式：除了空格字母数字和'之外的内容都为空
  clean_input <- gsub("[^[:alnum:][:space:]']", " ", input)
  clean_input <- gsub("[[:space:]]+", " ",trimws(clean_input))
#函数写法 直接一步到位的
   #rule <- gregexpr("[A-Za-z0-9' ]+", input)
   #clean_input <- regmatches(input, rule)
  #根据空格分割字符串
  word <- strsplit(clean_input, " ")[[1]]
  word <- gsub("^'+|'+$", "", word)
  word <- word[word != ""]
  df_word <- as.data.frame(table(word))
  return(setNames(as.list(df_word[, 2]), df_word[, 1]))
}
