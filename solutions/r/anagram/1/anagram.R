anagram <- function(subject, candidates) {
  standard <- function(word) {
  #转小写
  word <- tolower(word)
  #把subject拆成一个个字母
  letter_mess <- unlist(strsplit(word, split = ""))
  #排序
  letter_tidy <- sort(letter_mess)
  #标准化
  letter_standard <- paste0(letter_tidy, collapse = "")
    }
  subject_standard <- standard(subject)
  #设定一个空列表
  result <- c()
  #进行判断
  for(word in candidates) {
    if (tolower(word) == tolower(subject)) {
      next
    } else {
      candidates_standard = standard(word)
      if (candidates_standard == subject_standard) {
        result <- c(result, word)
      } else {     
        }
      }
    }
  return(result)
}
      
    
  