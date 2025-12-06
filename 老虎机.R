play_many <- function(n) {
  #确定一个游玩老虎机次数的函数
  get_many_symbols <- function(n = 3) {
    #转盘抽到的结果的种类
    wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
    #随机生成symbols的表格，3列n行
    symbols <- sample(wheel, size = 3 * n, replace = TRUE, prob = c(0.35, 0.25, 0.25, 0.1, 0.02, 0.02, 0.01))
    #生成矩阵,函数会默认返回它的最后一行，自动展示结果，同时局部变量会被回收
    symbols_matrix <- matrix(symbols, ncol = 3)
  }
    #调用get_symany_symbols
  symbols_matrix <- get_many_symbols()
  #接下来调用一个计算得分的函数
  score_many_symbols <- function(symbols_matrix) {
    diamonds <- rowSums(symbols_matrix == "DD")
    #局部变量回收，不展示
    diamonds
    cherries <- rowSums(symbols_matrix == "C")
    #钻石百搭，也就是说钻石加樱桃可以实现大部分2、5的组合
    prize <- c(0, 2, 5)[cherries + diamonds + 1]
    #没有真实的樱桃，那么就0,防止三个钻石的情况产生bug
    prize[!cherries] <- 0
    #情况一：三个符号完全一样
    same <- symbols_matrix[, 1] == symbols_matrix[, 2] & symbols_matrix[, 2] == symbols_matrix[, 3]
    #制作奖励表，后续same可以用于提取奖励调查表的内容
    payoffs <- c("DD" = 100, "7" = 15, "BBB" = 30, "BB" = 20, "B" = 10, "C" = 5, "0" = 0)
    prize[same]  <- payoffs[symbols_matrix[same,1]]
    #开始处理全是bar类的符号，先构建一个bar的逻辑判断矩阵
    is_bar_element <- symbols_matrix %in% c("BBB", "BB", "B")
    bars <- matrix(is_bar_element, ncol = 3, byrow = FALSE)
    #根据bars的逻辑判断矩阵，&符号判断多个条件是否同时成立
    all_bars <- bars[, 1] & bars[, 2] & bars[, 3] & !same
    #根据all_bars分配奖励
    prize[all_bars] <- 5
    #处理百搭钻石的数量
    #含有两个钻石,但是先得到钻石判断矩阵
    two_diamonds <- matrix(symbols_matrix == "DD", nrow = nrow(symbols_matrix), byrow = FALSE)
    #标记非钻石在哪个位置，要注意得到的是逻辑判断矩阵，如果有两个FALSE也会被误当成dd
    one <- two_diamonds[, 2] == two_diamonds[, 3] & two_diamonds[, 1] != two_diamonds[, 2] & diamonds == 2
    two <- two_diamonds[, 1] == two_diamonds[, 3] & two_diamonds[, 2] != two_diamonds[, 3] & diamonds == 2
    three <- two_diamonds[, 1] == two_diamonds[, 2] & two_diamonds[, 1] != two_diamonds[, 3] & diamonds == 2
    #根据非钻石奖励符号设置奖励
    prize[one] <- payoffs[symbols_matrix[one, 1]]
    prize[two] <- payoffs[symbols_matrix[two, 2]]
    prize[three] <- payoffs[symbols_matrix[three, 3]]
    #处理含有一个钻石的局
    #两个bars和1个钻石
    bar_count <- rowSums(bars)
    bdd <- (bar_count == 2) & (diamonds == 1)
    prize[bdd] <- 5
    one_1 <- two_diamonds[, 1] == "DD" & symbols_matrix[, 2] == symbols_matrix[, 3] & symbols_matrix[, 1] != symbols_matrix[, 2]
    two_1 <- two_diamonds[, 2] == "DD" & symbols_matrix[, 1] == symbols_matrix[, 3] & symbols_matrix[, 1] != symbols_matrix[, 2]
    three_1 <- two_diamonds[, 3] == "DD" & symbols_matrix[, 1] == symbols_matrix[, 2] & symbols_matrix[, 1] != symbols_matrix[, 3]
    prize[one_1] <- payoffs[symbols_matrix[one_1, 2]]
    prize[two_1] <- payoffs[symbols_matrix[two_1, 3]]
    prize[three_1] <- payoffs[symbols_matrix[three_1, 1]]
    #最终奖励——有钻石那么奖励翻倍
    prize <- prize * 2^diamonds
  }
}

  