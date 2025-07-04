# R语言算法编程

# 零钱凑整问题

 

# for 循环 + 向量存储



# 使用动态规划方法，自底向上。

# coins 为硬币面值的向量，比如1,2,5...

# amount 为目标总金额，比如11...
# 
# dp[i] 为总金额为i-1所需的最小硬币数

# 先预设dp[1] = 0

# dp[i] = min(dp[i],dp[i-coin] + 1)

# 先顶一个数值向量，来装所需最小硬币数,初始定义每个值均为Inf，意味着每个总额都无解

coins_change <- function(coins,amount){
    
    dp <- rep(Inf, amount+1)
    
    dp[1] <- 0
    
    for (i in 2:(amount + 1)) {
        for(coin in coins){
                  if(coin <=(i - 1)){
                    dp[i] <-  min(dp[i],dp[i-coin] + 1)
                }
            }
            
        }
    if (is.infinite(dp[amount + 1])){
        return(-1)
    } else{
        return(dp[amount + 1])
  }
    
}

# 或者这样，逻辑是一致的


coins_change2 <- function(coins, amount) {
    # 创建dp数组，dp[i]表示组成金额i所需的最少硬币数
    dp <- rep(Inf, amount + 1)
    # 金额为0时，需要0个硬币
    dp[1] <- 0  # 对应金额0
    
    # 填充dp数组
    for (i in 1:amount) {
        for (coin in coins) {
            if (coin <= i) {
                # 状态转移方程
                dp[i + 1] <- min(dp[i + 1], dp[i - coin + 1] + 1)
            }
        }
    }
    
    # 判断是否能组成目标金额
    if (dp[amount + 1] == Inf) {
        return(-1)
    } else {
        return(dp[amount + 1])
    }
}

## 测试结果
coins_change(coins = c(1,2,5),amount = 11)

coins_change2(coins = c(5,8),amount = 7)
