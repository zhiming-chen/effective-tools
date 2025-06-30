# R语言算法编程

# fibonacci数列，学习认识递归及替代方法

# 思路方法描述

#  用递归的方法实现



fibonacci_1  <- function(n){
    if (!is.numeric(n) || n %% 1 != 0 || n < 0) {
        stop("请输入非负整数")}
    
    if(n == 0) return(0)
    if(n == 1) return(1) 
    fibonacci_1(n-1) + fibonacci_1(n-2)
   
}
# 示例
fibonacci_1(n = 4)


#上述方法看上去简洁直观，但重复计算太多，效率极低。

# 我们使用迭代法或者存储法来实现，重点使用for 循环。

fibonacci_2 <- function(n){
    if (!is.numeric(n) || n %% 1 != 0 || n < 0) {
        stop("请输入非负整数")}
    
    if(n == 0) return(0)
    if(n == 1) return(1) 
    
    a <- 0
    b <- 1
    
    
    for(i in 2:n){
        n_fib <- a + b
        a <- b
        b <- n_fib
    }
    
  return(b)    
}

fibonacci_2(6)


## 这个思路可以用一个比较直观的数列缓存方法来“可视化”。
## 我们把每次计算结果存成一个向量V，那F(n)就是向量V的第n+1个值，也就是V[n+1]

## 代码实现如下

fibonacci_3 <- function(n){
    if (!is.numeric(n) || n %% 1 != 0 || n < 0) {
        stop("请输入非负整数")}
    
    if(n == 0) return(0)
    if(n == 1) return(1) 
 
    #创建一个长度为n+1的向量
 fi_v <- numeric(n+1)
 fi_v[1] <- 0  # 这里存放F(0)
 fi_v[2] <- 1  # 这里存放F(1)
 
 for (i in 3:(n+1)){# 这里n+1 需要括起来...
     fi_v[i] <- fi_v[i-1] + fi_v[i-2]
 }
 return(list(fi_v[n+1], fi_v))
 
}
# 和很多其他编程语言不一样，R语言向量第一位用数字1表示，不是其他语言多用的0    
    
fibonacci_3(6)

