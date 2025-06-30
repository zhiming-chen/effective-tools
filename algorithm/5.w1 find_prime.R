# R语言算法编程

# 找寻质数并存放到向量中

source("./algorithm/2.w1 is_prime.R")
 
# for 循环 + 向量存储

get_prime_for <- function(n){
    # 目标位找到1到n之间的全部质数
    
    if (!(is.numeric(n) && n > 1 && n %% 1 == 0)) {
        stop("请输入大于1的整数")
    }
    
    primes <- c()
    
    for(i in 2:n){# 因为1不是质数，所以不纳入
        
        if(is_prime_for(i)){ # 调用的函数，他的输出是TRUE|FALSE 的逻辑值
            
            primes <- c(primes,i)
        }
     
    }
    
    return(primes)   # 不能写在for循环里，否则他会直接在第一步就结束for循环
}


# get_prime_for(19)


# 也可以使用apply系列函数来实现遍历

# 输入向量+apply系列函数循环 + 向量输出

# 这看上去会非常简单

get_prime_apply <- function(n){
    
    
    
    # 目标位找到1到n之间的全部质数
    
    if (!(is.numeric(n) && n > 1 && n %% 1 == 0)) {
        stop("请输入大于1的整数")
    }
    
    vec <- 2:n
    
    vec[sapply(vec,is_prime_for)]
     
    }
    
get_prime_apply(19)


## map 循环

get_prime_map <- function(n){
    
    
    
    # 目标位找到1到n之间的全部质数
    
    if (!(is.numeric(n) && n > 1 && n %% 1 == 0)) {
        stop("请输入大于1的整数")
    }
    
    vec <- 2:n
    
    vec[purrr::map_lgl(vec,is_prime_for)]
    
}

get_prime_map(19)

 
