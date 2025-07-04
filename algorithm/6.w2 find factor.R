# R语言算法编程

# 找出一个数的全部因数

 

# for 循环 + 向量存储

get_factor <- function(n){
    # 目标位找到1到n之间的全部质数
    
    if (!(is.numeric(n) && n > 1 && n %% 1 == 0)) {
        stop("请输入大于1的整数")
    }
    
    factor_vec <- c(1,n)
    
    k <- floor(sqrt(n))
    
    for(i in 2:k){# 因为1和数字本身肯定是数字的因数
        
        if(n %% i == 0){ # 调用的函数，他的输出是TRUE|FALSE 的逻辑值
            q <- n/i
            factor_vec <- c(factor_vec,i,q)
        }
        
    }
    factor_vec <- unique(factor_vec) ## 去重
    
    factor_vec <- sort(factor_vec) ## 排序
    
    return(factor_vec)  
}


# get_factor(100)
