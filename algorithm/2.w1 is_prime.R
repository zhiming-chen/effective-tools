# R语言算法编程

# 判断一个数是否为质数

# 思路方法描述

#  1. 需要是自然数
#  2. 1不是质数
#  3. 2,3 是质数
#  4. 偶数不是质数
#  5.大于3的奇数中，对该数进行开方得到一个结果（取大整数和小整数均可，代码描述会有一点区别），
#    该数逐个除以3与这一结果之间的奇数，如存在能整除的情况，则不是奇数，否则就是奇数

#  举例：数字7，开方后结果大于2小于3,3与该结果之间的奇数仅为3,7 不能整除3，所以是奇数
#        数字23，开方后结果大于4小于5,3与该结果之间的奇数为3,5, 23不能整除3以及5，是奇数...
#  进行开方后再逐一判断，能够减少计算量。

 


# 用R 写一个小函数 if +for 循环

is_prime_for <- function(x){
    # 先判断自然数
    if(!(is.numeric(x)&x%%1==0&x>1)){
        stop("请输入正整数")
    }
   # 2为质数 
    if(x == 2|x == 3) return( TRUE ) 
    
   # 2 之外的偶数不是质数
    if(x %% 2 ==0) return( FALSE )
    
   # 大于3开始的奇数，使用开方整除的方法判断
    for( i in seq(3, ceiling(sqrt(x)),2)){ # 用floor函数取开方值小值
        
        if(x %% i == 0 )return( FALSE )
    }
    return( TRUE)
}

is_prime_for(111)

## 这里去大整数，会存在多计算一次的情形，比如数字79，他需要对3,5,7,9四个数字进行除法检查
## 实际上只要试到7 就够了。

# 优化一下代码
is_prime_for_floor <- function(x){
    # 先判断自然数
    if(!(is.numeric(x) & x%%1 == 0 & x > 1)){
        stop("请输入正整数")
    }
    # 2,3为质数 
    if(x == 2|x == 3) return( TRUE ) 
    
    # 2 之外的偶数不是质数
    if(x %% 2 ==0) return( FALSE )
   
    # 大于3开始的奇数，使用开方整除的方法判断
    
    max_divisor <- floor(sqrt(x))
    
    if(max_divisor >= 3){
        for( i in seq(3, max_divisor,2)){
            if(x %% i == 0 )return( FALSE )
        } 
        return(TRUE)
    } else 
        return(TRUE)
}
is_prime_for_floor(101)


## 用while函数来实现



is_prime_while <- function(x){
    # 先判断自然数
    if(!(is.numeric(x) & x%%1==0 & x>1)){
        stop("请输入正整数")
    }
    
    # 处理2和3的情况
    if(x <= 3) return(TRUE) 
    
    # 2 之外的偶数不是质数
    if(x %% 2 == 0) return(FALSE)
    
    # 大于3的奇数，使用开方整除的方法判断
    
    
    # 设置初始值为3，确保循环至少执行一次（当max_divisor >=3时）
    i <- 3
    while(i <= sqrt(x)){
        if(x %% i == 0) return(FALSE)
        i <- i + 2  # 只检查奇数
    }
    
    # 如果都不能整除，则为质数
    return(TRUE)
}

# 这个看起来更舒适，比如数字97，他对3进行整除，发现不能整除，不进行输出，
# 继续对5进行整除...直到对9进行整除，都不能整除，他结束循环；
# 对于数字65，他对3 不能整除，继续对5整除，发现能整除，输出FALSE，打完收工。
# 也不需要对开方值进行取整判断操作一堆做法。因为当开方值小于3时，直接判断为TRUE,不参与整除的判断。

is_prime_while(11)
