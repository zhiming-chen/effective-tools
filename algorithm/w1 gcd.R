# R语言算法编程

#  求两个数的最大公约数 greatest common divisor

# 思路方法描述

## 1.穷举法

## 从两个数中的最小数开始从大到小对两数进行整除，如果同时余数为0，则该数为最大公约数
## 这个方法很笨重。但适合练习for循环。

## 2.欧几里得法

## 这个方法其实是一个递归的思维，可以使用while语句来实现。其方法为：
## 两个数大数除以小数，得到余数，然后其中的小数继续除以余数，直到余数为0，前面的被除数即为最大公约数

## 总体来讲，这是比较简单的入门函数。
 
gcd_for <- function(a,b){
    # 先验证是否为整数
    
    if(!(is.numeric(a) & is.numeric(a) & a%%1==0 & b%%1==0)){
        stop("请输入两个整数")
    }
    a <- abs(a)
    b <- abs(b)
    
    ab_min <- min(a,b)
    gcd <- 1 # 这种写法需要先赋值，作为兜底策略。

    for(i in seq(ab_min,1)) {
        if(a%%i==0 & b%%i==0){
            gcd <- i
            break
        }
           
    }  
    return(gcd)
    
}


## 示例

gcd_for(48,32)
gcd_for(37,25)


# 上述方法相对冗长，可以有相对更简洁的for循环写法。

gcd_for_2 <- function(a,b){
    # 先验证是否为整数
    
    if(!(is.numeric(a) & is.numeric(a) & a%%1==0 & b%%1==0)){
        stop("请输入两个整数")
    }
    a <- abs(a)
    b <- abs(b)
    
    for(i in min(a,b):1) {
        if(a%%i==0 & b%%i==0){
            return(i)
             
        }
    } 
}

## 比较两者的区别

## return()：终止整个函数的执行；

## break：终止当前循环，函数本身还继续执行。

#  示例
gcd_for_2(111,37)

gcd_for_2(111,36)

## 使用while语句

gcd_while <- function(a,b){
    # 先验证是否为整数
    
    if(!(is.numeric(a) & is.numeric(a) & a%%1==0 & b%%1==0)){
        stop("请输入两个整数")
    }
    a <- abs(a)
    b <- abs(b)
    
    while(b != 0){
        temp <- b
        b <- a%%b
        a <- temp
    }
   return(a) 
}

## 举例

gcd_while(128,48)

## 尝试用递归的方法来写这个函数

gcd_recursive <-  function(a,b){
    # 先验证是否为整数
    
    if(!(is.numeric(a) & is.numeric(a) & a%%1==0 & b%%1==0)){
        stop("请输入两个整数")
    }
    a <- abs(a)
    b <- abs(b)
    
    if(b == 0){
        return(a)
    
    } else {
        return(gcd_recursive(b, a%%b))
    }
}

## 示例
gcd_recursive(111,37)

gcd_recursive(48,18)
