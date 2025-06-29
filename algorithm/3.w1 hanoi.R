# R语言算法编程

# 汉诺塔问题，学习认识递归

# 思路方法描述

#  天赋使然，对递归的掌握理解真的是白痴级别。但需要迈过这个关。

#  3根柱子，其中一根放着64(也可以是别的数字)个大小不一的盘子，下面的盘子比上面的大
#  现在需要将这些盘子挪到另外一根柱子，
#  每次只能动一个盘子，
#  大盘子不能放在小盘子上

#  这是一个典型的递归问题

#  已经琢磨得差不多了，但用文字表达出来很费劲。后面再通过MD 文件补充。



hanoi <- function(n,from = "A",to = "C", aux = "B"){
    if(n == 1){
        cat(sprintf("move disk 1 from %s to %s\n", from, to))
    }
    else {
        hanoi(n-1, from,aux,to)
        cat(sprintf("move disk %d from %s to %s\n", n, from, to))
        hanoi(n-1, aux, to, from)
    }
}

# 示例
hanoi(n = 5)
