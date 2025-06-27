
library(tidyverse)
library(mixexp) 

#######
### SLD 和SCD 是混料设计里最基础的两个函数。用于生成SLD 设计和SCD 设计。

### R语言中已有成熟的包mixexp 来实现混料设计功能。

### 这里使用该包函数生成设计案例，并使用tidyverse风格重写对应函数。

### 重写的函数效率并不会优于mixexp包中的函数，更多出于练习R语言函数编写，

###以及从底层了解掌握SLD 和SCD 实验设计的目的





### SLD###

### 关于SLD:构造 Simplex Lattice Design（单纯型格点设计）, 由因素数量和水平两个参数决定。

### 混料设计是比较成熟的内容，有诸多参考内容，我们参考minitab官方材料：

### https://support.minitab.com/zh-cn/minitab/help-and-how-to/statistical-modeling/doe/supporting-topics/mixture-designs/choose-a-mixture-design/



# 首先对分量数进行检验判定，比如，分量数不能为1，且需要为自然数；

# 同时，分量数也不能太大，会导致大量的试验组数，也与实践不相符；

# 实践中，用于混料设计的成分分量数不会太多，6个以内为宜，通常只做3-4个。

# 如果需要很多分量，需要用D-优化设计等其他方法手段进行试验设计。



check_nfac_valid <- function(nfac, max_nfac = 6) {
    if (!(is.numeric(nfac) && nfac %% 1 == 0 && nfac > 1)) {
        stop("nfack 必须是大于 1 的自然数")
    }
    if (nfac > max_nfac) {
        stop(glue::glue("当前函数建议变量数不超过 {max_nfac}，以防止设计点数过多导致内存与计算崩溃。如需更高维设计，请考虑最优设计法（如 D-optimal）"))
    }
}


# 相对于mixexp包中SLD 函数，这里对分量名称进行优化，允许用户自定义分量名，

# 这样对试验设计实操更有帮助，符合实际需求。

# 当用户指定分量名时，使用用户指定的分量名，否则用X1,X2,X3...

#  这个函数很简单，先检查用户是否指定了分量名

#  然后看分量名个数是否与分量数一致，如果不一致，那就使用默认值

# 如果没有指定，也使用默认值

# 其实还需要考虑用户提供的分量名称是否规范，如不规范，有些时候是不能作为列名使用的


validate_varnames <- function(varnames, nfac) {
    check_nfac_valid(nfac)  # 增加对 k 的检查
    if (!is.null(varnames)) {
        if (length(varnames) != nfac) {
            warning("变量名数量与因子数不一致，自动使用默认变量名")
            return(paste0("x", 1:nfac))
        }
        make.names(varnames, unique = TRUE)
    } else {
        paste0("x", 1:nfac)
    }
}


#### 构建SLD 函数

# 这个其实很简单，依据混料设计的特点，各分量合计值为1.

# 根据水平数设置，对各分量排列0到水平数，满打满算的排列

# 然后排列的值全部除以水平数

# 找出求和结果等于1的那些组合，其他组合都干掉，就实现了SLD 设计。

# 以3个分量2水平设计为例：

# A,B,C三个分量，0,1,2 三个数字进行拍列

# 显然就有3的3次方，27种组合{000,001，002，010，...333}

# 这些组合中各数除以2后得到的数值相加等于1，就满足我们的试验设计需求。
 
tidy_sld <- function(nfac = 3, 
                     level = 2,
                     varnames = NULL, 
                     randomize = TRUE) {
    vars <- validate_varnames(varnames, nfac)
    expand_grid(!!!set_names(rep(list(0:level), ## 这里!!!涉及到R 语言编程相对高级的技能,入参，这里不详细展开，在其他章节会重点涉及。
                                 nfac), 
                             vars)) %>%
        filter(abs(rowSums(across(all_of(vars))) - level) < 1e-6) %>% # 这里整合求和等于2，但写成相减小于一个趋近于0的值，也是有讲究的，关乎到浮点数精度的考量。
        mutate(across(all_of(vars), ~ .x / level)) %>%
        {
            if (randomize) sample_n(., nrow(.)) %>% mutate(run = row_number()) else mutate(., run = row_number())
    # 这里增加了一个实验运行序号的字段。并设置随机
                } %>%
        select(run, everything())
}

# 例子
tidy_sld(nfac = 3,level = 2)
tidy_sld(nfac = 4,level = 3,varnames = c("面粉","白糖","发酵粉","酸奶"))


### 2. 构建 Simplex Centroid Design (SCD)

## 关于SCD:构造 Simplex-centroid design（单纯型质心设计）

# SCD 相对复杂一点，只需要分量数一个参数，根据分量数来设计，因为有质心点，

# 假设分量数为k,设计组数为2的k次方-1.比如3个分量的SCD 设计，有7个点，即7组设计。

# 其实现的逻辑概述如下，建设有k个分量，先列好1,2,3，...k，然后从1:k这组数里分别做1,2,3,...k的组合。

# 123..k这些列好的数与这些组合的进行比对，在里头的，全部赋值1除以组合取值数，不在里头的赋值为0

# 全部组合起来就成了设计结果。

# 以分量数3为例
# 1,2,3先排好
# 取组合，显然有：
# 1.C(3,1)结果为{1,2,3}这三种，比对下来就得到{(0,0,1),(0,1,0){1,0,0}}三个组合

# 2.C(3,2)结果为{(1,2),(1,3),(2,3)}这三种，比对下来就得到{(1/2,1/2,0),(1/2,0,1/2){01/2,1/2}}三个组合

# 2.C(3,23)结果为{(1,2,3) }这一种，比对下来就得到{(1/3,1/3,1/3)}一个组合

# 合计7个组合。

tidy_scd <- function(nfac, varnames = NULL, randomize = TRUE) {
    vars <- validate_varnames(varnames, nfac)
    map_dfr(1:nfac, function(i) {
        combn(nfac, i, simplify = FALSE) %>%
            map_dfr(~{
                tibble(x = seq_len(nfac)) %>%
                    mutate(value = if_else(x %in% .x, 1 / i, 0)) %>%
                    pivot_wider(names_from = x, values_from = value, names_prefix = "x")
            })
    }) %>%
        rename_with(~ vars, starts_with("x")) %>%
        distinct() %>%
        {
            if (randomize) sample_n(., nrow(.)) %>% mutate(run = row_number()) else mutate(., run = row_number())
        } %>%
        select(run, everything())
}

# 举例

tidy_scd(3)
    
tidy_scd(nfac = 4, varnames = c("面粉","白糖","发酵粉","酸奶"))



## mixexp 函数举例

SLD(3,2)
SCD(3)
SCD(4)


