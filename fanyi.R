# # 调用百度翻译API接口  编写一个小函数，实现翻译自由


library(openssl)
library(httr)
library(jsonlite)
library(tidyverse)

fanyi <- function(q = "",
                  from = "zh",
                  to = "en",
                  appid = "20230701001730181",
                  key = "WK5o4T76ljpfy45QBSq3",
                  salt = salt,
                  encoding = "utf-8"){
    library(openssl)
    library(httr)
    library(jsonlite)
    library(tidyverse)
    
    url0 <- "https://fanyi-api.baidu.com/api/trans/vip/translate"
    salt <- rand_bytes(1)
    sign <- paste0(appid, q, salt, key) %>% 
        md5()
    result <- modify_url(url0,
                         query = list(q = q, 
                                      from = from, 
                                      to = to,
                                      appid = appid,
                                      salt = salt,
                                      sign = sign)) %>%
        url(.,encoding = encoding) %>% 
        fromJSON() %>% 
        .[["trans_result"]] %>% 
        .[[1,2]]
    return(result)
}
 

# 测试 中文翻译成英文
print(fanyi("春蚕到死丝方尽，蜡炬成灰泪始干"))

# 测试 英文翻译成中文

fanyi(from = "en",
      to = "zh",
      q = "where there is a will,there is a way"
      )
 




fanyie2c("mean")

fanyi("给我一片蓝天")
