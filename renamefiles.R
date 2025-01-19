 # 对文件夹内的文件进行操作的一个实际例子
# 利用字符串处理方法进行批量处理，复杂场景需要使用正则表达式

# 这样可以高效的对文件进行批量重命名

library(tidyverse)

# 文件夹名
folderpath <- "E:/english word/OPD"
#就文件名
files <- list.files(folderpath)
# 新文件名
files_new <- str_remove_all(files,"新的一年，从这本有声书开始巩固英语日常词汇！") %>% 
    str_replace_all("\\(","") %>% 
    str_replace_all("\\)","") %>% 
    str_replace_all("\\[01\\]\\.mp4","\\[01\\]\\.mp3")

# 重命名
file.path(folderpath,files) %>% 
    file.rename(file.path(folderpath,files_new))
                
 
    
    
