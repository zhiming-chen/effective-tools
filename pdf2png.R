# 每天有大量需求，将pdf转换为png，这个脚本就是用来实现这个功能的
# 我的实际场景是每天在一个文件夹内，将当天的pdf转换为png
# 市面上有很多工具可以实现这个功能，但是我还是选择了R语言来实现这个功能：基本做到了一键生成。
# 如果配置好命令行工具，写一个bat脚本来调用这个R脚本，那么就可以实现一键生成了。

# Load required library
library(pdftools)

# Function to convert PDF to PNG
convert_pdf_to_png <- function(input_folder, output_folder) {
    if (!dir.exists(input_folder)) {
        stop("Input folder does not exist.")
    }
    
    if (!dir.exists(output_folder)) {
        dir.create(output_folder, recursive = TRUE)
    }
    
    pdf_files <- list.files(input_folder, pattern = "\\.pdf$", full.names = TRUE)
    if (length(pdf_files) == 0) {
        stop("No PDF files found in the input folder.")
    }
    
    for (pdf_file in pdf_files) {
        base_name <- tools::file_path_sans_ext(basename(pdf_file))
        png_output <- file.path(output_folder, paste0(base_name, "_page%d.png"))
        
        # Render PDF to PNG using pdftools
        tryCatch({
            pdftools::pdf_convert(pdf_file, dpi = 300, filenames = png_output)
            message(paste("Converted:", pdf_file, "to PNG in", output_folder))
        }, error = function(e) {
            warning(paste("Failed to convert:", pdf_file, "-", e$message))
        })
    }
}

# 以下部分为命令行工具内实施，还需要调试。
# For command-line execution
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 2) {
    input_folder <- args[1]
    output_folder <- args[2]
    convert_pdf_to_png(input_folder, output_folder)
} else if (interactive()) {
    # Example usage in interactive R session
    convert_pdf_to_png("input_pdf_folder", "output_png_folder")
} else {
    stop("Usage: Rscript convert_pdf_to_png.R <input_folder> <output_folder>")
}

#  在文件夹内，如果不使用命令行工具，我只需要执行如下两行代码即可

# 将上述代码保存为"E:/english word/pdf2png.R"，(包含地址，地址可以自己定)然后执行如下代码



# source("E:/english word/pdf2png.R")

# convert_pdf_to_png(input_folder = getwd(),output_folder = getwd())

# 就实现了将当前文件夹内的pdf转换为png的功能。
# 其他文件夹执行时，我直接将两行代码的脚本文件拷贝过去，打开运行即可。因为每天就一次操作，这个倒可以接受，时间上差不多。
# 后续有时间，我会尝试将这个脚本封装成一个bat脚本，实现一键生成。



