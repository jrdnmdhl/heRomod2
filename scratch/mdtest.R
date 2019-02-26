# func_text <- heRomod2:::evaluate.heRoModel %>%
#   capture.output()
# 
# text <- paste0(c('# TEST', '', '```r', func_text, '```'), collapse = '\n')
# 
# fileConn <- file("output.txt")
# writeLines(text, fileConn)
# markdown::markdownToHTML('output.txt', 'output.html')
