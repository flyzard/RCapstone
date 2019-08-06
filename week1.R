library(tidyverse)

dataFinalFolder <- "./final/"
if(!dir.exists(dataFinalFolder)) {
  if(!file.exists("./final/en_US/en_US.blogs.txt") & !file.exists("./final/en_US/en_US.news.txt") & !file.exists("./final/en_US/en_US.twitter.txt")){
    url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
    zip_file_name <- "capstone.zip"
    download.file(url, zip_file_name)
    unzip(zip_file_name)
  }
}

## 1 to 3 - calculating file size, number of lines, bigger line, and number of words for each file
datafolder <- "./final/en_US/"
flist <- list.files(path=datafolder, recursive=T, pattern=".*en_.*.txt")
l <- lapply(paste(datafolder, flist, sep="/"), function(f) {
  fsize <- file.info(f)[1]/1024/1024
  con <- file(f, open="r")
  lines <- readLines(con)
  nchars <- lapply(lines, nchar)
  maxchars <- which.max(nchars)
  nwords <- sum(sapply(strsplit(lines, "\\s+"), length))
  close(con)
  return(c("file" = f, format(round(fsize, 2), nsmall=2), "nlines" = length(lines), "biggerLine" = maxchars, "nwords" = nwords))
})
l

twiter_path <- "./final/en_US/en_US.twitter.txt"
con <- file(twiter_path, open="r")
twiter_lines <- readLines(con)
close(con)
## 4 - Divide the number of lines where the word "love" occurs by the number of lines the word "hate"
sum(grepl("love", twiter_lines)) / sum(grepl("hate", twiter_lines))

# 5 - The one tweet in the en_US twitter data set that matches the word "biostats" says what?
twiter_lines[grepl("biostats", twiter_lines)]

# 6 - How many tweets have the exact characters "A computer once beat me at chess, but it was no match for me at kickboxing".
length(twiter_lines[grepl("A computer once beat me at chess, but it was no match for me at kickboxing", twiter_lines)])
