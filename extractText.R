set.seed(1111)
library(parallel)

cl <- makeCluster(3, type = "FORK")

extractText <- function(filePath, portion) {
  patt <- "^(en_US\\.)([a-z]{1,7})(\\.txt$)"
  en_US.Dir <- "./final/en_US/"
  
  if (grepl(patt, filePath)) { # Only for the files matching the pattern
    
    con <- file(paste(en_US.Dir, filePath, sep = ""), "r")
    ds <- readLines(con)
    close(con)
    
    len <- length(ds)
    
    ds[sample(len, len * portion)]
    
  } else {
    NULL 
  }
}

text.list <- parSapply(cl, list.files("./final/en_US/"), extractText, 0.5)

# Merge the extracted samples to one list only and remove all weird characters non latin characters
allText <- iconv(
  unlist(
    rbind(text.list)
  ), 
  "latin1", 
  "ASCII", 
  sub=""
)

# Remove not needed variables
rm(text.list)

len = length(allText)
inTrain <- sample(len, len * 0.7)
trainningSet <- allText[inTrain]

testNValidation <- allText[-inTrain]
len = length(testNValidation)
inTest <- sample(len, len * 0.5)
testSet <- testNValidation[inTest]
validationSet <- testNValidation[-inTest]

saveRDS(trainningSet, "trainningSet.rds")
saveRDS(testSet, "testSet.rds")
saveRDS(validationSet, "validationSet.rds")

stopCluster(cl)
rm(list = ls())
