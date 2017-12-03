# This is the function that actuually makes the word prediction based on the 
# preceeeding words. It requires that the nGrams are already loaded into the 
# workspace

library(dplyr)

# Check that the data is available:
if (!exists('biGrams')){
    load('nGramData3.RData')
}

nGramPredictor <- function (pattern) {
    
    # Remove any puntucation
    # pattern <- gsub ('\'|#|?|!|;|&|;|;|\"', '',pattern )
    pattern <- gsub ('[[:punct:]]|[[:alpha:]]', '', pattern)
    
    # Remove leading or trailing space
    pattern <- gsub ('^ ', '',pattern )
    pattern <- gsub (' $', '',pattern )
    
    pattern <- tolower(pattern)
    
    
    # numWords <- length(gregexpr("\\W+", pattern)[[1]]) + 1
    numWords <- sum (gregexpr("[[:alpha:]]+", pattern)[[1]]>0)
    
    
    if (numWords > 4) {
        pattern <- strsplit (pattern, ' ')[[1]] %>%
            tail (4) %>%
            paste (collapse = ' ')
    }
    
    
    if(numWords == 0 | is.na(numWords)) {
        # rands <- sample.int(length(monoGram),3)
        prediction <- monoGram[sample.int(length(monoGram),3)]
        return(prediction)
    } else if(numWords == 1) {
        nGram <- biGrams
    } else if(numWords == 2) {
        nGram <- triGrams
    } else if(numWords == 3) {
        nGram <- quadGrams
    } else {
        nGram <- quintGrams
    } #else { #i.e numwords > 4
        # only use the last four words
      
       # nGram <- quintGrams
#    }
    
    
    
    # Get the most popular phrase strting with my Pattern
    prediction <- nGram [nGram$phrase == pattern, 2:ncol(nGram)]
    # Check to ensure the ngram caught something
    if (nrow(prediction)==0) {
        # drop the first word and reacall this function recursivly.
        # it will eventually reach numWords = 0 and escape the loop
        pattern <- strsplit (pattern, ' ')[[1]] %>%
            tail (numWords - 1) %>%
            paste (collapse = ' ')
        prediction <- nGramPredictor (pattern)
    }
    
    
    
    
    # Check if any of the values are nas
    # if so, fil them up with predictions
    while (is.na(prediction)%>%sum > 0) {
        
        empty.preds <- is.na(prediction)%>%sum
        if (empty.preds == 2) {
            empty.cols <- c(2,3)
        } else {
            empty.cols <- 3
        }
        
        pattern <- strsplit (pattern, ' ')[[1]] %>%
            tail (numWords - 1) %>%
            paste (collapse = ' ')
        
        prediction.new <- nGramPredictor (pattern)
        
        prediction[empty.cols] <- prediction.new [empty.cols]
    }
    
        
    rm(nGram)
    
    as.character(prediction)
}