source('PredictFunc3.R')

sampled_loader <- function (type, samplePerCen) {
    # Takes a type (eg news, blog or twitter) and returns a fraction of the
    # loaded enteriest
    fpath <- 'C:/Users/USER/Documents/Courses/Johns Hopkins Data Science Specializition/10  Capstone/Coursera-SwiftKey/final/'
    fname <- paste0(fpath, 'en_US', '/', 'en_US', '.', type, '.txt')
    
    con <- file(fname, "r")
    corp <- readLines(con)
    close(con)
    # Randomly choose a selection of the loaded values
    randSample <- rbinom(length(corp), size = 1, prob = samplePerCen/100) %>%
        as.logical()
    
    corp <- corp[randSample]
    corp
    
}


# load a small fraction of the twitter data
test.set <- sampled_loader('twitter', 0.05)
print (paste('data loaded with', length(test.set), 'random tweets'))


stringSplitter <- function(string) {
    # Return a vector with 2 enteries. The first is the words of the string up
    # to a random word, the other collumn is the following word
    
    numWords <- sum (gregexpr("[[:alpha:]]+", string)[[1]]>0)
    # Randomly choose a split point in each entry. A random number from 1 to
    # the number of words -1 is chosen.
    # The -1 is used so that the last word cannot be chosen
    
    splitPoint <- sample.int(numWords-1, 1)
    
    fistWords <- strsplit(string, ' ')[[1]] %>%
        head (splitPoint) %>%
        paste (collapse = ' ')
    
    nextWord <- strsplit(string, ' ')[[1]][splitPoint+1]
    
    c(fistWords, nextWord)
}

# Update test.set to be randomly split 
test.set.split <- sapply(test.set, stringSplitter)%>%
    t () %>%
    as.data.frame(stringsAsFactors = FALSE)

# bit of tidying
rownames(test.set.split) <- 1:nrow(test.set.split)

predict.set <- sapply(test.set.split$V1, nGramPredictor)

# convert to a df
predict.set <- data.frame(matrix(temp, ncol=3, byrow=T),
                           stringsAsFactors=FALSE)

# function to test if the prediction matches the correct value
f <- function (index) {
    test.set.split$V2[index] %in% predict.set[index,]
}

accuracy <- sapply(1:nrow(predict.set), f)%>%
    mean
