#initialising

library(dplyr)
library (tm)
library (ngram)
library(parallel)
library(reshape2)
set.seed(123)

# Loader ----------------------------------------------------------------------


sampled_loader <- function (type, samplePerCen) {
    # Takes a type and language and returns fraction of the loaded results
    fpath <- 'C:/Users/USER/Documents/Courses/Johns Hopkins Data Science Specializition/10  Capstone/Coursera-SwiftKey/final/'
    fname <- paste0(fpath, 'en_US', '/', 'en_US', '.', type, '.txt')
    
    con <- file(fname, "r")
    corp <- readLines(con)
    close(con)
    
    randSample <- rbinom(length(corp), size = 1, prob = samplePerCen/100) %>%
        as.logical()
    
    corp <- corp[randSample]
    
    corp
    
}

myDocTidy <- function(doc){
    
    # any single charater except a and i
    xprsn <- '(?: )[^a|i](?: )'
    doc <- gsub(xprsn, ' ', doc)
    
    #NoMoreHashTags
    xprsn <- '#[a-z]+'
    doc <- gsub(xprsn, ' ', doc)
    
    # Very long words
    xprsn <- '(?: )[a-z]{11,}(?: )'
    doc <- gsub(xprsn, ' ', doc)
    
    # urls
    xprsn <- '(www|http)[^ ]*'
    doc <- gsub(xprsn, ' ', doc)
    
    # Due to the encodeing, some words have Tm, or dollar signs etc - remove these words
    # regex 0 or more letters followed by at least one not (letter, space, or punctuation) followed
    # by 0 or more letters
    xprsn <- '([a-z]*[^(a-z| |[[:punct:])]+[a-z]*)'
    doc <- gsub(xprsn, ' ', doc)
    
    doc
}


## Making the corpus ---------------------------------------------------------------
corpusBuilder <- function (samplePerCen, retCorp = FALSE) {
    # retCorp - this sunction can either return a tm comrpus OR 
    # all of the text smashed into one large text doc
    
    blogs <- sampled_loader ('blogs', samplePerCen)
    news <- sampled_loader ('news', samplePerCen)
    tweets<- sampled_loader ('twitter', samplePerCen)

    doc.corpus <- Corpus(VectorSource(paste (blogs, news, tweets)))
    rm (list = c('blogs', 'news', 'tweets'))
    
    # Do the tidying
    doc.corpus <- tm_map(doc.corpus, tolower)
    doc.corpus <- tm_map(doc.corpus, myDocTidy)
    doc.corpus <- tm_map(doc.corpus, removePunctuation)
    doc.corpus <- tm_map(doc.corpus, removeNumbers)
    doc.corpus <- tm_map(doc.corpus, stripWhitespace)
    
    # smash the doc back into a a single 'document'
    allText <- concatenate ( lapply ( doc.corpus , "[", 1))
    
    if (retCorp) {
        return (doc.corpus)
    } else {
        return (allText)
    }
    
}

## Loading data ---------------------------------------------------------------

### Tidy the data ------------------------------------------------------------

# Usefull functions ----------------------------------------------------------

firstWordGetter <- function (strng) {
    strng <- as.character(strng)
    strsplit(strng, ' ')[[1]] %>%
        head (1)
}

lastWordGetter <- function (strng) {
    strsplit(strng, ' ')[[1]] %>%
        tail (1) %>%
        paste(collapse = ' ')
}

lastWordRemover <- function (strng) {
    strsplit(strng, ' ')[[1]] %>%
        head (-1) %>%
        paste(collapse = ' ')
}

# Build the monoGram ---------------------------------------------------------
# find all the first words in the text

print('Making the one-Gram')
start.time <- Sys.time()
doc.corpus <- corpusBuilder (10, retCorp = TRUE)

monoGram <- sapply(doc.corpus, firstWordGetter) %>%
    table() %>%
    sort(decreasing = TRUE)

# Remove an empty entry
monoGram <- monoGram[!names(monoGram[1:5])=='']
# only take a subset of the words
monoGram <- head(monoGram, 60)
# the actual strings are the row names
monoGram <- names(monoGram)
rm (doc.corpus)

time.taken <- round(Sys.time() - start.time,2)
print (paste('onegram complete. time taken:', time.taken, 'seconds'))


# Make the ngrams -------------------------------------------------------------

myNgramMaker <- function (doc, n) {
    
    # Make the ngram ----------------------------------------------------------
    print (paste('building the ngram, n =', n))
    myNgram <- ngram (doc, n=n, sep =' ')
    # Get a df of the ngram phrases
    print (paste('getting the phrasetable, n =', n))
    df.ngram <- (get.phrasetable (myNgram))
    
    # Remove low frequency enteries -----------------------------------------
    # Reduce the size of the df by removeing low frequency enteries
    # i.e. things occuring less than 3 times
    size.pre <- nrow(df.ngram)
    # remove phrases which occur less then 2 times
    df.ngram <- df.ngram[df.ngram$freq>2, ]
    print(paste('number of n-gram enteries after thinning', nrow(df.ngram),
                '. This is a reduction of',
                (size.pre - nrow(df.ngram))/size.pre*100 ,'%'
    ))
    
    
    # Remove repeated phrases ------------------------------------------------
    
    # Build a df with the first n-1 words in the 1st col
    # And the nth word in the 2nd col
    cl <- makeCluster(detectCores() - 1)
    clusterEvalQ(cl, library(dplyr))
    df.ngram <- data.frame(
        phrase = parSapply(cl, df.ngram$ngrams, lastWordRemover),
        prediction = parSapply(cl, df.ngram$ngrams, lastWordGetter),
        stringsAsFactors = FALSE
    )
    
    # Convert the ngram from long format to wide format
    df.ngram <- aggregate(prediction~phrase, df.ngram, c)%>%
        as.data.frame
    
    # We only want the unique predictions for each pattern
    # because the ngram is originally ordered by frequency,
    # choosing the unique values will also be sorted by most common first
    prediction.unique <- parLapply(cl, df.ngram$prediction, unique)
    # We only want 1 predictions per entry
    prediction.unique <- parLapply(cl, prediction.unique, head, 3)
    # The prediction is stored as a list
    # break it out into df entereies, with NA's for empty spaces
    prediction.unique <- plyr::ldply(prediction.unique, rbind)
    # During the conversion to a df all cols were converted to factors
    # [] is used to return  a df, not a list
    # For some reason parLapply cannot be used here - jibbierish is returned
    prediction.unique [] <- lapply (prediction.unique, as.character)
    # remove an unused column
    prediction.unique <- subset (prediction.unique, select = -.id)
    
    stopCluster(cl)
    
    # Put it all back together
    df.ngram <- data.frame(phrase = df.ngram$phrase,
                           prediction.unique,
                           stringsAsFactors = FALSE
    )
    
    
    # Return
    df.ngram
}




print ('Making the ngrams...')
start.time <- Sys.time()
allText <- corpusBuilder (3)
biGrams <- myNgramMaker (allText, 2)
time.taken <- round(Sys.time() - start.time,2)

print (paste('biGrams complete (1/4). time taken:', time.taken, 'seconds'))

start.time <- Sys.time()
allText <- corpusBuilder (5)
triGrams <- myNgramMaker (allText, 3)
time.taken <- round(Sys.time() - start.time,2)
print (paste('triGrams complete (2/4). time taken:', time.taken, 'seconds'))

start.time <- Sys.time()
allText <- corpusBuilder (15)
quadGrams <- myNgramMaker (allText, 4)
time.taken <- round(Sys.time() - start.time,2)
print (paste('quadGrams complete (3/4). time taken:', time.taken, 'seconds'))

start.time <- Sys.time()
allText <- corpusBuilder (15)
quintGrams <- myNgramMaker (allText, 5)
time.taken <- round(Sys.time() - start.time,2)
print (paste('quintGrams complete (4/4). time taken:', time.taken, 'seconds'))

# print a new line. when sourcing this file is wrapped in system.time()
# New line to make printing nicer
cat('\n')


save (monoGram, biGrams,triGrams,quadGrams, quintGrams, file = 'nGramData3.RData')

