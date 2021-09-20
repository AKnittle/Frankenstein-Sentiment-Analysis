# Text Analysis Support File

# library(pacman)
# p_load(ggplot2)
# p_load(ggwordcloud)
# p_load(tm)
# p_load(knitr)
# p_load(SnowballC)
# p_load(wordcloud)
# p_load(tidytext)
# p_load(tibble)
# p_load(rmarkdown)
# p_load(imager)
# p_load(officer)
# p_load(dplyr)
# p_load(stringr)
# p_load(ngram)
# p_load(textstem)
# p_load(tokenizers)
# p_load(kableExtra)
# p_load(hunspell)
# p_load(XML)
# p_load(xml2)
# p_load(rvest)


# --------------------------------------------------------------------
# Parser that makes text lower-case
# Can also remove words that are "banned" by passing in a list
# Can also fix words that have been "stuck together" (ie. "notsee" to "not see")
# data is parsed given the flags passed in
parserFunc <- function(givenData, noStopWords = TRUE, noNumbers = TRUE, 
                       noPunc = TRUE, noSingleChar = TRUE, noEnglish = TRUE, stripSpace = TRUE, bannedWords = NULL, fixComboWords = NULL){
  # sets lower-case
  givenData <- tolower(givenData)
  if(noNumbers){
    # remove digits
    givenData <- gsub("\\d", " ", givenData)
  }
  if(noPunc){
    # remove punctionuation
    givenData <- gsub("\\W", " ", givenData)
  }
  if(noSingleChar){
    # remove single chars
    # TODO: FIX BUGS
    # try: "\\b\\w{1}\\b"
    givenData <- gsub("(^| ).( |$)", " ", givenData)
  }
  if(noEnglish){
    # remove non-English letters
    givenData <- gsub("[^a-zA-Z]", " ", givenData)
  }
  if(!is.null(bannedWords)){
    for(i in 1:length(bannedWords)){
      givenData <- gsub(bannedWords[i], " ", givenData)
    }
  }
  if(!is.null(fixComboWords)){
    for(i in 1:length(fixComboWords)){
      givenData <- gsub(fixComboWords[[i]][1], fixComboWords[[i]][2], givenData)
    }
  }
  if(noStopWords){
    # remove stop words
    givenData <- removeWords(givenData, stopwords())
  }
  if(stripSpace){
    # Get rid of extra white space
    givenData <- stripWhitespace(givenData)
    givenData <- str_squish(givenData)
  }
  # returned preprocessed data
  return(givenData) 
}


# --------------------------------------------------------------------
# Returns table that shows the top most 20 words used
# colNum is the column to check
# numHead is the number of top results to show
frequentWords <- function(data){
  # get count of words
  count <- as.data.frame(table(data))
  # get frequency of words
  prop <- as.data.frame(prop.table(table(data)))
  # Put together and sort
  wordStats <- left_join(count, prop, by = "data")
  colnames(wordStats) <- c("word", "count", "Proportion")
  wordStats <- arrange(wordStats, desc(count))
  return(wordStats)
}


# --------------------------------------------------------------------
# Get TF-IDF for Tokenized Data
getTF_IDF <- function(tokenWords){
  tfWords <- lapply(1:length(tokenWords), 
                    function(i){
                      unique_words <- unique(tokenWords[[i]])
                      tf_review <- sapply(1:length(unique_words), function(j){sum(tokenWords[[i]] == unique_words[j])})
                      tf_single_review <- (data.frame("pitch_num" = rep(i, length(tf_review)), 
                                                      "word" = unique_words, 
                                                      "count" = tf_review, 
                                                      "total_words" = rep(length(tokenWords[[i]]), length(tf_review))))
                    })
  # Make dataframe
  tempTF <- do.call("rbind", tfWords)
  
  # Get TF-IDF data
  tempTF_IDF <- bind_tf_idf(as_tibble(tempTF), word, pitch_num, count)
  tempTF_IDF <- arrange(tempTF_IDF, desc(tf_idf))
  return(tempTF_IDF)
}


# --------------------------------------------------------------------
# Make Plotting function: Word Cloud
cloudBuilder <- function(wordTable, lowColor="maroon", highColor ="orange"){
  wordCloud <- ggplot(head(wordTable, 15), aes(label = word, size = count, color = count)) + 
    geom_text_wordcloud_area() + 
    scale_color_gradient(low = lowColor, high = highColor) +
    scale_size_area(max_size = 20) + labs(title = "Term Frequency:")
  return(wordCloud)
}

# Make Plotting function: Bar Graph
buildBar <- function(wordTable, lowColor="maroon", highColor ="orange"){
  barPlot <- ggplot(head(wordTable, 15), aes(x = reorder(word, count), y = count)) + 
    geom_bar(stat = "identity", aes(fill = count)) + 
    labs(title = "Term Frequency: ", x = "Word", y = "Word Count") + 
    coord_flip() + 
    theme(legend.position = "none", axis.text=element_text(size = 16), 
          axis.title.y = element_text(size = 18), axis.title.x=element_text(size = 18), 
          plot.title = element_text(size = 20)) + 
    geom_text(stat = "identity", aes(label = count), hjust = -0.3, size = 4.5) +  
    scale_fill_gradient(low = lowColor, high = highColor)
  return(barPlot)
}



# Code from Get Chapters Chunk ----------------------------------------------------------------------------------

# Remove html tags
cleanFun <- function(line) {
  return(gsub("<.*?>", "", line))
}

# Get chapter text between 2 points
getChapterText <- function(vec, startIndex=0, endIndex=1){
  vec <- vec[startIndex:endIndex]
  return(vec)
}

# Get all the text for every chapter
getBookText <- function(vec, chapterBreakLines = chapterLineNum){
  book <- list()
  # Get chapter lines
  numChapters <- length(chapterBreakLines)
  index <- 1
  # Go through the chapter starting/ending lines
  while(index < numChapters){
    startI <- index
    endI  <- index+1
    startLine <- chapterBreakLines[startI]
    endLine <- chapterBreakLines[endI]
    # Remove the extra line that has the next chapter title in it
    endLine <- endLine - 1
    chapter <- getChapterText(vec, startLine, endLine)
    # Clean up tags
    for(currLine in 1:length(chapter)){
      chapter[currLine] <- cleanFun(chapter[currLine])
    }
    #chapter <- sapply(chapter, paste, collapse = " ")
    chapter <- paste(chapter, collapse = '')
    #print(chapter)
    # Add Chapter to book
    book[[index]] <- chapter
    index <- index +1
  }
  return(book)
}















