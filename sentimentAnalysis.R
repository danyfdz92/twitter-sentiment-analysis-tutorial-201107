require(plyr)
require(stringr)

pos.file <- file.choose() #choosing the file
pos.file
pos.words <- read.csv(file = pos.file, header = FALSE, stringsAsFactors = FALSE)

neg.file <- file.choose() #choosing the file
neg.file
neg.words <- read.csv(file = neg.file, header = FALSE, stringsAsFactors = FALSE)

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{

  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)

    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    #unlist positive and negative words dictionaries
    pos.words <- unlist(pos.words, recursive = TRUE)
    neg.words <- unlist(neg.words, recursive = TRUE)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    #print(pos.matches)
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)

    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

#Sentiment score
#Score below 0 is considered as Negative 
#all the scores above 0 are considered as positive
#0 is neutral text

result <- score.sentiment("this is super positive abundance great adorable text",pos.words,neg.words)
result$score

#Analyzing tweets
tweets.file <- file.choose() #choosing the file
tweets.file
tweets <- read.csv(file = tweets.file, header = TRUE, stringsAsFactors = FALSE)
result <- score.sentiment(tweets$text,pos.words,neg.words)
positiveTweets <- result[result$score > 0,]
negativeTweets <- result[result$score < 0,]
neutralTweets <- result[result$score == 0,]