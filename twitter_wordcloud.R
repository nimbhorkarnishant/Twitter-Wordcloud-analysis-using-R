#install.packages("twitteR", dependencies=TRUE)
#install.packages("RCurl")
#install.packages('bitops')
#install.packages('base64enc')
#install.packages('httpuv')
#install.packages('tm')
#install.packages('wordcloud')
#install.packages("stringr")

libs = c("twitteR", "RCurl","bitops","base64enc","httpuv","tm", "stringr", "wordcloud")
lapply(libs, require, character.only=TRUE)

options(stringsAsFactors = FALSE)


create_file="C:/Users/Nishant Nimbhorkar/Desktop/Data science data/descriptive/Skill develpemrnt/R file/twitter_oauth.txt"


#function for authentication of twitter api


oauthCreds = read.table(create_file,header=T)
setup_twitter_oauth(oauthCreds$consumer_key,
                    oauthCreds$consumer_secret,
                    oauthCreds$access_token,
                    oauthCreds$access_secret) 

oauthCreds

#function for getting tweets

search_term=c("india + south africa +cricket")
no_search_term=300

tweets_list = searchTwitter(search_term,lang="en",n=no_search_term,resultType="recent")
tweets_list[[1]]

#length(tweets_list)
#class(tweets_list)
#return(tweets_list)

tweets_text = sapply(tweets_list, function(x) x$getText())
tweets_text


tweets_corpus = Corpus(VectorSource(tweets_text))
tweets_corpus

#see the first tweet in the tweet corpus

inspect(tweets_corpus[1:3])

# data or text preprocessing

tweets_corpus_clean = tm_map(tweets_corpus, removePunctuation)
tweets_corpus_clean = tm_map(tweets_corpus_clean, stripWhitespace)
tweets_corpus_clean = tm_map(tweets_corpus_clean, removeNumbers)
tweets_corpus_clean = tm_map(tweets_corpus_clean, removeWords, stopwords("english"))
tweets_corpus_clean = tm_map(tweets_corpus_clean, content_transformer(tolower))
toSpace = content_transformer(function(x, pattern) gsub(pattern,"",x))
tweets_corpus_clean = tm_map(tweets_corpus_clean, toSpace,"https*|youtu*")

tweets_corpus_clean

#TDM(term document matrix) contains the words and their frequency. It is a simple_triple_matrix

tweets_tdm = TermDocumentMatrix(tweets_corpus_clean)
tweets_tdm
class(tweets_tdm)

str(tweets_tdm)

#Create a dataframe for words and their frequency in the term document matrix

#tweets_tdm = as.matrix(tweets_tdm)
#tweets_tdm
#class(tweets_tdm)
#View(tweets_tdm)
#str(tweets_tdm)
#head(tweets_tdm)

#wordcloud(tweets_tdm)
tweets_tdm = as.matrix(tweets_tdm)
class(tweets_tdm)
#[1] "matrix"
str(tweets_tdm)
###################
# num [1:1409, 1:400] 0 0 0 0 0 0 0 0 0 0 ...
# - attr(*, "dimnames")=List of 2
# ..$ Terms: chr [1:1409] "-fantastic""| __truncated__ "'blade""| __truncated__ "'the""| __truncated__ ""ui""| __truncated__ ...
# ..$ Docs : chr [1:400] "1" "2" "3" "4" ...
###############################

tdm_term_freq_sort = sort(rowSums(tweets_tdm), decreasing=TRUE)
#class(tdm_term_freq_sort)
#[1] "numeric"
#str(tdm_term_freq_sort)
##############
# Named num [1:1409] 82 78 78 78 78 69 62 53 46 46 ...
# - attr(*, "names")= chr [1:1409] "connect" "carmack" "john" "keynote" ...
#
##################################

tdm_term_freq_sort_inc = sort(rowSums(tweets_tdm), decreasing=FALSE)
#class(tdm_term_freq_sort_inc)
#[1] "numeric"
#str(tdm_term_freq_sort_inc)
##############
# Named num [1:1409] 1 1 1 1 1 1 1 1 1 1 ...
# - attr(*, "names")= chr [1:1409] "-fantastic""| __truncated__ "'blade""| __truncated__ "'the""| __truncated__ ""ui""| __truncated__ ...
##################################

tdm_term_freq_df = data.frame(word = names(tdm_term_freq_sort),
                              freq = tdm_term_freq_sort)
str(tdm_term_freq_df)
head(tdm_term_freq_df,10)

View(tdm_term_freq_df)

# plot wordcloud


wordcloud(words = tdm_term_freq_df$word,
          freq= tdm_term_freq_df$freq,
          min.freq=5,
          max.words=300,
          random.order=FALSE,
          rot.per=0.35,
          colors=brewer.pal(8,'Dark2'),
          scale=c(3,0.5))




barplot(tdm_term_freq_df[1:10,]$freq,
        las =2,
        names.arg = tdm_term_freq_df[1:10,]$word,
        col="lightblue",
        main="Most frequent words",
        ylab = "Word frequencies")

