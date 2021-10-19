#' Plot a word cloud with text mining
#'
#' A function returns a figure of a word cloud after text mining
#'
#' @import pdftools
#' @import tm
#' @import wordcloud
#' @import RColorBrewer
#'
#' @param text_directory - A path that contains all pdf files
#' @param lower_case - Convert text to lower case or not
#' @param remove_numbers - Remove numbers in the text or not
#' @param remove_common_words - Remove common english words or not
#' @param custom_stopwords - Customized words to be removed
#'
#' @return output a list object containing the text matrix, frequent words, and original text
#'
#' @export
#'

text_mining <- function(text_directory,
                        lower_case=TRUE,
                        remove_numbers=TRUE,
                        remove_common_words=TRUE,
                        custom_stopwords=NULL
                        ){
  setwd(file.path(text_directory))

  # Read names of files
  files <- list.files(pattern = "pdf$")

  # Create metadata of files
  docs <- Corpus(URISource(files),
                 readerControl = list(reader = readPDF))

  # Convert symbols to space
  toSpace <- content_transformer(
    function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")

  # Convert text to lower case
  if (lower_case==TRUE) docs <- tm_map(
    docs,
    content_transformer(tolower))

  # Remove numbers
  if (remove_numbers==TRUE) docs <- tm_map(
    docs,
    removeNumbers)

  # Remove english common stopwords
  if (remove_common_words==TRUE) docs <- tm_map(
    docs,
    removeWords,
    stopwords("en"))

  # Remove your own stop words
  # specify your stopwords as a character vector
  if (!is.null(custom_stopwords)) docs <- tm_map(
    docs,
    removeWords,
    custom_stopwords)

  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)

  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)

  # Format docs
  output <- list()
  output$matrix <- TermDocumentMatrix(docs)
  m <- as.matrix(output$matrix)
  v <- sort(rowSums(m), decreasing=TRUE)
  d <- data.frame(word=names(v), freq=v)
  output$freqwords <- findFreqTerms(output$matrix, lowfreq = 4)
  output$docs <- docs

  jpeg(filename=file.path(text_directory, "word_cloud.jpg"),
       width=130, height=150,
       units="mm", res=1200)
  wordcloud(words=d$word, freq=d$freq, min.freq=1,
            max.words=200, random.order=FALSE, rot.per=0.35,
            colors=brewer.pal(8, "Dark2"))
  dev.off()

  jpeg(filename=file.path(text_directory, "frequent_words.jpg"),
       width=130, height=150,
       units="mm", res=1200)
  barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
          col ="lightblue", main ="Most frequent words",
          ylab = "Word frequencies")
  dev.off()

  return(output)
}




