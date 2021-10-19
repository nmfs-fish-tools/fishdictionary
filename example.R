# install.packages("pdftools")
# install.packages("tm")
# install.packages("wordcloud") # word-cloud generator
# install.packages("RColorBrewer") # color palette
library(pdftools)
library(tm)
library(wordcloud)
library(RColorBrewer)

output <- text_mining(
  text_directory="C:/Users/chris/Documents/GitHub/data_dictionary/StockAssessment",
  lower_case=TRUE,
  remove_numbers=TRUE,
  remove_common_words=TRUE,
  custom_stopwords=NULL
)
