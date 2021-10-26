
library(pdftools)
library(tm)
library(stringr)
library(wordcloud)
library(RColorBrewer)

working_path <- "./StockAssessment"

file_list <- list.files(path=working_path)

file_path <- file.path(working_path, file_list)

# Define key terms ---------------------------------------------------------------

key_terms <- c("biomass", 
               "spawning stock biomass", 
               "unfished",
               "recruitment",
               "catch",
               "landings",
               "catch per unite effort",
               "maximum sustainable yield",
               "instantaneous mortality rate",
               "instantaneous fishing mortality rate",
               "fmsy",
               "spawning per recruit",
               "weight",
               "length composition",
               "age composition",
               "forecast",
               "sex",
               "logr0",
               "plus group",
               "cohort",
               "metier",
               "fraction unfished",
               "fork length",
               "stock",
               "fishery",
               "fishing"
)


# Create empty database ----------------------------------------------------------------

database <- data.frame(matrix(NA, ncol=(2+length(key_terms)), nrow=length(file_list))) # column 1: ID; column 2: File_Name

colnames(database) <- c("ID", "File_Name", key_terms)

for (i in seq_along(file_list)){
  print(i)
  file <- file_path[i]
  
  # Create metadata of files
  docs <- tm::Corpus(URISource(file),
                 readerControl = list(reader = readPDF))
  
  # Convert symbols to space
  toSpace <- tm::content_transformer(
    function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm::tm_map(docs, toSpace, "/")
  docs <- tm::tm_map(docs, toSpace, "@")
  docs <- tm::tm_map(docs, toSpace, "\\|")
  
  # Convert text to lower case
  docs <- tm::tm_map(
    docs,
    content_transformer(tolower))
  
  # Remove punctuations
  docs <- tm::tm_map(docs, removePunctuation)
  
  # Eliminate extra white spaces
  docs <- tm::tm_map(docs, stripWhitespace)
  
  database$ID[i] <- i
  database$File_Name[i] <- file_list[i]
  
  for (j in seq_along(key_terms)){
    database[i,key_terms[j]] <- sum(stringr::str_count(docs[[1]]$content, key_terms[j]))
  }
}
write.csv(database, file.path(working_path, "database.csv"), row.names = F)


# Plot results ------------------------------------------------------------

word_frequency <- apply(database[3:ncol(database)], 2, sum)

barplot(height=word_frequency, names.arg = key_terms,
        ylab="Frequency", las=2)

wordcloud(words=key_terms, freq=word_frequency, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

