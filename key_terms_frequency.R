
library(pdftools)
library(tm)
library(stringr)
library(wordcloud)
library(RColorBrewer)

working_path <- "C:/Users/bai.li/Documents/GitHub/data_dictionary/StockAssessment"

file_list <- list.files(path = working_path, pattern = "pdf$")

file_path <- file.path(working_path, file_list)

# Define key terms ---------------------------------------------------------------

key_terms <- c(
  "biomass|b",
  "spawning stock biomass|ssb|spawning stock output",
  "unfished|virgin|equilibrium",
  "recruitment|age-0 fish",
  "catch",
  "landings|retained catch",
  "catch per unit effort|cpue|catch rate|index of abundance",
  "maximum sustainable yield|msy|msyproxy",
  "instantaneous mortality rate|z",
  "instantaneous fishing mortality rate|f|harvest rate|exploitation rate",
  "fmsy|fproxy",
  "spawning per recruit|spr|spr30%",
  "weight|kg|mass",
  "length composition|length frequency",
  "age composition|age frequency",
  "forecast|projection",
  "sex|gender",
  "logr0|lnr0|r0|log(r0)",
  "plus group",
  "cohort|recruit",
  "metier|fleet",
  "fraction unfished|depletion|relative spawning biomass",
  "fork length|FL",
  "stock|population",
  "fishery",
  "fishing"
)

xlabels <- c(
  "biomass",
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


# key_terms <- c("unfished", "virgin", "equilibrium")
# key_terms <- c("landings", "retained catch")
# 
# xlabels <- key_terms

# Create empty database ----------------------------------------------------------------

database <- data.frame(matrix(NA, ncol = (2 + length(key_terms)), nrow = length(file_list))) # column 1: ID; column 2: File_Name

colnames(database) <- c("ID", "File_Name", key_terms)

for (i in seq_along(file_list)) {
  print(i)
  file <- file_path[i]

  # Create metadata of files
  docs <- tm::Corpus(URISource(file),
    readerControl = list(reader = readPDF)
  )

  # Convert symbols to space
  toSpace <- tm::content_transformer(
    function(x, pattern) gsub(pattern, " ", x)
  )
  docs <- tm::tm_map(docs, toSpace, "/")
  docs <- tm::tm_map(docs, toSpace, "@")
  docs <- tm::tm_map(docs, toSpace, "\\|")

  # Convert text to lower case
  docs <- tm::tm_map(
    docs,
    content_transformer(tolower)
  )

  # Remove punctuations
  docs <- tm::tm_map(docs, removePunctuation)

  # Eliminate extra white spaces
  docs <- tm::tm_map(docs, stripWhitespace)

  database$ID[i] <- i
  database$File_Name[i] <- file_list[i]

  for (j in seq_along(key_terms)) {
    database[i, key_terms[j]] <- sum(stringr::str_count(docs[[1]]$content, paste("\\b", key_terms[j], "\\b", sep = ""))) # Match if the current position is a word boundary
  }
}
write.csv(database, file.path(working_path, "key_terms_frequency.csv"), row.names = F)


# Plot results ------------------------------------------------------------

word_frequency <- apply(database[3:ncol(database)], 2, sum)

par(mar = c(6, 4, 1, 1))
terms_barplot <- barplot(height = word_frequency, xaxt = "n", ylab = "Frequency")
text(terms_barplot - 0.25, par("usr")[3] - 0.25, xlabels, xpd = TRUE, srt = 40, adj = 1, cex = 0.6)

par(mar = c(1, 1, 1, 1))
wordcloud(
  words = xlabels, freq = word_frequency, random.order = FALSE, rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)

