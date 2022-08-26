library(googledrive)
library(here)
library(pdftools)
library(tm)
library(stringr)
library(xlsx)
library(wordcloud)
library(RColorBrewer)

# Download stock assessment reports from Google Drive -------------

# googledrive set-up: https://github.com/nmfs-openscapes/GoogleDrive1
download_assessments <- function(googledrive_url, folder_name, subfolder_name) {

  # Get the files in the folder
  dir_files <- googledrive::drive_ls(path = googledrive_url)
  file_name <- dir_files$name

  # Create folders to save downloaded files
  if (!dir.exists(here::here(folder_name))) dir.create(here::here(folder_name))
  if (!dir.exists(here::here(folder_name, subfolder_name))) dir.create(here::here(folder_name, subfolder_name))

  # Download files from Google Drive
  for (i in 1:length(dir_files$id)) {
    googledrive::drive_download(file = dir_files$id[i], overwrite = TRUE, path = here::here(folder_name, subfolder_name, file_name[i]))
  }
}

# Download AFSC stock assessment reports
download_assessments(
  googledrive_url = "https://drive.google.com/drive/folders/1vbxiADIsjnCPGyOf-v4zTexOUSTidMTA",
  folder_name = file.path("StockAssessment"),
  subfolder_name = "AFSC"
)

# Download NEFSC stock assessment reports
download_assessments(
  googledrive_url = "https://drive.google.com/drive/folders/1edP73M7Bd4SpGLsgXIS8-jlqclA1QTZ9",
  folder_name = file.path("StockAssessment"),
  subfolder_name = "NEFSC"
)

# Download NWFSC stock assessment reports
download_assessments(
  googledrive_url = "https://drive.google.com/drive/folders/1UHv3f7JDkz3T6OUFNGiurBZIWR_EzyQq",
  folder_name = file.path("StockAssessment"),
  subfolder_name = "NWFSC"
)

# Download PIFSC stock assessment reports
download_assessments(
  googledrive_url = "https://drive.google.com/drive/folders/11I-3gwehVMEK_0aAkuytijNs7PXsu_e-",
  folder_name = file.path("StockAssessment"),
  subfolder_name = "PIFSC"
)

# Download SEFSC stock assessment reports
download_assessments(
  googledrive_url = "https://drive.google.com/drive/folders/12-09nqC7CvKfL2j1JX8ooi_bgMob2c1I",
  folder_name = file.path("StockAssessment"),
  subfolder_name = "SEFSC"
)

# Download SWFSC stock assessment reports
download_assessments(
  googledrive_url = "https://drive.google.com/drive/folders/1IqbncWlwbSwmBKroIfo-C9ckI_uGe6NG",
  folder_name = file.path("StockAssessment"),
  subfolder_name = "SWFSC"
)

# Define keyword ---------------------------------------------------------------

keyword <- c(
  "biomass|b|abundance",
  "spawning biomass|sb|spawning stock biomass|ssb|spawning stock output|sso|spawning output",
  "unfished|virgin|initial|equilibrium|unfished equilibrium",
  "recruits|age-0 fish",
  "catch|total mortality",
  "landings|retained catch|wanted catch",
  "catch per unit effort|cpue|catch rate|index of abundance|standardized fishery catch time series|catch per effort|fishing success|availability",
  "maximum sustainable yield|msy|msyproxy",
  "instantaneous mortality rate|z",
  "instantaneous fishing mortality rate|f|harvest rate|exploitation rate|finite fishing mortality|apical f",
  "fishing mortality at maximum sustainable yield|fproxy|f_msy|fmsy",
  "spawning per recruit|spr|spr30%|30%spr",
  "weight|kg|mass",
  "length composition|length frequency|length observation",
  "age composition|age frequency|age observation",
  "projection|forecast|prediction",
  "sex|gender",
  "natural log of unfished recruitment|logr0|lnr0|r0|log(r0)",
  "plus group",
  "cohort|recruit"
)

# Create keywords database ----------------------------------------------------------------

working_path <- here::here("StockAssessment")
subfolder_path <- list.dirs(path = working_path, full.names = TRUE, recursive = FALSE)
subfolder_name <- list.dirs(path = working_path, full.names = FALSE, recursive = FALSE)

# Create keyword database
col_name <- c("ID", "Science_Center", "File_Path", keyword)
frequency_database <- presence_database <- proportion_database <-
  data.frame(matrix(NA, ncol = length(col_name)))
colnames(frequency_database) <- colnames(presence_database) <- colnames(proportion_database) <-
  col_name

for (subfolder_id in seq_along(subfolder_path)) {
  file_path <- list.files(subfolder_path[subfolder_id], recursive = FALSE, full.names = TRUE)

  for (file_id in seq_along(file_path)) {
    file <- file_path[file_id]

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

    frequency <- presence <- proportion <- c()
    for (keyword_id in seq_along(keyword)) {
      # print(paste0("Folder: ", subfolder_id, "; File: ", file_id, "; Keyword: ", keyword_id))
      frequency[keyword_id] <- sum(stringr::str_count(docs[[1]]$content, paste("\\b", keyword[keyword_id], "\\b", sep = ""))) # Match if the current position is a word boundary
      presence[keyword_id] <- ifelse(frequency[keyword_id] > 0, 1, 0)
      proportion[keyword_id] <- round(frequency[keyword_id] / sum(stringr::str_count(docs[[1]]$content)) * 100, digits = 2)
    }

    if (subfolder_id == 1 & file_id == 1) {
      frequency_database[1, ] <- c(NA, subfolder_name[subfolder_id], file_path[file_id], frequency)

      presence_database[1, ] <- c(NA, subfolder_name[subfolder_id], file_path[file_id], presence)

      proportion_database[1, ] <- c(NA, subfolder_name[subfolder_id], file_path[file_id], proportion)
    } else {
      frequency_database <- rbind(
        frequency_database,
        c(NA, subfolder_name[subfolder_id], file_path[file_id], frequency)
      )

      presence_database <- rbind(
        presence_database,
        c(NA, subfolder_name[subfolder_id], file_path[file_id], presence)
      )

      proportion_database <- rbind(
        proportion_database,
        c(NA, subfolder_name[subfolder_id], file_path[file_id], proportion)
      )
    }
  }
}

frequency_database$ID <- 1:nrow(frequency_database)
presence_database$ID <- 1:nrow(presence_database)
proportion_database$ID <- 1:nrow(proportion_database)

frequency_database[, 4:ncol(frequency_database)] <- sapply(frequency_database[, 4:ncol(frequency_database)], as.numeric)

frequency_database <- rbind(
  frequency_database,
  c(NA, NA, NA, apply(frequency_database[, 4:ncol(frequency_database)], 2, sum))
)
frequency_database[nrow(frequency_database), "ID"] <- "Sum"

presence_database[, 4:ncol(presence_database)] <- sapply(presence_database[, 4:ncol(presence_database)], as.numeric)
presence_database <- rbind(
  presence_database,
  c(NA, NA, NA, apply(presence_database[, 4:ncol(presence_database)], 2, sum))
)
presence_database[nrow(presence_database), "ID"] <- "Sum"

proportion_database[, 4:ncol(proportion_database)] <- sapply(proportion_database[, 4:ncol(proportion_database)], as.numeric)
proportion_database <- rbind(
  proportion_database,
  c(NA, NA, NA, apply(proportion_database[, 4:ncol(proportion_database)], 2, sum))
)
proportion_database[nrow(proportion_database), "ID"] <- "Sum"

xlsx_path <- here::here("TextAnalysis", "top20_analysis.xlsx")
xlsx::write.xlsx(frequency_database, file = xlsx_path, sheetName = "frequency", row.names = FALSE)
xlsx::write.xlsx(presence_database, file = xlsx_path, sheetName = "presence", append = TRUE, row.names = FALSE)
xlsx::write.xlsx(proportion_database, file = xlsx_path, sheetName = "proportion", append = TRUE, row.names = FALSE)

# Upload xlsx to Google Drive
# Google Drive folder id
id_googledrive <- "1BUsYYd11lE2TECqHru5tX6LwkMniVnKv"
googledrive::drive_upload(media = xlsx_path, path = as_id(id_googledrive), overwrite = TRUE, type = "spreadsheet")

# Plot results ------------------------------------------------------------
xlsx_path <- here::here("TextAnalysis", "top20_analysis.xlsx")
frequency_database <- xlsx::read.xlsx(file = xlsx_path, sheetName = "frequency")
presence_database <- xlsx::read.xlsx(file = xlsx_path, sheetName = "presence")
proportion_database <- xlsx::read.xlsx(file = xlsx_path, sheetName = "proportion")

word_frequency <- as.matrix(frequency_database[frequency_database$ID == "Sum", 4:ncol(frequency_database)])
word_presence <- as.matrix(presence_database[presence_database$ID == "Sum", 4:ncol(presence_database)])
word_proportion <- as.matrix(proportion_database[proportion_database$ID == "Sum", 4:ncol(proportion_database)])

xlabels <- c(
  "biomass",
  "spawning biomass",
  "unfished",
  "recruitment",
  "catch",
  "landings",
  "catch per unit effort",
  "MSY",
  "Z",
  "F",
  "FMSY",
  "spawner per recruit",
  "weight",
  "length composition",
  "age composition",
  "projection",
  "sex",
  "natural log of unfished recruitment",
  "plus group",
  "cohort"
)

# barplot
jpeg(filename = here::here("TextAnalysis", "top20_barplot.jpg"), width = 200, height = 120, units = "mm", res = 1200)
par(mar = c(6, 4, 1, 1), mfrow = c(3, 1))
frequency_barplot <- barplot(height = word_frequency, xaxt = "n", ylab = "Frequency")
text(frequency_barplot - 0.25, par("usr")[3] - 0.25, xlabels, xpd = TRUE, srt = 40, adj = 1, cex = 0.8)


presence_barplot <- barplot(height = word_presence, xaxt = "n", ylab = "Presence")
text(presence_barplot - 0.25, par("usr")[3] - 0.25, xlabels, xpd = TRUE, srt = 40, adj = 1, cex = 0.8)

proportion_barplot <- barplot(height = word_proportion, xaxt = "n", ylab = "Proportion")
text(proportion_barplot - 0.25, par("usr")[3] - 0.25, xlabels, xpd = TRUE, srt = 40, adj = 1, cex = 0.8)

dev.off()

jpeg(filename = here::here("TextAnalysis", "top20_presence.jpg"), width = 200, height = 120, units = "mm", res = 1200)
par(mar = c(6, 4, 1, 1), mfrow = c(1, 1))
presence_barplot <- barplot(height = word_presence, xaxt = "n", ylab = "Frequency") # range: 26-84, median:67.5
text(presence_barplot - 0.25, par("usr")[3] - 0.25, xlabels, xpd = TRUE, srt = 40, adj = 1, cex = 0.7)
dev.off()

# wordcloud figure
jpeg(filename = here::here("TextAnalysis", "top20_wordcloud.jpg"), width = 200, height = 200, units = "mm", res = 1200)
par(mar = c(1, 1, 1, 1), mfrow = c(1, 1))
wordcloud(
  words = xlabels, freq = word_presence, rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)
dev.off()
