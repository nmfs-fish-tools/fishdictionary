library(googledrive)
library(here)
library(pdftools)
library(tm)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(jpeg)
library(grid)
library(gridExtra)

# Download stock assessment reports from Google Drive -------------

## If authorize_GoogleDrive <- TRUE:
## Users need to authorize googledrive package to access your Google Drive
## Please follow the instructions from https://googledrive.tidyverse.org/reference/drive_auth.html and https://github.com/nmfs-openscapes/GoogleDrive1 to
## authorize googledrive

## If authorize_GoogleDrive <- FALSE:
## Users need to manually download stock assessment reports from the
## Assessment Docs Google Drive folder (https://drive.google.com/drive/folders/1uyLVjf7Xu2iMY6dhEjAAkB4l2vtSDMw9?usp=share_link) to fishdictionary/StockAssessment folder
## The folder structure looks

authorize_GoogleDrive <- FALSE
if (authorize_GoogleDrive) {
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
}

# run_text_mining function ------------------------------------------------

run_text_mining <- function(keyword, xlabels, working_path, 
                            subfolder_path_all, subfolder_name_all, 
                            subfolder_info, scenario_name) {
  wordcloud_data <- wordcloud_data_mean <-vector(mode="list", length=length(subfolder_info))
  for (org in seq_along(subfolder_info)){
    subfolder_path <- subfolder_path_all[subfolder_info[[org]]]
    subfolder_name <- subfolder_name_all[subfolder_info[[org]]]
    
    # Create keyword database
    col_name <- c("ID", "Region", "File_Path", keyword)
    frequency_database <- 
      presence_database <- 
      proportion_database <-
      data.frame(matrix(NA, ncol = length(col_name)))
    
    colnames(frequency_database) <- 
      colnames(presence_database) <- 
      colnames(proportion_database) <-
      col_name
    
    for (subfolder_id in seq_along(subfolder_path)) {
      
      file_path <- list.files(subfolder_path[subfolder_id], recursive = FALSE, full.names = TRUE)
      
      for (file_id in seq_along(file_path)) {
        
        # cat("file:", file_path[file_id], "\n")
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
    
    write.csv(frequency_database, file=here::here("TextAnalysis", paste0("top19_frequency_", names(subfolder_info)[org], "_", scenario_name, ".csv")), row.names=FALSE)
    write.csv(presence_database, file=here::here("TextAnalysis", paste0("top19_presence_", names(subfolder_info)[org], "_", scenario_name, ".csv")), row.names=FALSE)
    write.csv(proportion_database, file=here::here("TextAnalysis", paste0("top19_proportion_", names(subfolder_info)[org], "_", scenario_name, ".csv")), row.names=FALSE)
    # Upload xlsx to Google Drive
    if (authorize_GoogleDrive) {
      # Google Drive folder id
      id_googledrive <- "1BUsYYd11lE2TECqHru5tX6LwkMniVnKv"
      googledrive::drive_upload(media = xlsx_path, path = as_id(id_googledrive), overwrite = TRUE, type = "spreadsheet")
    }
    
    # Plot results ------------------------------------------------------------
    frequency_database <- read.csv(file=here::here("TextAnalysis", paste0("top19_frequency_", names(subfolder_info)[org], "_", scenario_name, ".csv")))
    presence_database<-read.csv(file=here::here("TextAnalysis", paste0("top19_presence_", names(subfolder_info)[org], "_", scenario_name, ".csv")))
    proportion_database<-read.csv(file=here::here("TextAnalysis", paste0("top19_proportion_", names(subfolder_info)[org], "_", scenario_name, ".csv")))
    
    word_frequency <- as.matrix(frequency_database[frequency_database$ID == "Sum", 4:ncol(frequency_database)])
    word_presence <- as.matrix(presence_database[presence_database$ID == "Sum", 4:ncol(presence_database)])
    word_presence_mean <- as.matrix(presence_database[presence_database$ID == "Sum", 4:ncol(presence_database)])/(nrow(presence_database)-1)*100
    word_proportion <- as.matrix(proportion_database[proportion_database$ID == "Sum", 4:ncol(proportion_database)])
    
    # barplot
    jpeg(filename = here::here("TextAnalysis", paste0("top19_barplot_", names(subfolder_info)[org], "_", scenario_name, ".jpg")), width = 200, height = 120, units = "mm", res = 1200)
    par(mar = c(6, 4, 1, 1), mfrow = c(3, 1))
    frequency_barplot <- barplot(height = word_frequency, xaxt = "n", ylab = "Frequency")
    text(frequency_barplot - 0.25, par("usr")[3] - 0.25, xlabels, xpd = TRUE, srt = 40, adj = 1, cex = 0.8)
    
    
    presence_barplot <- barplot(height = word_presence, xaxt = "n", ylab = "Presence")
    text(presence_barplot - 0.25, par("usr")[3] - 0.25, xlabels, xpd = TRUE, srt = 40, adj = 1, cex = 0.8)
    
    proportion_barplot <- barplot(height = word_proportion, xaxt = "n", ylab = "Proportion")
    text(proportion_barplot - 0.25, par("usr")[3] - 0.25, xlabels, xpd = TRUE, srt = 40, adj = 1, cex = 0.8)
    
    dev.off()
    
    jpeg(filename = here::here("TextAnalysis", paste0("top19_presence_", names(subfolder_info)[org], "_", scenario_name, ".jpg")), width = 200, height = 120, units = "mm", res = 1200)
    par(mar = c(6, 4, 1, 1), mfrow = c(1, 1))
    presence_barplot <- barplot(height = word_presence, xaxt = "n", ylab = "Presence (%)") 
    text(presence_barplot - 0.25, par("usr")[3] - 0.25, xlabels, xpd = TRUE, srt = 40, adj = 1, cex = 0.7)
    dev.off()
    
    jpeg(filename = here::here("TextAnalysis", paste0("top19_presence_mean_", names(subfolder_info)[org], "_", scenario_name, ".jpg")), width = 200, height = 120, units = "mm", res = 1200)
    par(mar = c(6, 4, 1, 1), mfrow = c(1, 1))
    presence_barplot <- barplot(height = word_presence_mean, xaxt = "n", ylab = "Presence (%)") 
    text(presence_barplot - 0.25, par("usr")[3] - 0.25, xlabels, xpd = TRUE, srt = 40, adj = 1, cex = 0.7)
    dev.off()
    
    # wordcloud figure
    jpeg(filename = here::here("TextAnalysis", paste0("top19_wordcloud_", names(subfolder_info)[org], "_mean_presence_grays", "_", scenario_name, ".jpg")), width = 200, height = 200, units = "mm", res = 1200)
    par(mar = c(0.1, 0.1, 0.1, 0.1), mfrow = c(1, 1))
    set.seed(123)
    wordcloud(
      words = xlabels, freq = word_presence_mean, rot.per = 0,
      scale = c(1.5, 0.5),
      min.freq = 1,
      random.order = F,  
      colors = grDevices::gray.colors(10, rev = TRUE), 
      use.r.layout = T
    )
    text(0.1, 0.8, scenario_name)
    dev.off()
    
    jpeg(filename = here::here("TextAnalysis", paste0("top19_wordcloud_", names(subfolder_info)[org], "_mean_presence_black", "_", scenario_name, ".jpg")), width = 200, height = 200, units = "mm", res = 1200)
    par(mar = c(0.1, 0.1, 0.1, 0.1), mfrow = c(1, 1))
    set.seed(123)
    wordcloud(
      words = xlabels, freq = word_presence_mean, rot.per = 0,
      scale = c(1.5, 0.5), 
      min.freq = 1,
      random.order = F,  
      use.r.layout = T
    )
    text(0.1, 0.8, scenario_name)
    dev.off()
  }
}

# Define keyword ---------------------------------------------------------------
working_path <- here::here("StockAssessment")
subfolder_path_all <- list.dirs(path = working_path, full.names = TRUE, recursive = FALSE)
subfolder_name_all <- list.dirs(path = working_path, full.names = FALSE, recursive = FALSE)

subfolder_info <- list(
  grep("Australia", subfolder_name_all),
  grep("ICES", subfolder_name_all),
  grep("NOAA", subfolder_name_all), 
  1:length(subfolder_name_all)
)

names(subfolder_info) <- c("Australia", "ICES", "NOAA", "All")

keyword <- c(
  "biomass|b", "abundance",
  "spawning biomass|sb", "spawning stock biomass|ssb", "mature biomass", "spawners", "effective spawning output",
  "unfished", "virgin", "initial equilibrium", "unfished equilibrium",
  "recruitment",
  "recruits", "recruit", "age 0 fish", "age 1 fish",
  "catch", "harvest", "total removals",
  "catch per unit effort|cpue", "catch rate", "index of abundance", "catch per effort", "fishing success",
  "landings", "retained catch",
  "spawner per recruit|spr", "spawning potential ratio",
  "maximum sustainable yield|msy",
  "instantaneous total mortality rate|z",
  "fishing mortality|f", "instantaneous fishing mortality rate", "harvest rate", "exploitation rate", "finite fishing mortality", "apical f",
  "fishing mortality at maximum sustainable yield|fmsy",
  "mass",
  "length composition", "length frequency", "length observation", "size frequency", "size composition",
  "age composition", "age frequency", "age observation",
  "projection",
  "sex", "gender",
  "plus group"
)

xlabels <- c(
  "Biomass", "Abundance",
  "Spawning biomass", "Spawning stock biomass", "Mature biomass", "Spawners", "Effective spawning output",
  "Unfished", "Virgin", "Initial equilibrium", "Unfished equilibrium",
  "Recruitment",
  "Recruits", "Recruit", "Age 0 fish", "Age 1 fish",
  "Catch", "Harvest", "Total removals",
  "Catch per unit effort", "Catch rate", "Index of abundance", "Catch per effort", "Fishing success",
  "Landings", "Retained catch",
  "Spawner per recruit", "Spawning potential ratio",
  "Maximum sustainable yield",
  "Instantaneous total mortality rate",
  "Fishing mortality", "Instantaneous fishing mortality rate", "Harvest rate", "Exploitation rate", "Finite fishing mortality", "Apical f",
  "Fishing mortality at maximum sustainable yield",
  "Mass",  
  "Length composition", "Length frequency", "Length observation", "Size frequency", "Size composition",
  "Age composition", "Age frequency", "Age observation",
  "Projection", 
  "Sex", "Gender",
  "Plus group"
)

run_text_mining(keyword = keyword, xlabels = xlabels, working_path = working_path, 
                subfolder_path_all = subfolder_path_all, subfolder_name_all = subfolder_name_all, 
                subfolder_info = subfolder_info, scenario_name = "A")

keyword <- c(
  "biomass|b|abundance",
  "spawning biomass|sb|spawning stock biomass|ssb|mature biomass|spawners|effective spawning output",
  "unfished|virgin|initial equilibrium|unfished equilibrium",
  "recruitment",
  "recruits|recruit|age 0 fish|age 1 fish",
  "catch|harvest|total removals",
  "catch per unit effort|cpue|catch rate|index of abundance|catch per effort|fishing success",
  "landings|retained catch",
  "spawner per recruit|spawning potential ratio|spr",
  "maximum sustainable yield|msy",
  "instantaneous total mortality rate|z",
  "fishing mortality|f|instantaneous fishing mortality rate|harvest rate|exploitation rate|finite fishing mortality|apical f",
  "fishing mortality at maximum sustainable yield|fmsy",
  "mass",
  "length composition|length frequency|length observation|size frequency|size composition",
  "age composition|age frequency|age observation",
  "projection",
  "sex|gender",
  "plus group"
)

xlabels <- c(
  "Biomass",
  "Spawning biomass",
  "Unfished",
  "Recruitment",
  "Recruit(s)",
  "Catch",
  "Catch per unit effort",
  "Landings",
  "Spawner per recruit",
  "Maximum sustainable yield",
  "Instantaneous total mortality rate",
  "Fishing mortality",
  "Fishing mortality at maximum sustainable yield",
  "Mass",
  "Length composition",
  "Age composition",
  "Projection",
  "Sex",
  "Plus group"
)

run_text_mining(keyword = keyword, xlabels = xlabels, working_path = working_path, 
                subfolder_path_all = subfolder_path_all, subfolder_name_all = subfolder_name_all, 
                subfolder_info = subfolder_info, scenario_name = "B")


jpeg(filename = here::here("TextAnalysis", "top19_wordcloud_All_mean_presence_grays_AB.jpg"), 
     width = 200, height = 120, units = "mm", res = 1200)
before_img <-  grid::rasterGrob(as.raster(jpeg::readJPEG(here::here("TextAnalysis", 
                                                              "top19_wordcloud_All_mean_presence_grays_A.jpg"))), 
                          interpolate = FALSE)
after_img <-  grid::rasterGrob(as.raster(jpeg::readJPEG(here::here("TextAnalysis", 
                                                             "top19_wordcloud_All_mean_presence_grays_B.jpg"))), 
                         interpolate = FALSE)
gridExtra::grid.arrange(before_img, after_img, ncol = 2)
dev.off()

jpeg(filename = here::here("TextAnalysis", "top19_wordcloud_All_mean_presence_black_AB.jpg"), 
     width = 200, height = 120, units = "mm", res = 1200)
before_img <-  grid::rasterGrob(as.raster(jpeg::readJPEG(here::here("TextAnalysis", 
                                                              "top19_wordcloud_All_mean_presence_black_A.jpg"))), 
                          interpolate = FALSE)
after_img <-  grid::rasterGrob(as.raster(jpeg::readJPEG(here::here("TextAnalysis", 
                                                             "top19_wordcloud_All_mean_presence_black_B.jpg"))), 
                         interpolate = FALSE)
gridExtra::grid.arrange(before_img, after_img, ncol = 2)
dev.off()
