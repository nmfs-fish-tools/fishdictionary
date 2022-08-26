library(googledrive)
library(here)
library(pdftools)
library(tm)
library(stringr)
library(xlsx)
library(ggplot2)
library(patchwork) #devtools::install_github("thomasp85/patchwork")
library(wordcloud)
library(RColorBrewer)

# Define keyword ---------------------------------------------------------------

z <- data.frame(
  keyword = c(
    "instantaneous mortality rate|z"
  ),
  group = "instantaneous mortality rate"
)

spawning_biomass <- data.frame(
  keyword = c(
    "spawning biomass|sb",
    "spawning stock biomass|ssb", 
    "spawning output",
    "spawning stock output|sso"
  ),
  group = "spawning biomass"
)

cpue <- data.frame(
  keyword = c(
    "catch per unit effort|cpue",
    "catch rate", 
    "index of abundance",
    "standardized fishery catch time series",
    "catch per effort",
    "fishing success",
    "availability"
  ),
  group = "catch per unit effort"
)

catch <- data.frame(
  keyword = c(
    "catch",
    "total mortality"
  ),
  group = "catch"
)

landings <- data.frame(
  keyword = c(
    "landings",
    "retained catch",
    "wanted catch"
  ),
  group = "landings"
)

projection <- data.frame(
  keyword = c(
    "projection",
    "forecast",
    "prediction"
  ),
  group = "projection"
)

sex <- data.frame(
  keyword = c(
    "sex",
    "gender"
  ),
  group = "sex"
)

spr <- data.frame(
  keyword = c(
    "spawner per recruit|spr",
    "30%spr|spr30%|30spr|spr30"
  ),
  group = "spawner per recruit"
)

weight <- data.frame(
  keyword = c(
    "weight",
    "mass"
  ),
  group = "weight"
)

lnr0 <- data.frame(
  keyword = c(
    "natural log of unfished recruitment|lnr0",
    "logr0",
    "logr_0",
    "log(r0)",
    "r0"
  ),
  group = "natural log of unfished recruitment"
)

keyword_map <- rbind(z, spawning_biomass, cpue, catch, landings,
                 projection, sex, spr, weight, lnr0)
keyword_map$keyword_id <- as.factor(1:nrow(keyword_map))
keyword <- keyword_map$keyword

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

for (subfolder_id in seq_along(subfolder_path)){
  
  file_path <- list.files(subfolder_path[subfolder_id], recursive = FALSE, full.names = TRUE)
  
  for(file_id in seq_along(file_path)){
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
      #print(paste0("Folder: ", subfolder_id, "; File: ", file_id, "; Keyword: ", keyword_id))
      frequency[keyword_id] <- sum(stringr::str_count(docs[[1]]$content, paste("\\b", keyword[keyword_id], "\\b", sep = ""))) # Match if the current position is a word boundary
      presence[keyword_id] <- ifelse(frequency[keyword_id] > 0, 1, 0)
      proportion[keyword_id] <- round(frequency[keyword_id]/sum(stringr::str_count(docs[[1]]$content))*100, digits = 2)
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

xlsx_path <- here::here("TextAnalysis", "top10_analysis.xlsx")
xlsx::write.xlsx(frequency_database, file=xlsx_path, sheetName="frequency", row.names=FALSE)
xlsx::write.xlsx(presence_database, file=xlsx_path, sheetName="presence", append=TRUE, row.names=FALSE)
xlsx::write.xlsx(proportion_database, file=xlsx_path, sheetName="proportion", append=TRUE, row.names=FALSE)

# Upload xlsx to Google Drive
# Google Drive folder id
id_googledrive <- "1BUsYYd11lE2TECqHru5tX6LwkMniVnKv"
googledrive::drive_upload(media = xlsx_path, path = as_id(id_googledrive), overwrite = TRUE, type="spreadsheet")

# Plot results ------------------------------------------------------------
xlsx_path <- here::here("TextAnalysis", "top10_analysis.xlsx")
frequency_database <- xlsx::read.xlsx(file=xlsx_path, sheetName="frequency")
presence_database<-xlsx::read.xlsx(file=xlsx_path, sheetName="presence")
proportion_database<-xlsx::read.xlsx(file=xlsx_path, sheetName="proportion")

colnames(presence_database) <- col_name
data_reshape <- reshape2::melt(
  presence_database[, c(2, 4:ncol(presence_database))],
  id = c("Science_Center")
)
colnames(data_reshape) <- c("Science_Center", "keyword", "value")

data_merge <- merge(data_reshape, keyword_map, by= "keyword")

sum_by_group <- aggregate(value ~ keyword+keyword_id+group+Science_Center, data = data_merge, sum)
sum_by_group <- sum_by_group[order(sum_by_group$keyword_id),]
group <- unique(sum_by_group$group)
figure <- vector(mode="list", length=length(group))

for (i in 1:length(group)){
  figure[[i]] <- ggplot(data = sum_by_group[sum_by_group$group==group[i], ], aes(x=keyword_id, y=value, fill=Science_Center)) +
    geom_bar(position='dodge', stat='identity') +
    labs(
      x = group[i],
      y = "Frequency"
    )+
    theme_bw()
}

jpeg(filename = here::here("TextAnalysis", "top10_barplot.jpg"), width=300, height=270, units="mm", res=1200)
(figure[[1]] + figure[[2]] + figure[[3]] + figure[[4]] + figure[[5]])/
  (figure[[6]] + figure[[7]] + figure[[8]] + figure[[9]] + figure[[10]])
dev.off()
