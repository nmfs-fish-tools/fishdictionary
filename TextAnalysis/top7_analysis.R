library(googledrive)
library(here)
library(pdftools)
library(tm)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(scales)
library(viridis)

# Define keyword ---------------------------------------------------------------

# z <- data.frame(
#   keyword = c(
#     "instantaneous total mortality rate|z"
#   ),
#   group = "instantaneous mortality rate"
# )

spawning_biomass <- data.frame(
  keyword = c(
    "spawning biomass|sb",
    "spawning stock biomass|ssb", 
    "spawning output",
    "spawning stock output|sso|mature biomass|spawners|effective spawning output"
  ),
  group = "spawning biomass"
)

cpue <- data.frame(
  keyword = c(
    "catch per unit effort|cpue",
    "catch rate", 
    "index of abundance",
    "catch per effort|fishing success"
  ),
  group = "catch per unit effort"
)

catch <- data.frame(
  keyword = c(
    "catch",
    "total mortality", 
    "harvest", 
    "total removals"
  ),
  group = "catch"
)

landings <- data.frame(
  keyword = c(
    "landings",
    "retained catch"
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

# spr <- data.frame(
#   keyword = c(
#     "spawner per recruit|spr",
#     "spawning potential ratio"
#   ),
#   group = "spawner per recruit"
# )

mass <- data.frame(
  keyword = c(
    "mass",
    "weight"
  ),
  group = "mass"
)

keyword_map <- rbind(catch, cpue, landings,
                     projection, sex, spawning_biomass, mass)
keyword_map$keyword_id <- as.factor(1:nrow(keyword_map))
keyword <- keyword_map$keyword

# Create keywords database ----------------------------------------------------------------

working_path <- here::here("StockAssessment")
subfolder_path <- list.dirs(path = working_path, full.names = TRUE, recursive = FALSE)
subfolder_name <- list.dirs(path = working_path, full.names = FALSE, recursive = FALSE)

# Create keyword database
col_name <- c("ID", "Region", "File_Path", keyword)
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

write.csv(frequency_database, file=here::here("TextAnalysis", "top7_frequency.csv"), row.names=FALSE)
write.csv(presence_database, file=here::here("TextAnalysis", "top7_presence.csv"), row.names=FALSE)
write.csv(proportion_database, file=here::here("TextAnalysis", "top7_proportion.csv"), row.names=FALSE)

# Upload xlsx to Google Drive
authorize_GoogleDrive <- FALSE
if (authorize_GoogleDrive) {
  # Google Drive folder id
  id_googledrive <- "1BUsYYd11lE2TECqHru5tX6LwkMniVnKv"
  googledrive::drive_upload(media = xlsx_path, path = as_id(id_googledrive), overwrite = TRUE, type="spreadsheet")
}

# Plot results ------------------------------------------------------------
frequency_database <- read.csv(file=here::here("TextAnalysis", "top7_frequency.csv"))
presence_database<-read.csv(file=here::here("TextAnalysis", "top7_presence.csv"))
proportion_database<-read.csv(file=here::here("TextAnalysis", "top7_proportion.csv"))

colnames(presence_database) <- col_name
data_reshape <- reshape2::melt(
  presence_database[, c(2, 4:ncol(presence_database))],
  id = c("Region")
)
colnames(data_reshape) <- c("Region", "keyword", "value")

data_merge <- merge(data_reshape, keyword_map, by= "keyword")

sum_by_group <- aggregate(value ~ keyword+keyword_id+group+Region, data = data_merge, sum)
sum_by_group <- sum_by_group[order(sum_by_group$keyword_id),]
group <- unique(sum_by_group$group)
sum_by_group$group <- factor(sum_by_group$group, levels = group)
jpeg(filename = here::here("TextAnalysis", "top7_barplot_sum.jpg"), width=200, height=300, units="mm", res=1200)
ggplot(sum_by_group, aes(fill=Region, y=value, x=keyword_id)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~group, scales = "free_x", ncol = 1) +
  labs(
    x = "Term ID",
    y = "Frequency"
  ) +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    panel.background = NULL,
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_blank()) 
# ggsave(here::here("TextAnalysis", "top7_barplot_sum.jpg"))
dev.off()

mean_by_group <- aggregate(value ~ keyword+keyword_id+group+Region, data = data_merge, mean)
mean_by_group <- mean_by_group[order(mean_by_group$keyword_id),]
group <- unique(mean_by_group$group)
mean_by_group$group <- factor(sum_by_group$group, levels = group)
jpeg(filename = here::here("TextAnalysis", "top7_barplot_mean.jpg"), width=200, height=300, units="mm", res=1200)
ggplot(mean_by_group, aes(fill=Region, y=value*100, x=keyword_id)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~group, scales = "free_x", ncol = 1) +
  labs(
    x = "Term ID",
    y = "Presence (%)"
  ) +
  scale_fill_viridis(discrete = TRUE) +
  geom_vline(xintercept = c(0:5)+0.5) +
  scale_y_continuous(expand=expansion(mult=c(0.0,0.0)))+
  scale_x_discrete(expand=expansion(mult=c(0.0,0.0))) +
  theme_bw()
# ggsave(here::here("TextAnalysis", "top7_barplot_mean.jpg"))
dev.off()
