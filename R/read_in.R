#' read_in
#' Function to read in a .txt file and turn it into a data frame
#' @param filename name of the .txt file
#'
#' @return data frame of values
#' @export
#'
#' @examples
read_in <- function(filename = "top20.txt"){
  input_data<- readLines(filename, encoding="UTF-8")
  names_index <- seq(from = 1, to=140, by = 7)
  titles <- input_data[names_index]
  namedef <- unlist(strsplit(titles, split = ": "))
  namedef_df <- as.data.frame(matrix(namedef, ncol=2, byrow=TRUE))
  names(namedef_df) <- c("name","description")
  
  string_names <- c("Examples","ICES code type","Units","Rationale","Alternatives", "Range of possible values")
  for(i in 1:length(string_names)){
    ind <- grep(input_data,pattern=paste0(string_names[i],":"), ignore.case=T)
    strings <- gsub(pattern=paste0(string_names[i],":"), replacement="",x=input_data[ind], ignore.case=T)
    namedef_df[[string_names[i]]]  <- strings
  }
  
  return(namedef_df)

  }