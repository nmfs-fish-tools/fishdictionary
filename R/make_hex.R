#' Create a hexagonal sticker for the 'fishdictionary' package.
#'
#' @importFrom hexSticker sticker
#' @return The function does not return any value but creates a hexagonal sticker image.
make_hex <- function(){
  hexSticker::sticker(paste0("./static/pictures/", c("nounbook.png","nounfish.png")), 
                      package="fishdictionary", p_size=20, s_x=c(1,1), s_y=c(.4,.9), 
                      s_width=c(.6,.3),
                      filename="inst/figures/imgfile.png", h_color = "blue", p_color = "blue",
                      )
}