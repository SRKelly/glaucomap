#Functions used in the glaucoma_ses work

create_hover_text <- function(area, num_vfs, info_string, info_value){
  return(pmap(list(area, num_vfs, info_string, info_value), ~htmltools::HTML(sprintf("Area Name: %s <br/> Number of VFs: %s <br/> %s %s", ..1, ..2, ..3, ..4))))
}

style_sheet <-list(
  "color" = "black",
  "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
  "font-size" = "12px",
  "border-color" = "rgba(0,0,0,0.5)"
)


point_in_df <- function(x, y, df){
  length_df <- length(df@polygons)
  res <- numeric(length = length_df)
  
  for(i in 1:length_df){
    
    poly_cords <- df@polygons[[i]]@Polygons[[1]]@coords
    res[i] <- point.in.polygon(x, y, poly_cords[,1], poly_cords[,2])
  }
  return(df$msoa11cd[which(res == 1)])
}


