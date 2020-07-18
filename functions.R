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