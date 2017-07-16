#' scrap_doctor_rate function
#'
#' This function allows you to scrap doctors grade from znanylekarz based on given names
#' @param doctor_name Doctor name and surname scrapped from medicover website
#' @export
#' @examples
#' scrap_doctor_rate(doctor_name = "Kowalski Jan")




scrap_doctor_rate <- function(doctor_name){
  
  first_name <- gsub(".*\\s", "", doctor_name)
  last_name <- gsub("(.*)\\s.*", "\\1", doctor_name)
  full_name <- tolower(stringi::stri_trans_general(
    paste(first_name, last_name, sep = "-"),"Latin-ASCII" ))
  
  processed_name <- gsub("\\s", "", full_name)
  
  
  # processed_name <-  tolower(paste0(stringi::stri_trans_general(
  #   rev(c(unlist(strsplit(doctor_name, "\\s")))),"Latin-ASCII"), 
  #   collapse = "-"))
  adres <- sprintf("https://www.znanylekarz.pl/%s/", processed_name)
  znany_lekarz <- try(read_html(adres), silent = TRUE)
  if(length(znany_lekarz)==1){
    data <- data.table(doctor_name = doctor_name, rating = NA, grades_number = NA  )
  }
  else {
    rating <- znany_lekarz %>% 
      html_nodes("#profile-opinions div.panel-heading.opinion-score-summary > div > div.col-md-1 > a") %>%
      html_text() %>%
      as.numeric()
    grades_number <- znany_lekarz %>% 
      html_nodes("#profile-opinions > div.panel-heading.opinion-score-summary > div > div:nth-child(2) > div > p > small") %>%
      html_text() %>%
      parse_number() 
    data <- data.table(doctor_name = doctor_name, rating = rating, grades_number = grades_number  )
  }
  return(data)
}
