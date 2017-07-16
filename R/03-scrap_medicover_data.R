#' get_head function
#'
#' This function get doctors time availability data
#' @param box Elemnt containing head with time data
#' @export
#' @examples
#' get_head()

get_head <- function(box = box_head) {
  box_to_html <- box$getElementAttribute("outerHTML")[[1]]
  box_html <- rvest::minimal_html(box_to_html)
  raw_text <- rvest::html_text(box_html)
  raw_text <- gsub(" ", "", raw_text)
  raw_text <- gsub("\n"," ", raw_text)
  raw_text <- gsub("^ | $", "", raw_text) ### usuwam z konca i początku
  date_box <- as.character(as.POSIXct(strptime(raw_text, "%d/%m/%Y %H:%M"), tz="Warsaw"))
  
  return(date_box)
}



#' get_content function
#'
#' This function get doctor name and location
#' @param content Elemnt containing box with doctor data
#' @export
#' @examples
#' get_content()

get_content <- function(content = box_content) {
  content_to_html <- content$getElementAttribute("outerHTML")[[1]]
  content_html <- rvest::minimal_html(content_to_html)
  raw_text <- rvest::html_text(content_html)
  raw_text <- gsub("\n", ";", raw_text)
  raw_text <- gsub("\\s+{2,200}", "", raw_text)
  raw_text <- gsub("^;|;$", "", raw_text) ### usuwam z konca i początku
  split_data <- unlist(strsplit(raw_text, split = ";"))
  
  return(split_data)
}




#' get_reservation_data function
#'
#' This function scrap doctor availiability data from medicover service
#' @param sleep_time number of seconds to wait untill medicover website get more doctors details 
#' @param click_break number of seconds to wait untill more elemnt is getting clicked 
#' @export
#' @examples
#' get_reservation_data(sleep_time = 8, click_break = 2 )


get_reservation_data <- function(sleep_time = 8, click_break = 2 ){
  ### Pokaż więcej do momentu ile się da
  show_more <-   remDr$findElement(using = "xpath", '//*[@id="freeSlotsResult"]/div[3]/div[2]/div/button')
  while(unlist(show_more$isElementDisplayed()) == TRUE ){
    show_more <-   remDr$findElement(using = "xpath", '//*[@id="freeSlotsResult"]/div[3]/div[2]/div/button')
    Sys.sleep(click_break)
    show_more$clickElement()
    Sys.sleep(sleep_time)
   print(sleep_time)
    }
  
  box_content <- remDr$findElements(using = "class", 'freeSlot-content')
  box_head <- remDr$findElements(using = "class", 'freeSlot-head')
  content <- sapply(box_content, get_content)
  date_time <- sapply(box_head, get_head)
  
  reservation_data <- data.table(t(content),time = date_time, number = 1:length(date_time))
  setnames(reservation_data,c("V1", "V2", "V3"), c("doctor_type","doctor_name", "location"))
  return(reservation_data)
}






