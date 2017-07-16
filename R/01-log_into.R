#' Dynamic url function
#'
#' This function allows you to create search url based on condition you add in arguments
#' @param doctor Type polish name of doctor you are looking for
#' @param region type city in which you want to find Your doctor
#' @export
#' @examples
#' dynamic_url(doctor = "endokrynolog")


dynamic_url <- function(doctor = "endokrynolog", region ="warszawa") {
  
  start_url <- "https://mol.medicover.pl/MyVisits?regionId=204&bookingTypeId=2&specializationId=178&serviceId=&clinicId=-1&languageId=-1&doctorId=-1&searchSince=2017-07-14T08%3A00%3A00.000Z&periodOfTheDay=0&isSetBecauseOfPcc=false&isSetBecausePromoteSpecialization=false&selectedSpecialties="
  remDr$navigate(start_url)
  Sys.sleep(5)
  doctors <- get_ids_list(field_name = "SpecializationId")
    
  doctor_number <- doctors[tolower(name) %like% doctor & !(tolower(name) %like%   'telefoni')][order(nchar(name))][1]

  regions <- get_ids_list(field_name = "RegionId")
  region_number <- regions[tolower(name) %like% region ][order(nchar(name))][1]
  
  
  
  
  new_url <- paste(sprintf("https://mol.medicover.pl/MyVisits?regionId=%s&bookingTypeId=2&specializationId=%s",
                           region_number$id,
                           doctor_number$id),
                   "&serviceId=&clinicId=-1&languageId=-1&doctorId=-1&searchSince=2017-07-14T08%3A00%3A00.000Z&periodOfTheDay=0&isSetBecauseOfPcc=false&isSetBecausePromoteSpecialization=false&selectedSpecialties=",
    sep = "")
  remDr$navigate(new_url)
  Sys.sleep(4)
  
  find <- remDr$findElement(using = "xpath", '//*[@id="collapse_3_1"]/div/div[4]/div/div/button')
  find$clickElement()
}

#' get_ids_list url function
#'
#' This function allows you to list field values with its names
#' @param field_name name of field that you want to listr
#' @export
#' @examples
#' get_ids_list(doctor = "endokrynolog")


get_ids_list <- function(field_name = "SpecializationId") {
  web_elem <- remDr$findElement(using = "id", value = field_name)
  
  
  webElem5txt <- web_elem$getElementAttribute("outerHTML")[[1]]
  
  doc <- htmlParse(webElem5txt, encoding = "UTF-8")
  
  data <- data.table(id = unlist(doc["//option", fun = function(x) xmlGetAttr(x, "value")],F,F),
                     name = doc["//option", fun = function(x) xmlValue(x)])
  
  return(data)
  
  
}
















