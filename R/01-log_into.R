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
    ## zamiana na character
  doctor_number <- doctors[tolower(name) %like% doctor & !(tolower(name) %like%   'telefoni')][order(nchar(name))][1]

  regions <- get_ids_list(field_name = "RegionId")
  region_number <- regions[tolower(name) %like% tolower(region) ][order(nchar(name))][1]
  
  
  new_url <- paste(sprintf("https://mol.medicover.pl/MyVisits?regionId=%s&bookingTypeId=2&specializationId=%s",
                           region_number$id,
                           doctor_number$id),
                   "&serviceId=&clinicId=-1&languageId=-1&doctorId=-1&searchSince=2017-07-14T08%3A00%3A00.000Z&periodOfTheDay=0&isSetBecauseOfPcc=false&isSetBecausePromoteSpecialization=false&selectedSpecialties=",
    sep = "")
  remDr$navigate(new_url)
  Sys.sleep(4)
  ### Click search button
  
  
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
                     name = unlist(doc["//option", fun = function(x) xmlValue(x)], F,F))
  
  return(data)
  
  
}
#' log_into_medicover
#'
#' This function allows you to log into medicover service
#' @param url Type polish name of doctor you are looking for
#' @param credentials_path path to file with user login and pass
#' @param submit_id id value for submit button in medicover dom
#' @export
#' @examples
#' log_into_medicover(credentials_path = "~/pass/medicover.txt")





log_into_medicover <- function(url = "https://mol.medicover.pl/MyVisits?regionId=204&bookingTypeId=2&specializationId=178&serviceId=&clinicId=-1&languageId=-1&doctorId=-1&searchSince=2017-07-14T08%3A00%3A00.000Z&periodOfTheDay=0&isSetBecauseOfPcc=false&isSetBecausePromoteSpecialization=false&selectedSpecialties=",
                               credentials_path = "~/pass/medicover.txt",
                               submit_id = '//*[@id="oidc-submit"]'){
  
  remDr$navigate(url)
  ### Tuta1j muszę dać coś na sprawdzenie czy jest odpalone logowanie
  element_check <- remDr$findElements(using='xpath', submit_id)
  
  if (length(element_check) != 0) {
    log_into <- remDr$findElement(using = "xpath",'//*[@id="oidc-submit"]')
    log_into$clickElement()
    windows_list <- remDr$getWindowHandles()
    remDr$closeWindow()
    remDr$switchToWindow(windows_list[[2]])
    ## Get login and pass
    login <- read_credntials(credentials_path = credentials_path)[[1]]
    password <- read_credntials(credentials_path = credentials_path)[[2]]
    ####
    
    
    card_number <- remDr$findElement(using = "xpath",'//*[@id="username-email"]')
    card_number$sendKeysToElement(list(login, key="enter"))
    pass <- remDr$findElement(using = "xpath",'//*[@id="password"]')
    pass$sendKeysToElement(list(password, key="enter"))
    Sys.sleep(5)
    remDr$navigate(url)
  } else {
    print("You are probably log in")
  }
}












