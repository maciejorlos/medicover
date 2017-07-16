#' Read_credentials function
#'
#' This function read credentials from txt file
#' @param credentials_path Address to txt file with login and password
#' @export
#' @examples
#' read_credntials()
read_credntials <- function(credentials_path = "~/pass/medicover.txt") {
  
  raw_file <- read.csv(file = credentials_path, header = F, sep = " ")
  login <- as.character(raw_file$V1)
  pass <- as.character(raw_file$V2)

  return(list(login, pass))
    
}

