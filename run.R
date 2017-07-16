source("00-load_packages.R")

rD <- rsDriver(port = 4570L, browser = "chrome")
remDr <- rD[["client"]]
log_into_medicover()
Sys.sleep(5)
dynamic_url(doctor = "ortopeda")
Sys.sleep(15)

medicover_data <- get_reservation_data(sleep_time = 12, click_break = 5)


doctor_names <- unique(medicover_data$doctor_name)

data <- lapply(doctor_names, scrap_doctor_rate)
data <- rbindlist(data)
setkey(data, doctor_name)
setkey(medicover_data, doctor_name)
full_data <- data[medicover_data]

### znajdz medicover online

rD$server$stop()
