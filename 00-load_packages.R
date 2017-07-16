# install.packages("RSelenium")

## Instalka pakietu który instaluje inne pakiety jeżeli nie istnieją lub je ładuje
if (!require("pacman")) install.packages("pacman")

pacman::p_load(RSelenium)
pacman::p_load(data.table)
pacman::p_load(XML)
pacman::p_load(rvest)
pacman::p_load(readr)

