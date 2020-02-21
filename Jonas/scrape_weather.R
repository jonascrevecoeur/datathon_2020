################################################################################
## WOW.metoffice.gov.uk scrape
################################################################################


# set url
url <- "wow.metoffice.gov.uk"


# set working directory
dir <- "C:/Users/u0132167/Desktop/side projects/scrape23/"; setwd(dir)


# load libraries
library(dplyr)
library(jsonlite)
library(RSelenium)


# load weather json file
content <- readLines(paste0(dir, "weather.txt"))
res <- lapply(content, fromJSON)
coord_list <- res[[1]]$features$geometry$coordinates
id_list <- res[[1]]$features$properties$reportId


# set up rSeleniumDriver 
# redo: ctrl + shift + f10
chromever <- binman::list_versions("chromedriver")$win32[[1]]
rD <- rsDriver(port = 4567L, browser = "chrome", chromever = chromever,
               version = "latest", geckover = "latest", iedrver = NULL, 
               phantomver = "2.1.1", verbose = TRUE, check = TRUE)
remDr <- rD$client

#' scraping loop - 4235 weather stations
#' 
#' variables:
#' id_list -> website specific unique weather station location id
#' date -> randomly generated between 01/06/2019 and 31/01/2020 (+ 1 day 
#' seen as we focus on extracting the weather data for two adjacent days)


date <- c("01-01-2020", "02-01-2020") # example date set



# navigate to adress
remDr$navigate(paste0("https://", url, "/observations/details/", id_list[[1]]))

# switch to table tab
table_b <- remDr$findElement(using = "css selector", ".m-b-lg li:nth-child(2) a")
table_b$clickElement()

# locate & clear begindate
begin_date_f <- remDr$findElement(using = "xpath", '//*[@id="table-first-date"]//*[@name="Date"]')
begin_date_f$highlightElement()
begin_date_f$clearElement()

# locate & clear enddate
end_date_f <- remDr$findElement(using = "xpath", '//*[@id="table-last-date"]//*[@name="Date"]')
end_date_f$highlightElement()
end_date_f$clearElement()

# fill in dates
begin_date_f$sendKeysToElement(list(date[1]))
end_date_f$sendKeysToElement(list(date[2]))

# export table
export_b <- remDr$findElement(using = "css selector", '#tableViewExport')
export_b$clickElement()
