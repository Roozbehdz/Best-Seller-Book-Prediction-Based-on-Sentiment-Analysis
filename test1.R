# libraries
library(lubridate)
library(stringr)
library(dplyr)

# getting list of sundays

sundays_list<- function(){
  today <- Sys.Date()+7
  sundays = today
  nytimes_start_rank = as.Date("2011-02-13")
  repeat{
    previous_sunday <- floor_date(today, "week")
    sundays = c(sundays,previous_sunday)
    if(previous_sunday == nytimes_start_rank){
      sundays = sundays[-1]
      break()}
    today = previous_sunday -1
  }
  return(sundays)
}

scrape_nytimes <- function(url, throttle = 0){
  
  
  # Install / Load relevant packages
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load(RCurl, XML, dplyr, stringr, rvest, purrr)
  
  # Set throttle between URL calls
  sec = 0
  if(throttle < 0) warning("throttle was less than 0: set to 0")
  if(throttle > 0) sec = max(0, throttle + runif(1, -1, 1))
  
  # obtain HTML of URL
  doc <- xml2::read_html(url)
  
  # Parse relevant elements from HTML
  Title <- doc %>%
    html_nodes(".css-5pe77f") %>%
    html_text()
  
  Author <- doc %>%
    html_nodes(".css-hjukut") %>%
    html_text()

  Publisher <- doc %>%
    html_nodes(".css-heg334") %>%
    html_text()
  # 
  # # Description <- doc %>%
  # #   html_nodes(".css-14lubdp") %>%
  # #   html_text()

  Weeksonthelist <- doc %>%
    html_nodes(".css-1o26r9v") %>%
    html_text()

  Weekdate <- doc %>%
    html_nodes(".css-6068ga") %>%
    html_text()

  Link <- doc %>%
    html_nodes(".css-hndxeu") %>%
    html_attr("href")

  Link <- Link[seq(from = 1, to = 45, by = 3)]
   
  
  # Combine attributes into a single data frame
  df <- data.frame(Title, Author, Publisher, Weeksonthelist, Weekdate, Link)
  
  return(df)
  
}

a = scrape_nytimes('https://www.nytimes.com/books/best-sellers/2020/02/02/combined-print-and-e-book-fiction/')