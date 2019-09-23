# Authors: Aniruddha Ghosh, Robert J. Hijmans, Alex Mandel
# September 2019
# Version 0.1
# Licence GPL v3

# List different satellite data products that can be searched through LPDAAC AppEEARS
# https://lpdaac.usgs.gov/tools/appeears/
# https://lpdaacsvc.cr.usgs.gov/appeears/api/?language=R#introduction

# Step 1: List of available products (optional)
# Step 2: Log-in request
# Step 3: Data access

# Log-in

library(httr)
library(jsonlite)

# Set credentials for next steps
.loginLPDAAC <- function(){
  # earthdata login credentials will work here
  cred <- luna:::getCredentials(url = "https://urs.earthdata.nasa.gov/users/new")
  secret <- jsonlite::base64_enc(paste(cred$user, cred$password, sep = ":"))
  
  headers <- httr::add_headers("Authorization" = paste("Basic", gsub("\n", "", secret)),
                               "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8")
  
  response <- httr::POST("https://lpdaacsvc.cr.usgs.gov/appeears/api/login", 
                         config = headers, 
                         body = "grant_type=client_credentials")
  
  token_response <- jsonlite::prettify(jsonlite::toJSON(httr::content(response), auto_unbox = TRUE))
  
  if(grepl("message:", token_response)){
    cat("Authentication unsuccessful, check response message to troubleshoot the problem")
  } else {
    cat("Authentication successful")
  }
  return(token_response)
}

# token_response <- .loginLPDAAC()

.logoutLPDAAC <- function(token_response){
  token <- paste("Bearer", jsonlite::fromJSON(token_response)$token)
  headers <- httr::add_headers(Authorization = token,
                  "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8")
  response <- httr::POST("https://lpdaacsvc.cr.usgs.gov/appeears/api/logout",
                    config = headers,
                    body = "grant_type=client_credentials")
  if(grepl("204",response$status_code)){
    cat("Log out successful", "\n")
  } else {
    cat("Log out unsuccessful, check the following message to troubleshoot the problem", "\n")
    return(response)
  }
}

# .logoutLPDAAC(token_response)


# Get list of all products using API and pagination
appeearsProducts <- function(){
  # product details
  response <- httr::GET("https://lpdaacsvc.cr.usgs.gov/appeears/api/product")
  product_response <- jsonlite::toJSON(httr::content(response), auto_unbox = TRUE)
  # data.frame of products properties 
  products <- jsonlite::fromJSON(product_response)
  products$ProductAndVersion <- paste0(products$Product, ".", products$Version)
  
  # should it be a different function?
  # Quality details
  qresponse <- httr::GET("https://lpdaacsvc.cr.usgs.gov/appeears/api/quality")
  quality_response <- jsonlite::toJSON(httr::content(qresponse), auto_unbox = TRUE)
  # data.frame of quality information 
  quality <- jsonlite::fromJSON(quality_response)
  
  # merge product-qualit
  pq <- merge(products, quality, by = "ProductAndVersion", all.x = TRUE)
  return(pq)
}


# Function to convert regular query to json task, e.g. time series for spatial patterns for a sensor

# baseurl <- "https://lpdaacsvc.cr.usgs.gov/appeears/products"
# request <- httr::GET(baseurl)
# httr::stop_for_status(request)
# u <- try(xml2::read_html(baseurl), silent = TRUE)
# 
# library(tidyverse)
# library(rvest)
# u %>% 
#   html_nodes("*") %>% 
#   unique()
# 
# xp <- '//*[@id="top"]/div'
# xp <- '//*[@id="top"]/div[2]/main/div[2]/div/div/div/div[1]/div/select'
# x <- rvest::html_nodes(u, xpath = xp)
# dt <- rvest::html_nodes(u, xpath = "div[contains(@class, 'available-products')")
# u <- try(xml2::read_html(baseurl), silent = TRUE)
# dt <- html_nodes(u, "option")
# dt <- rvest::html_text(dt)

