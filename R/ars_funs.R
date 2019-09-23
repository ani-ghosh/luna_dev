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

# Description
# Application for Extracting and Exploring Analysis Ready Samples (AρρEEARS)

# Set credentials for next steps
.ars_login <- function(){
  # earthdata login credentials will work here
  cred <- luna::getCredentials(url = "https://urs.earthdata.nasa.gov/users/new")
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

# token_response <- .ars_login()

.ars_logout <- function(token_response){
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

# .ars_logout(token_response)


# Get list of all products available with the API and corresponding quality bands, if available.

arsProducts <- function(quality = FALSE){
  # product details
  response <- httr::GET("https://lpdaacsvc.cr.usgs.gov/appeears/api/product")
  product_response <- jsonlite::toJSON(httr::content(response), auto_unbox = TRUE)
  # data.frame of products properties 
  products <- jsonlite::fromJSON(product_response)
  
  if (quality){
    # should it be a different function?
    # Quality details
    qresponse <- httr::GET("https://lpdaacsvc.cr.usgs.gov/appeears/api/quality")
    quality_response <- jsonlite::toJSON(httr::content(qresponse), auto_unbox = TRUE)
    # data.frame of quality information
    quality <- jsonlite::fromJSON(quality_response)
    # merge product-quality information
    products <- merge(products, quality, by = "ProductAndVersion", all.x = TRUE)
  }
  return(products)
}

# Not run
# pp <- arsProducts()

# details for any particular product
# Product details can be searched with following Product, Platform or Description
# Partial name matching is supported. However user should be aware of the message for number of possible items

arsInfo <- function(product, platform, description, json = TRUE, df = FALSE){
  
  # if missing, return all
  product <- ifelse(missing(product), "?", product)
  platform <- ifelse(missing(platform), "?", platform)
  description <- ifelse(missing(description), "?", description)
  
  pp <- arsProducts()
  
  i <- grep(tolower(product), tolower(pp$Product)) 
  j <- grep(tolower(platform), tolower(pp$Platform)) 
  k <- grep(tolower(description), tolower(pp$Description))
  q <- Reduce(intersect, list(i,j,k))
  
  spp <- pp[q,]
  
  if(df) return(spp)
  
  product_id <- unique(spp$ProductAndVersion)
  
  if(length(product_id) == 0) warning("No products match for the search criteria")
  
  product_response <- list()
  
  for(i in 1:length(product_id)){
    params <- list(pretty = TRUE)  
    response <- httr::GET(paste0("https://lpdaacsvc.cr.usgs.gov/appeears/api/product/", product_id[[i]]), query = params)
    prod_response <- jsonlite::prettify(jsonlite::toJSON(httr::content(response), auto_unbox = TRUE))
    product_response[[i]] <- prod_response  
  }
  names(product_response) <- product_id
  cat("Product information returned for", paste(product_id, collapse = ", "))
  return(product_response)
}  

# Not run
# mcd <- arsInfo(product = "MCD12Q1")
# pop <- arsInfo(product = "popcount", platform = "gpw")
# JSON information for multiple matched products
# snow <- arsInfo(description = "snow")
# Subset of dataframe matching search criteria
# snow <- arsInfo(description = "snow", df = TRUE)


# Data request for a point

arsSubmit <- function(task_name, 
                         start_date, end_date, 
                         product, layer,
                         latitude, longitude,
                         id, category,
                         recurring = FALSE,
                         ....){
  
  # first make sure the login works
  token_response <- .ars_login()
  
  # prepare task parameters --- this is json example 
  # query string is also supported, yet to figure out how to pass on multiple layers or locations 
  
  params <- list()
  params$dates <- data.frame(startDate = start_date, endDate = end_date)
  params$layers <- data.frame(layer = layer, product = product)
  params$coordinates <- data.frame(latitude = latitude, longitude = longitude, id = id, category = category)
  
  listparams <- list(task_type = "point", task_name = task_name)
  listparams$params <- params
  jsontask <- jsonlite::toJSON(listparams, auto_unbox = TRUE, digits = 10)
  
  # submit task
  token <- paste("Bearer", jsonlite::fromJSON(token_response)$token)
  response <- httr::POST("https://lpdaacsvc.cr.usgs.gov/appeears/api/task", body = jsontask, encode = "json", 
                httr::add_headers(Authorization = token, "Content-Type" = "application/json"))
  
  taskinfo <- httr::content(response)
  taskinfo$token <- token
  
  task_response <- jsonlite::prettify(jsonlite::toJSON(taskinfo, auto_unbox = TRUE))
  
  if(grepl("message:", task_response)){
    cat("Task submission unsuccessful, check response message to troubleshoot the problem")
  } else {
    cat("Successfully submitted task. Use `arsTaskStatus` to check status of the task")
  }
  task_response
}

# another function for polygon?

# retrieve all tasks
# should we define a new class for task_response type object?

# task_response is returned from a submitted task, e.g. `arsSubmit`

arsTaskRetrieve <- function(task_response){
  jstask <- jsonlite::fromJSON(task_response)
  token <- jstask$token
  task_id <- jstask$task_id
  tasks <- httr::GET(paste0("https://lpdaacsvc.cr.usgs.gov/appeears/api/task/", task_id), 
                        httr::add_headers(Authorization = token))
  task_response <- jsonlite::prettify(jsonlite::toJSON(httr::content(tasks), auto_unbox = TRUE))
  task_response
  # option to return a simplfied dataframe to better track the results? also save the json with task_id
  out <- jsonlite::fromJSON(task_response)
  out <- out[c("task_id", "task_name", "status")]
}

# check single task status
arsTaskStatus <- function(task_response){
  jstask <- jsonlite::fromJSON(task_response)
  token <- jstask$token
  task_id <- jstask$task_id
  status <- httr::GET(paste0("https://lpdaacsvc.cr.usgs.gov/appeears/api/status/", task_id), 
                        httr::add_headers(Authorization = token))
  
  task_status <- jsonlite::prettify(jsonlite::toJSON(httr::content(status), auto_unbox = TRUE))
  task_status
}

# Delete a request
arsTaskDelete <- function(task_response){
  jstask <- jsonlite::fromJSON(task_response)
  token <- jstask$token
  task_id <- jstask$task_id
  delete <- httr::DELETE(paste0("https://lpdaacsvc.cr.usgs.gov/appeears/api/task/", task_id), 
                      httr::add_headers(Authorization = token))

  
  if(grepl("204", delete$status_code)){
    cat("Successfully deleted request")
  } else {
    cat("Task submission unsuccessful, check response message to troubleshoot the problem")
  }
}

# create bundle to download result
arsTaskBundle <- function(task_response){
  jstask <- jsonlite::fromJSON(task_response)
  token <- jstask$token
  task_id <- jstask$task_id
  bundle <- httr::GET(paste0("https://lpdaacsvc.cr.usgs.gov/appeears/api/bundle/", task_id), 
                      httr::add_headers(Authorization = token))
  
  bundle_status <- jsonlite::prettify(jsonlite::toJSON(httr::content(bundle), auto_unbox = TRUE))
  bundle_status
}

# finally download results
arsDownload <- function(task_response, path){
  
  # also need token
  
  # check if the processing is complete
  bundle_status <- arsTaskBundle(task_response)
  
  bundle <- jsonlite::fromJSON(bundle_status)
  # subset bundle bu task_id?
  
  # create a destination directory to store the files
  # save the files in folder based on their task_id/task_name
  path <- .getCleanPath(path)
  dir <- file.path(path, "appeears", bundle$task_id) #task_name is a better option
  dir.create(dir, FALSE, TRUE)
  
  # retrieve the filename from the file_id
  files <- bundle$files
  
  for (i in 1:nrow(files)){
    ff <- files[i,]
    
    filename <- file.path(dir, ff$file_name)
    
    # write the file to disk using the destination directory and file name 
    response <- httr::GET(paste0("https://lpdaacsvc.cr.usgs.gov/appeears/api/bundle/", bundle$task_id, '/', ff$file_id),
                    httr::write_disk(filename, overwrite = TRUE), httr::progress(), httr::add_headers(Authorization = token)) 
  }
  
}

# not run
# path <- "/scratch/tmp"
# task_name = "ani_example" 
# start_date = "01-01-2015" 
# end_date = "02-01-2015"
# layer = "LST_Day_1km" 
# product = "MOD11A2.006"
# latitude = 45 
# longitude = -93;
# id = "ani_location" 
# category = "ani_category"