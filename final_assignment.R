#### Research question:

#"_Rolling Stone_ Magazine ranked their 100 greatest musical artists of all time. At the end of 2023, how has their music endured? Are there any features or characteristics that seem to explain enduring engagement? 

#### Required data sources:

#- [Rolling Stone's 100 Greatest Artists, 2010](https://www.rollingstone.com/music/music-lists/100-greatest-artists-147446/)

#- [Spotify Web API](https://developer.spotify.com/documentation/web-api)


library(tidyverse)
library(RSelenium)
library(netstat)
library(DBI)
library(RSQLite)
library(purrr)


################################################################################
# Step 0: Create a relational database

# Create database
db <- dbConnect(RSQLite::SQLite(), "data/spotify.sqlite")

# Check existence of the database
print(file.exists("data/spotify.sqlite"))

# Check which tables exist in the database
tables <- dbListTables(db)
print(tables)

# Function that checks whether a table exists in the relational database
check_table <- function(db, a_table){
  
  # Check if the "a_table" exists
  if (a_table %in% dbListTables(db)) {
    
    # Get the row count
    query <- paste("SELECT COUNT(*) FROM", a_table)
    row_count <- dbGetQuery(db, query)[1, 1]
    
    # Get the column names
    column_names <- as.character(dbListFields(db, a_table))
    
    # Get the column count
    column_count <- length(unlist(column_names))
    
    # Print out dimensions and column names
    formatted_string <- sprintf("The table %s exists and has the following dimensions: \nNumber of rows: %s \nNumber of columns: %s", a_table, row_count, column_count)
    column_answer <- sprintf("Column names: %s", paste(column_names, collapse = ", "))
    
    return(cat(formatted_string, column_answer, sep = "\n"))
    
  } else {
    # If "a_table" does not exist, return this statement:
    return(cat("The table does not exist."))
  }
}

################################################################################
# Step 1: Get the data

################################################################################
# Step 1A: Rolling Stones data

# Launch the driver and browser
invisible(capture.output({
  rD <- rsDriver(browser=c("firefox"), port = free_port(random = TRUE), chromever = NULL) 
  driver <- rD$client
}))

# Navigate to the website
url <- "https://www.rollingstone.com/music/music-lists/100-greatest-artists-147446/"
driver$navigate(url)

# Reject the privacy policies
reject_button <- driver$findElement(using = "xpath", 
                                    value = '//*[@id="onetrust-reject-all-handler"]')
# Click on the button:
reject_button$clickElement()


# Create a data frame that will later hold the top 100 artists
top_hundred_artists_df <- data.frame(
  Artist_Name = character(0),
  Ranking = numeric(0),
  Article_Text = character(0),
  stringsAsFactors = FALSE
)

# Add 100 empty rows
top_hundred_artists_df <- top_hundred_artists_df[rep(NA, 100), ]


# Click the load more button
load_more <- function(){
  # Load more button
  load_more_button <- driver$findElement(using = "xpath", 
                                      value = '/html/body/div[5]/main/div[2]/div[1]/div/article/div[3]/div[2]/div[2]/a')
  # Click on the button:
  load_more_button$clickElement()
}

# Click the load previous button
load_previous <- function(){
  # Load more button
  load_previous_button <- driver$findElement(using = "xpath", 
                                         value = '/html/body/div[5]/main/div[2]/div[1]/div/article/div[3]/div[2]/div[1]')
  # Click on the button:
  load_previous_button$clickElement()
}


# Extract the rankings
extract_rank <- function(){
  # Find all elements with the class name "c-gallery-vertical-album__number"
  artist_rank_elements <- driver$findElements(using = "class name", value = "c-gallery-vertical-album__number")
  
  # Extract text from each element
  artist_ranks <- sapply(artist_rank_elements, function(element) element$getElementText()[[1]])
  
  # Print or use the extracted information
  return(artist_ranks)
}


# Extract the artist names
extract_artist_names <- function(){
  # Find all elements with the class name "c-gallery-vertical-album__title"
  artist_name_elements <- driver$findElements(using = "class name", value = "c-gallery-vertical-album__title")
  
  # Extract text from each element
  artist_names <- sapply(artist_name_elements, function(element) element$getElementText()[[1]])
  
  # Print or use the extracted information
  return(artist_names)
}


# Extract the article text
extract_article_text <- function(){
  # Find all elements with the class name "c-gallery-vertical-album__title"
  article_text_elements <- driver$findElements(using = "class name", value = "c-gallery-vertical-album__description")
  
  # Extract text for the first 50 elements
  article_text <- sapply(article_text_elements, function(element) element$getElementText()[[1]])
  
  # Print or use the extracted information
  return(article_text)
}

# Scrape the data
scrape_artist_rankings <- function(){
  # Assign values from the vector to the first 50 rows of the "Ranking" column in top_hundred_artists_df
  top_hundred_artists_df$Ranking[1:50] <- extract_rank()
  top_hundred_artists_df$Artist_Name[1:50] <- extract_artist_names()
  top_hundred_artists_df$Article_Text[1:50] <- extract_article_text()

  # Click the load more button
  load_more()
  Sys.sleep(2)

  # Assign values from the vector to the last 50 rows of the "Ranking" column in top_hundred_artists_df
  top_hundred_artists_df$Ranking[51:100] <- extract_rank()
  top_hundred_artists_df$Artist_Name[51:100] <- extract_artist_names()
  top_hundred_artists_df$Article_Text[51:100] <- extract_article_text()
  
  return(top_hundred_artists_df)
}

# Call the artist ranking web scraping function
top_hundred_artists_df <- scrape_artist_rankings()

# Save the data frame as a global variable
assign("top_hundred_artists_df", top_hundred_artists_df, envir = .GlobalEnv)

# Save the data frame as an RData file in the current working directory
save(top_hundred_artists_df, file = "data/top_hundred_artists_data.RData")


# Load the data from the RData file
load("data/top_hundred_artists_data.RData")

# Make a copy of the data frame
top_hundred_artists <- data.frame(top_hundred_artists_df)

# Close the browser

# Close the RSelenium processes:
driver$close()
# Close the associated Java processes
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)



################################################################################
# Step 1B: Spotify API data

library(httr)
library(base64enc)

readRenviron("../../Documents/R_Environs/spotify_api.env")
client_id <- Sys.getenv("CLIENT_ID")
client_secret <- Sys.getenv("CLIENT_SECRET")

# Create a base64-encoded string of the client ID and client secret
credentials <- paste0(client_id, ':', client_secret)
base64_credentials <- base64enc::base64encode(charToRaw(credentials))

# Set up the authentication request
auth_response <- httr::POST(
  'https://accounts.spotify.com/api/token',
  add_headers(
    Authorization = paste0('Basic ', base64_credentials),
    'Content-Type' = 'application/x-www-form-urlencoded'
  ),
  body = list(grant_type = 'client_credentials'),
  encode = 'form'
)

# Check for HTTP errors
if (http_error(auth_response)) {
  print(status_code(auth_response))
  print(content(auth_response, "text"))
} else {
  # Extract the access token from the response
  access_token <- httr::content(auth_response)$access_token
  print(access_token)
}



# Get the artist id for each artist in the top_hundred_artists data frame
library(httr)
library(jsonlite)

# Function to get the Spotify Artist ID for a given artist name
get_artist_id <- function(artist_name) {
  # Define the Spotify API endpoint for searching an artist
  search_url <- 'https://api.spotify.com/v1/search'
  
  # Set up the request with the access token
  search_response <- GET(
    search_url,
    query = list(q = artist_name, type = 'artist'),
    add_headers(Authorization = paste0('Bearer ', access_token))
  )
  
  # Extract the artist ID from the response
  search_results <- content(search_response, "parsed")
  
  # Check if any results were returned
  if (length(search_results$artists$items) > 0) {
    artist_id <- search_results$artists$items[[1]]$id
    return(artist_id)
  } else {
    # Return NA or any other value to indicate no match
    return(NA)
  }
}

# Apply the function to the entire "Artist_Name" column in the data frame
top_hundred_artists$Spotify_Artist_ID <- sapply(top_hundred_artists$Artist_Name, get_artist_id)


# Get the artist info for each artist in the top_hundred_artists data frame
get_artist_info <- function(artist_id){
  # Define the Spotify API endpoint for getting information about an artist
  artist_url <- paste0('https://api.spotify.com/v1/artists/', artist_id)
  
  # Set up the request with the access token
  artist_response <- GET(artist_url, add_headers(Authorization = paste0('Bearer ', access_token)))
  
  # Extract the artist information from the response
  artist_info <- content(artist_response)
  
  followers <- artist_info$followers$total[1]
  genres <- artist_info$genres[1]
  popularity <- artist_info$popularity
  
  # Create a list with the extracted information
  artist_data <- list(followers = followers, genres = genres, popularity = popularity)
  
  return(artist_data)
}

# Apply the function to the entire "Artist_Name" column in the data frame
result <- lapply(top_hundred_artists$Spotify_Artist_ID, get_artist_info)

# Extract individual elements
top_hundred_artists$Followers <- sapply(result, function(x) x$followers)
top_hundred_artists$Genres <- sapply(result, function(x) x$genres)
top_hundred_artists$Popularity <- sapply(result, function(x) x$popularity)


# Remove the row names
top_hundred_artists <- data.frame(top_hundred_artists, row.names = NULL)
# Transform the "Genres" column into type character 
top_hundred_artists$Genres <- sapply(top_hundred_artists$Genres, function(x) paste(x, collapse = ","))

# Write top_hundred_artists_df to the relational database
dbWriteTable(db, "top_hundred_artists_df", top_hundred_artists, overwrite = TRUE)

# Call check_table on "top_hundred_artists_df"
check_table(db, "top_hundred_artists_df")



# Get the artist top tracks

# Create an empty data frame for the top tracks
top_tracks_data <- data.frame(
  Spotify_Artist_ID = character(0),
  Artist_Name = character(0),
  Spotify_Track_ID = character(0),
  Track_Name = character(0),
  Track_Popularity = numeric(0),
  Track_Duration = numeric(0),
  Album_Release_Date = character(0),
  stringsAsFactors = FALSE
)

# Function that queries the API for each artist's top track data given their ID
get_data <- function(the_artist_id, market="US") {
  # Define the Spotify API endpoint for getting an artist's top tracks
  top_tracks_url <- paste0('https://api.spotify.com/v1/artists/', the_artist_id, '/top-tracks?market=', market)
  
  # Set up the request with the access token
  top_tracks_response <- GET(
    top_tracks_url,
    add_headers(Authorization = paste0('Bearer ', access_token))
  )
  
  # Extract the top tracks from the response
  top_tracks <- content(top_tracks_response, "parsed")
  
  return(top_tracks)
}

# Function that gets the top tracks data given the artist ID and using the function get_data
get_top_tracks <- function(the_artist_id) {
  
  # Get the data given the ein
  top_tracks <- get_data(the_artist_id)
  
  # Loop through the top tracks for each artist
  if (!is.null(top_tracks$tracks) && length(top_tracks$tracks) > 0) {
    # Enter the loop
    for (i in seq(length(top_tracks$tracks))) {
      # Calculate Collaboration value for each song
      collaboration_value <- ifelse(length(top_tracks$tracks[[i]]$artists) > 1, 1, 0)
      
      # Check and extract values, appending NA if a value is missing
      top_tracks_data <- rbind(top_tracks_data, data.frame(
        Spotify_Artist_ID = ifelse(!is.null(top_tracks$tracks[[i]]$artists[[1]]$id), the_artist_id, NA),
        Artist_Name = ifelse(!is.null(top_tracks$tracks[[i]]$artists[[1]]$name), top_tracks$tracks[[i]]$artists[[1]]$name, NA),
        Spotify_Track_ID = ifelse(!is.null(top_tracks$tracks[[i]]$id), top_tracks$tracks[[i]]$id, NA),
        Track_Name = ifelse(!is.null(top_tracks$tracks[[i]]$name), top_tracks$tracks[[i]]$name, NA),
        Track_Popularity = ifelse(!is.null(top_tracks$tracks[[i]]$popularity), top_tracks$tracks[[i]]$popularity, NA),
        Track_Duration = ifelse(!is.null(top_tracks$tracks[[i]]$duration_ms), top_tracks$tracks[[i]]$duration_ms, NA),
        Album_Release_Date = ifelse(!is.null(top_tracks$tracks[[i]]$album$release_date), top_tracks$tracks[[i]]$album$release_date, NA),
        Collaboration = collaboration_value
      ))
    }
  } else {
    # Print a message or take appropriate action when top_tracks$tracks is NULL or empty
    top_tracks_data <- rbind(top_tracks_data, data.frame(
      Spotify_Artist_ID = the_artist_id,
      Artist_Name = NA,
      Spotify_Track_ID = NA,
      Track_Name = NA,
      Track_Popularity = NA,
      Track_Duration = NA,
      Album_Release_Date = NA,
      Collaboration = NA
    ))
  }
  
  return(top_tracks_data)
}

result_list <- lapply(top_hundred_artists$Spotify_Artist_ID, get_top_tracks)
top_tracks_data <- do.call(rbind, result_list)


### NOTE: Drop the artist name later and if you need it retrieve it through SQL query from the "top_hundred_artists" data frame.


# Write top_tracks_data to the relational database
dbWriteTable(db, "top_tracks_df", top_tracks_data, overwrite = TRUE)

# Call check_table on "top_tracks_df"
check_table(db, "top_tracks_df")




# Third table: Top tracks of 2023, USA

# Get a playlist
get_playlist <- function(playlist_id){
 
  # Spotify API endpoint for getting playlist details
  endpoint <- paste0('https://api.spotify.com/v1/playlists/', playlist_id)
  
  # Set up the request headers with the access token
  headers <- c('Authorization' = paste0('Bearer ', access_token))
  
  # Make the GET request
  playlist_response <- GET(endpoint, add_headers(headers))
  playlist_data <- content(playlist_response, 'parsed')
  
  return(playlist_data)
}

# Create an empty dataframe

top_tracks_2023_USA <- data.frame(
  Playlist_Name = character(0),
  Playlist_ID = character(0),
  Playlist_Description = character(0),
  Track_Name = character(0),
  Track_Artist = character(0),
  Spotify_Track_ID = character(0),
  stringsAsFactors = FALSE
)

# function that gets the playlist info given the playlist id
get_playlist_info <- function(playlist_id) {
  
  # Get the playlist data given the playlist id
  dt <- get_playlist(playlist_id)
  
  # Loop to append values
  for (i in seq(length(results_top_tracks_2023_USA$tracks$items))) {
    # Append the values to the dataframe
    top_tracks_2023_USA <- rbind(top_tracks_2023_USA, data.frame(Playlist_Name = dt$name,
                                                                 Playlist_ID = dt$id,
                                                                 Playlist_Description = dt$description,
                                                                 Track_Name = dt$tracks$items[[i]]$track$name,
                                                                 Track_Artist = dt$tracks$items[[i]]$track$artists[[1]]$name,
                                                                 Spotify_Track_ID = dt$tracks$items[[i]]$track$id
                                                                 ))
  }
  
  # Return the resulting dataframe
  return(top_tracks_2023_USA)
}

top_tracks_2023_USA_id <- "37i9dQZF1DXbJMiQ53rTyJ"
top_tracks_2023_USA <- get_playlist_info(top_tracks_2023_USA_id)


# Write top_tracks_2023_USA to the relational database
dbWriteTable(db, "top_tracks_2023_USA_df", top_tracks_2023_USA, overwrite = TRUE)

# Call check_table on "top_tracks_2023_USA_df"
check_table(db, "top_tracks_2023_USA_df")


# Fourth table: Top artists of 2023, USA

# A playlist dataframe
top_artists_USA_id <- "37i9dQZF1DX4dwwIoKH0j7"
top_artists_2023_USA <- get_playlist_info(top_artists_USA_id)

# However, we are not interested in the tracks here but in the artists.
# Since some tracks have several artists associated with them, we do the following:

get_tracks_23 <- function(track_id){
  
  # Spotify API endpoint for getting playlist details
  endpoint <- paste0('https://api.spotify.com/v1/tracks/', track_id)
  
  # Set up the request headers with the access token
  headers <- c('Authorization' = paste0('Bearer ', access_token))
  
  # Make the GET request
  tracks_23_response <- GET(endpoint, add_headers(headers))
  tracks_23_data <- content(tracks_23_response, 'parsed')
  
  return(tracks_23_data)
}

top_artists_2023 <- data.frame(
  Artist_Name = character(0),
  stringsAsFactors = FALSE
)

# function that gets the playlist info given the playlist id
get_artists_23 <- function(track_id) {
  
  # Get the playlist data given the playlist id
  dt <- get_tracks_23(track_id)
  
  for (i in seq(length(dt$artists))){
    top_artists_2023 <- rbind(top_artists_2023, data.frame(Artist_Name = dt$artists[[i]]$name))
  }
  
  # Return the resulting dataframe
  return(top_artists_2023)
}

top_artists_2023 <- get_artists_23("0vjeOZ3Ft5jvAi9SBFJm1j")


result_list <- lapply(top_artists_2023_USA$Spotify_Track_ID, get_artists_23)
top_artists_2023 <- do.call(rbind, result_list)

# We also need more information for these top artists in 2023. 
# Artist ID


top_artists_2023$Spotify_Artist_ID <- NA

library(httr)
access_token

# Function to search for artist by name and get the artist ID
get_artist_id <- function(artist_name) {
  # Spotify API endpoint for searching artists by name
  endpoint <- 'https://api.spotify.com/v1/search'
  
  # Set up the request parameters with a filter for artists
  params <- list(q = artist_name, type = 'artist', limit = 1)
  
  # Set up the request headers with the access token
  headers <- c('Authorization' = paste0('Bearer ', access_token))
  
  # Make the GET request
  response <- GET(endpoint, query = params, add_headers(headers))
  
  search_result <- content(response, 'parsed')
  
  id <- search_result$artists$items[[1]]$id
  
  return(id)
}

top_artists_2023$Spotify_Artist_ID <- lapply(top_artists_2023$Artist_Name, get_artist_id)


# Popularity

top_artists_2023$Popularity <- NA

# Function to search for artist by name and get the artist ID
get_popularity <- function(artist_name) {
  # Spotify API endpoint for searching artists by name
  endpoint <- 'https://api.spotify.com/v1/search'
  
  # Set up the request parameters with a filter for artists
  params <- list(q = artist_name, type = 'artist', limit = 1)
  
  # Set up the request headers with the access token
  headers <- c('Authorization' = paste0('Bearer ', access_token))
  
  # Make the GET request
  response <- GET(endpoint, query = params, add_headers(headers))
  
  search_result <- content(response, 'parsed')
  
  popularity <- search_result$artists$items[[1]]$popularity
  
  return(popularity)
}

top_artists_2023$Popularity <- lapply(top_artists_2023$Artist_Name, get_popularity)



# Followers

top_artists_2023$Followers <- NA

# Function to search for artist by name and get the artist ID
get_followers <- function(artist_name) {
  # Spotify API endpoint for searching artists by name
  endpoint <- 'https://api.spotify.com/v1/search'
  
  # Set up the request parameters with a filter for artists
  params <- list(q = artist_name, type = 'artist', limit = 1)
  
  # Set up the request headers with the access token
  headers <- c('Authorization' = paste0('Bearer ', access_token))
  
  # Make the GET request
  response <- GET(endpoint, query = params, add_headers(headers))
  
  search_result <- content(response, 'parsed')
  
  followers <- search_result$artists$items[[1]]$followers$total
  
  return(followers)
}

top_artists_2023$Followers <- lapply(top_artists_2023$Artist_Name, get_followers)

# Convert list columns to character
top_artists_2023$Spotify_Artist_ID <- as.character(top_artists_2023$Spotify_Artist_ID)
top_artists_2023$Popularity <- as.character(top_artists_2023$Popularity)
top_artists_2023$Followers <- as.character(top_artists_2023$Followers)

# Write top_artists_2023 to the relational database
dbWriteTable(db, "top_artists_2023_USA_df", top_artists_2023, overwrite = TRUE)

# Call check_table on "top_artists_2023_USA_df"
check_table(db, "top_artists_2023_USA_df")


################################################################################
# Fifth table: Event Information using the Ticketmaster API

# Set up the API key
library("jsonlite")
library("httr")

readRenviron("../../Documents/R_Environs/ticketmaster_api.env")
ticketmaster_apikey <- Sys.getenv("KEY")



get_event_data <- function(artist_name, ticketmaster_apikey){
  # Specify an endpoint (e.g., events)
  endpoint <- "events"
  
  # Build the complete URL
  url <- paste0("https://app.ticketmaster.com/discovery/v2/", endpoint)
  
  # Set query parameters
  params <- list(
    apikey = ticketmaster_apikey,
    countryCode = "US",
    keyword = artist_name,  # Replace with the name of the artist
    startDateTime = "2023-01-01T00:00:00Z",  # Set the start date to filter past events
    endDateTime = "2023-12-31T23:59:59Z"    # Set the end date
    #size = 5 
  )
  
  # Make the GET request
  response <- GET(url, query = params)
  content <- content(response, "parsed") 
  
  return(content)
}

content <- get_event_data("Taylor Swift", ticketmaster_apikey)


# Create an empty dataframe

event_data <- data.frame(
  Artist_Name = character(0),
  Event_Name = character(0),
  Event_ID = character(0),
  stringsAsFactors = FALSE
)

# function that gets the finance data given the ein and using the functions get_university_name and get_data
get_ticketmaster_info <- function(artist_name, apikey = ticketmaster_apikey) {
  
  # Get the playlist data given the playlist id
  dt <- get_event_data(artist_name, apikey)
  
  
  # Loop through the top tracks for each artist
  if (!is.null(dt$'_embedded'$events) && length(dt$'_embedded'$events) > 0) {
    # Loop to append values
    for (i in seq(length(dt$'_embedded'$events))) {
      # Append the values to the data frame
      event_data <- rbind(event_data, data.frame(Artist_Name = artist_name,
                                                 Event_Name = dt$'_embedded'$events[[i]]$name,
                                                 Event_ID = dt$'_embedded'$events[[i]]$id
      ))
    }
    
  } else {
    # Print a message or take appropriate action when top_tracks$tracks is NULL or empty
    event_data <- rbind(event_data, data.frame(
      Artist_Name = artist_name,
      Event_Name = NA,
      Event_ID = NA
    ))
  }
  
  # Return the resulting data frame
  return(event_data)
}

result_list <- lapply(top_hundred_artists$Artist_Name, get_ticketmaster_info)
event_data <- do.call(rbind, result_list)

# Clean the data
event_data <- event_data %>%
  # Clean up "The Drifters"
  mutate(
    Event_Name = ifelse(Artist_Name == "The Drifters", NA, Event_Name),
    Event_ID = ifelse(Artist_Name == "The Drifters", NA, Event_ID)
  ) %>%
  filter(!duplicated(Artist_Name) | Artist_Name != "The Drifters")  %>%
  
  # Clean up "Eagles"
  mutate(
    Event_Name = ifelse(Artist_Name == "Eagles", NA, Event_Name),
    Event_ID = ifelse(Artist_Name == "Eagles", NA, Event_ID)
  ) %>%
  filter(!duplicated(Artist_Name) | Artist_Name != "Eagles")  %>%
  
  # Clean up "The Doors"
  mutate(
    Event_Name = ifelse(Artist_Name == "The Doors", NA, Event_Name),
    Event_ID = ifelse(Artist_Name == "The Doors", NA, Event_ID)
  ) %>%
  filter(!duplicated(Artist_Name) | Artist_Name != "The Doors")  %>%
  
  # Clean up "The Police"
  filter(!(Artist_Name == "The Police" & (Event_Name == "Fire Vs Police Flag Football" | Event_Name == "POLICE STATE / GEMM / CENOBITE / DOGPILE"))) %>%
  
  # Clean up "Cream" 
  mutate(
    Event_Name = ifelse(Artist_Name == "Cream", NA, Event_Name),
    Event_ID = ifelse(Artist_Name == "Cream", NA, Event_ID)
  ) %>%
  filter(!duplicated(Artist_Name) | Artist_Name != "Cream")  %>%
  
  # Clean up "Grateful Dead"
  filter(!(Artist_Name == "Grateful Dead" & (Event_Name == "DEAD NIGHT w/ Grateful Upstate Toodeloo" | Event_Name == "POLICE STATE / GEMM / CENOBITE / DOGPILE"))) %>%
  
  # Clean up "Howlin' Wolf" 
  mutate(
    Event_Name = ifelse(Artist_Name == "Howlin’ Wolf", NA, Event_Name),
    Event_ID = ifelse(Artist_Name == "Howlin’ Wolf", NA, Event_ID)
  ) %>%
  filter(!duplicated(Artist_Name) | Artist_Name != "Howlin’ Wolf")  %>%
  
  # Clean up "Queen"
  filter(!(Artist_Name == "Queen" & Event_Name == "The Snow Queen")) %>%

  # Clean up "The Band" 
  mutate(
    Event_Name = ifelse(Artist_Name == "The Band", NA, Event_Name),
    Event_ID = ifelse(Artist_Name == "The Band", NA, Event_ID)
  ) %>%
  filter(!duplicated(Artist_Name) | Artist_Name != "The Band")  %>%

  # Clean up "The Doors" 
  mutate(
    Event_Name = ifelse(Artist_Name == "The Doors", NA, Event_Name),
    Event_ID = ifelse(Artist_Name == "The Doors", NA, Event_ID)
  ) %>%
  filter(!duplicated(Artist_Name) | Artist_Name != "The Doors")  %>%

  # Clean up "The Who" 
  mutate(
    Event_Name = ifelse(Artist_Name == "The Who", NA, Event_Name),
    Event_ID = ifelse(Artist_Name == "The Who", NA, Event_ID)
  ) %>%
  filter(!duplicated(Artist_Name) | Artist_Name != "The Who")  %>%
    
  # Clean up "Buddy Holly" 
  mutate(
    Event_Name = ifelse(Artist_Name == "Buddy Holly", NA, Event_Name),
    Event_ID = ifelse(Artist_Name == "Buddy Holly", NA, Event_ID)
  ) %>%
  filter(!duplicated(Artist_Name) | Artist_Name != "Buddy Holly")  %>%
    
  # Clean up "The Beach Boys" 
  mutate(
    Event_Name = ifelse(Artist_Name == "The Beach Boys", NA, Event_Name),
    Event_ID = ifelse(Artist_Name == "The Beach Boys", NA, Event_ID)
  ) %>%
  filter(!duplicated(Artist_Name) | Artist_Name != "The Beach Boys")  %>%

  # Clean up "James Brown" 
  mutate(
    Event_Name = ifelse(Artist_Name == "James Brown", NA, Event_Name),
    Event_ID = ifelse(Artist_Name == "James Brown", NA, Event_ID)
  ) %>%
  filter(!duplicated(Artist_Name) | Artist_Name != "James Brown")
    

event_data <- event_data %>%
  left_join(top_hundred_artists, by = 'Artist_Name') %>%
  select(Artist_Name, Spotify_Artist_ID, Event_Name, Event_ID)    

# Write event_data to the relational database
dbWriteTable(db, "event_data_df", event_data, overwrite = TRUE)

# Call check_table on "event_data_df"
check_table(db, "event_data_df")


################################################################################
# Table 5: Platinum certifications from RIAA

# RIAA does not allow webscraping: https://www.riaa.com/privacy-policy-and-terms-of-use/ 
# Therefore, I used wikipedia as a source to scrape information on album awards
# Be aware, that the wikipedia data might not reflect the actual data on RIAA perfectly

library(rvest)
url <- "https://en.wikipedia.org/wiki/List_of_highest-certified_music_artists_in_the_United_States"

get_wikipedia_tables <- function(url){
  # Storing the URL's HTML code
  html_content <- read_html(url)
  
  # Extracting all tables in the document 
  tab <- html_table(html_content, fill = TRUE)
  
  return(tab)
}

tab <- get_wikipedia_tables(url)

certified_albums_data <- as_tibble(tab[[1]][, 1:5])
certified_singles_data <- as_tibble(tab[[2]][, 1:5])

# Save the tables as global variables
assign("certified_albums_data", certified_albums_data, envir = .GlobalEnv)
assign("certified_singles_data", certified_singles_data, envir = .GlobalEnv)

# Save the global variables to an RData files
save(certified_albums_df, file = "data/certified_albums_data.RData")
save(certified_singles_df, file = "data/certified_singles_data.RData")


# Load global variables
load("data/certified_albums_data.RData")
load("data/certified_singles_data.RData")

# Copy the scraped table to avoid modifying the original data
certified_albums <- certified_albums_data
certified_singles <- certified_singles_data


certified_albums <- certified_albums %>%
  rename(Artist_Name = Name) %>%
  mutate(Spotify_Artist_ID = ifelse(Artist_Name %in% top_hundred_artists$Artist_Name, 
                            top_hundred_artists$Spotify_Artist_ID[match(Artist_Name, top_hundred_artists$Artist_Name)], 
                            NA)) %>%
  mutate(Top_Hundred = ifelse(!is.na(Spotify_Artist_ID), 1, 0))

certified_singles <- certified_singles %>%
  rename(Artist_Name = Name) %>%
  mutate(Spotify_Artist_ID = ifelse(Artist_Name %in% top_hundred_artists$Artist_Name, 
                                    top_hundred_artists$Spotify_Artist_ID[match(Artist_Name, top_hundred_artists$Artist_Name)], 
                                    NA)) %>%
  mutate(Top_Hundred = ifelse(!is.na(Spotify_Artist_ID), 1, 0))



# Write certified_singles and certified_singles to the relational database
dbWriteTable(db, "certified_albums_df", certified_albums, overwrite = TRUE)
dbWriteTable(db, "certified_singles_df", certified_singles, overwrite = TRUE)

# Call check_table on "certified_albums_df" and "certified_singles_df"
check_table(db, "certified_albums_df")
check_table(db, "certified_singles_df")




################################################################################
# Check out the data

dbGetQuery(db, "SELECT * FROM certified_albums_df LIMIT 5")
dbGetQuery(db, "SELECT * FROM certified_singles_df LIMIT 5")
dbGetQuery(db, "SELECT * FROM event_data_df LIMIT 5")
dbGetQuery(db, "SELECT * FROM top_hundred_artists_df LIMIT 5")
dbGetQuery(db, "SELECT * FROM top_tracks_2023_USA_df LIMIT 5")
dbGetQuery(db, "SELECT * FROM top_tracks_df LIMIT 5")


check_table(db, "top_tracks_2023_USA_df")


################################################################################
# Data Analysis
library(plotly)

# Step 1: Are the top 100 artists still relevant?
# Analysis of the performance on Spotify

# 1A: First plot: top 100 ranking vs. followers and popularity

# Create an analysis table with the data we are interested in 

# 1. Column: Artist name
# 2. Column: Artist ID
# 4. Column: Followers
# 5. Column: Popularity

first_query_top100 <- "
  SELECT Artist_Name, Spotify_Artist_ID, Followers, Popularity
  FROM top_hundred_artists_df;
"

first_query_top23 <- "
  SELECT Artist_Name, Spotify_Artist_ID, Followers, Popularity
  FROM top_artists_2023_USA_df;
"

# Execute the query
first_result_top100 <- dbGetQuery(db, first_query_top100)
first_result_top23 <- dbGetQuery(db, first_query_top23)

# Add indicators
first_result_top100 <- first_result_top100 %>%
  mutate(Artist_Type = "Rolling Stones Top 100") %>%
  mutate(Followers = as.numeric(Followers)) %>%
  mutate(Popularity = as.numeric(Popularity))

first_result_top23 <- first_result_top23 %>%
  mutate(Artist_Type = "Spotify Top Artists 2023") %>%
  mutate(Followers = as.numeric(Followers)) %>%
  mutate(Popularity = as.numeric(Popularity))

first_result <- rbind(first_result_top100, first_result_top23)

# Calculate means
mean_followers_top100 <- mean(first_result_top100$Followers)
mean_followers_top23 <- mean(first_result_top23$Followers)

mean_popularity_top100 <- mean(first_result_top100$Popularity)
mean_popularity_top23 <- mean(first_result_top23$Popularity)

# First Plot: Popularity vs followers 
first_plot <- ggplot(first_result, 
                     aes(y = Popularity, x = Followers, color = factor(Artist_Type))) +
  geom_point(size = 1) +
  # Add means
  geom_point(aes(x = mean_followers_top100, y = mean_popularity_top100), color = "#F8766D", size = 3, fill = "#F8766D", shape = 23, text = "Mean Top 100") +
  geom_point(aes(x = mean_followers_top23, y = mean_popularity_top23), color = "#00BFC4", size = 3, fill = "#00BFC4", shape = 23, text = "Mean Top 100") +
  geom_vline(xintercept = mean_followers_top100, linetype = "dotted", color = "#F8766D") +
  geom_hline(yintercept = mean_popularity_top100, linetype = "dotted", color = "#F8766D") +
  geom_vline(xintercept = mean_followers_top23, linetype = "dotted", color = "#00BFC4") +
  geom_hline(yintercept = mean_popularity_top23, linetype = "dotted", color = "#00BFC4") +
  # Add titles
  labs(title = "Followers vs. Popularity by Artist Type",
       x = "Spotify Followers",
       y = "Spotify Popularity",
       color = "Artist Type",
       shape = "Mean") +
  theme_minimal() +
  # Adjust the font size for the title and the axes
  theme(
    axis.text = element_text(size = 8),     
    axis.title = element_text(size = 8),    
    plot.title = element_text(size = 12)
  ) +
  # Use a logarithmic scale for the x-axis
  scale_x_continuous(
    labels = scales::comma_format(scale = 1),
    trans = 'log10'
  ) +
  scale_y_continuous(
    trans = 'log10'
  )


# Display the plot
first_plot

# Convert ggplot to interactive plotly plot
first_plot_plotly <- ggplotly(first_plot) %>%
  layout(font = list(family = "Arial"))

# Display the interactive plot
first_plot_plotly



# Standard colors
# Hex codes for the default ggplot2 color palette
ggplot2_default_colors <- c(
  "#F8766D", "#B79F00", "#00BA38", "#00BFC4", "#619CFF",
  "#F564E3", "#8DD3C7", "#E76BF3", "#00BF7D", "#EBAC23"
)



# Idea: plot the followers and the popularity of the top 100 artists against the followers and popularity of the most streamed artists of 2023
# Do the same with the top songs 
# QUESTION: Können die rolling stones top 100 mit den spotify top 100 mithalten?

# First plot




# Test ggplot: Ranking vs. Followers

# Assuming first_result is your data frame
# Create ggplot with dual y-axes
first_plot <- ggplot(first_result, aes(x = Ranking, y = Followers, text = Artist_Name)) +
  geom_point(size = 1.5, color="#F781BF") +
  geom_point(aes(y = Popularity * 1000000), size = 1.5, color = "#81BEF7") +
  # Add titles
  labs(title = "Top 100 Ranking vs. Spotify Followers",
       x = "Top 100 Ranking") +
  theme_minimal() +
  # Adjust the font size for the title and the axes
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12)
  ) +
  scale_y_continuous(
    name = "Spotify Followers",
    labels = scales::comma_format(scale = 1e0),
    sec.axis = sec_axis(~. * 0.000001, name = "Popularity")
  )

first_plot



# Convert ggplot to interactive plotly plot
first_plot_plotly <- ggplotly(first_plot) %>%
  layout(font = list(family = "Arial"))

# Display the interactive plot
first_plot_plotly






# Step 2: Most artists from the top 100 did not appear in the top tracks of 2023. 
# Why are they ranked as the top 100 artists, nonetheless of their popularity in 2023?
# RIAA ranking -> Seemingly, the top 100 artists align quite a bit with the top 100 artists who received the most platinum awards
# Events -> most of the top 100 artists are dead or not performing anymore 

# Step 3: Further analysis needed
# Social media -> Likely, tracks nowadays are popular, when they go "viral" on TikTok 
# and artists are more than just musicians but also social-media personae. It would be 
# interesting to include social media data in the analysis of which artists are still relevant
# We do not have social media data publicly available. However, we can do a bit of social media 
# analysis with Spotify data.

# Check the duration of the most popular songs 
# Viral hits playlist?








################################################################################
# Close the database connection
dbDisconnect(db)








