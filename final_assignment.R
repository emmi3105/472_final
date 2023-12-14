#### Research question:

#"_Rolling Stone_ Magazine ranked their 100 greatest musical artists of all time. At the end of 2023, how has their music endured? Are there any features or characteristics that seem to explain enduring engagement? 

#### Required data sources:

#- [Rolling Stone's 100 Greatest Artists, 2010](https://www.rollingstone.com/music/music-lists/100-greatest-artists-147446/)

#- [Spotify Web API](https://developer.spotify.com/documentation/web-api)


library(tidyverse)
library(RSelenium)
library(netstat)

################################################################################
# Step 1: Get the data

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
save(top_hundred_artists_df, file = "top_hundred_artists_data.RData")


# Load the data from the RData file
load("top_hundred_artists_data.RData")

# Make a copy of the data frame
top_hundred_artists <- data.frame(top_hundred_artists_df)

# Close the browser

# Close the RSelenium processes:
driver$close()
# Close the associated Java processes
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)





