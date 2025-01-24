# Drive.R by JHCV

##### Required Packages #####

library(tidyverse)
library(lubridate)
library(plotly)
library(timetk)
library(ggimage)
library(ggthemes)
library(httr)
library(rvest)
library(wdman)
library(tidytext)
library(scales)
library(jsonlite)

#####

##### Plot Appearance Theme #####

myTheme <- theme(legend.position = "none",
                 plot.background = element_rect(fill = "#02233F"),
                 panel.background = element_rect(fill = "#02233F"),
                 panel.grid = element_line(color = "#274066"),
                 axis.ticks = element_line(color = "#274066"),
                 axis.text = element_text(color = "white"),
                 axis.title = element_text(color = "white"),
                 plot.title = element_text(color = "white",
                                           hjust = .5),
                 plot.subtitle = element_text(color = "white",
                                              hjust = .5),
                 strip.background = element_rect(fill = "#02233F"),
                 strip.text = element_text(color = "white"))

#####

##### Legend appearance theme #####

myLegend <- theme(legend.position = "right",
                  legend.background = element_rect(fill = "#02233F"),
                  legend.text = element_text(color = "white"),
                  legend.title = element_text(color = "white"))#,
#legend.key.height = unit(100, "cm"))

#####

##### auto.dev API Key #####

adApiKey <- "ZrQEPSkKamFtZXNjaXNzZWxAZ21haWwuY29t"

#####

getListings <- function(car = "Toyota 4Runner") {
  # Split the input string into make and model
  carParts <- unlist(strsplit(car, 
                              " ", 
                              fixed = TRUE))
  
  # Ensure that the make and model are extracted correctly
  if (length(carParts) < 2) {
    stop("Please provide both the make and model, separated by a space.")
  }
  
  make <- carParts[1]
  model <- paste(carParts[-1], 
                 collapse = " ") # Handle cases with multi-word models
  
  baseUrl <- "https://auto.dev/api/listings?apikey=ZrQEPSkKamFtZXNjaXNzZWxAZ21haWwuY29t&make="
  
  condition <- "used"
  excludeNoPrice <- "true"
  maxOdo <- "200000"
  
  reqUrl <- paste(baseUrl,
                  make,
                  "&model=",
                  model,
                  "&condition[]=",
                  condition,
                  "&mileage=",
                  maxOdo,
                  "&exclude_no_price=",
                  excludeNoPrice,
                  "&condition[]=certified%20pre-owned",
                  sep = "")
  
  # API Request
  req <- GET(reqUrl)
  res <- fromJSON(content(req, 
                          "text", 
                          encoding = "UTF-8"))
  
  res$pages <- ceiling(res$totalCount/20)
  
  # Initialize df with the first page of results
  df <- as.data.frame(res$records) |> flatten()
  
  # Pagination loop to fetch additional pages
  for (i in 2:251) {
    
    req2 <- GET(paste(reqUrl, 
                      "&page=", 
                      i, 
                      sep = ""))
    
    res2 <- fromJSON(content(req2, 
                             "text", 
                             encoding = "UTF-8"))
    
    df2 <- as.data.frame(res2$records) |> flatten()
    
    df <- bind_rows(df, df2)
  }
  
  # Add 'age' column based on year
  df$age <- 2024 - df$year
  
  df$mpy <- (df$mileageUnformatted/(df$age+.1))#*-1
  
  return(df)
  
}

plotListings <- function(car = "Toyota 4Runner") {
  
  df <- getListings(car)
  
  # Step 1: Identify the top 4 most common trims
  trims <- df |>
    
    group_by(trim) |>
    
    summarize(n = n(),
              avg_price = mean(priceUnformatted, 
                               na.rm = TRUE)) |> # Calculate average price per trim
    
    arrange(desc(n)) |> # Sort by frequency
    
    head(6)   # Select the top 4 most common trims
  
  # Step 2: Sort trims by average price (descending) within the top 4
  trims <- trims |>
    
    arrange(desc(avg_price))
  
  # Step 3: Filter data to include only the top 4 trims and reorder the factor levels
  df <- subset(df, 
               trim %in% trims$trim)
  df$trim <- factor(df$trim, 
                    levels = trims$trim) # Reorder factor levels by avg price
  
  # Step 4: Plot the data
  p <- ggplot(df,
              aes(x = mileageUnformatted,
                  y = priceUnformatted,
                  size = mpy,
                  fill = trim,
                  color = year)) +
    
    geom_point(alpha = .5) +
    
    geom_smooth(span = .95,
                level = .99, 
                color = "white") +
    
    #facet_wrap(~trim, 
    #           nrow = 3) + # Facet by trim level
    
    labs(x = "Odometer (Miles)",
         y = "Price (USD)",
         title = paste(car, 
                       "Valuation Models by Trim Level", 
                       sep = " ")) +
    
    scale_x_continuous(labels = scales::comma) +
    scale_y_continuous(labels = scales::dollar) +
    scale_size_continuous(transform = "log10") +
    scale_color_gradient(low = "white", 
                         high = "#02233F") +
    
    myTheme
  
  # Convert ggplot to plotly
  rp <- ggplotly(p)
  
  return(rp)
  
}


