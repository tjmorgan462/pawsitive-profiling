library(tidyverse)
library(httr)
library(jsonlite)

base_url <- "https://registry.dog/"
info_url <- "api/v1"
full_url <- paste0(base_url, info_url)

api_request <-  GET(full_url)

api_response <- api_request$status_code
api_content <- api_request$content

raw_content <-  rawToChar(api_content)
api_JSON <- fromJSON(raw_content, flatten = TRUE)

dog_data <- as.data.frame(api_JSON)

dog_data <- dog_data %>% mutate(data.general.personalityTraits = sapply(data.general.personalityTraits, toString))

# write.csv(dog_data, file = "dog_data.csv", row.names = TRUE)
