library(shiny)
library(tidyverse)
library(tidyr)
library(sf)
library(leaflet)
library(lubridate)
library(jsonlite)
library(aws.s3)
library(ndjson)
library(dplyr)

txt <- ndjson::stream_in("../id_content.json")
txt <- txt %>% select(-10) 
txt2 <- txt %>% mutate(
  place = coalesce(place, place.content)
)

write_csv(txt2, "olddata.csv")

Sys.setenv(
  "AWS_DEFAULT_REGION" = "us-west-2"
)

get_bucket(object, max = Inf)
bucket <- "smithsonian-open-access"
prefix <- "metadata/edan/sil/"
desired_cols <- c("id", "title")

files <- get_bucket(bucket = bucket, prefix = prefix)

keys <- sapply(files, function(x) x[["Key"]])

all_data <- lapply(keys, function(key) {
  message("Reading: ", key)
  tryCatch({
    s3read_using(
      FUN = function(con) {
        stream_in(con) %>% 
          select(any_of(desired_cols))
        },
      object = key,
      bucket = bucket)
  }, error = function(e) {
    warning("Failed to read ", key)
    NULL
  })
})

write_csv(combined, "all_data.csv")

