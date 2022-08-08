### import the libraries ----
library(tidyverse) # for data wrangling.
library(jsonlite) # for parsing json.
library(httr) # for making request.

### geting request ----
## getting the url 
mamanPaz_url <- "https://www.mamanpaz.ir/api/v3/food/search?"

## adding queries
mamanPaz <- GET(url = mamanPaz_url, 
                query = list(page = "1", 
                             size = "7200")) |> # use postman to get size.
    stop_for_status() # sending error type.

## the json parse function for parsing request
json_parse_func <- function(req){
    text = content(req, as = "text", encoding = "UTF-8") # convert to text.
    if(identical(text, "")) warn("No output to Parse.") # if text return empty get warning.
    fromJSON(text) # convert to json.
}

mamanPaz_json <- json_parse_func(mamanPaz) 
mamanPaz_data <- mamanPaz_json$data$content # getting content data.

### getting all comments in one row(separate any comment food by ",") -----
## creating empty tibble for add commenting column
comment_df <- tibble(id_name = "", comment = "")

## for loop: getting all id food names and all comments for on id food
for(i in 1:length(mamanPaz_data$id)){
    
    id = mamanPaz_data$id[i] # getting chefId.
    url_c = paste0("https://www.mamanpaz.ir/api/v3/food/",
                   id ,"/comments?")
    
    mamanPaz_c = GET(url_c, 
                     query = list(
                         page = "1", 
                         size = "40")) |> 
        stop_for_status()
    
    json_content = mamanPaz_c |>
        json_parse_func() 
    
    comment_df = comment_df |> 
        add_row(id_name = id,
                comment = json_content$data$content$comment)
}
 
## merge all comment from one food together in one column(all_comment column)
all_comment <- comment_df[-1, ] |> 
    group_by(id_name) |> 
    mutate(all_comment = paste(comment, collapse = ",")) |> 
    drop_na() |> 
    ungroup() |> 
    distinct(id_name, .keep_all = TRUE) |> 
    select(-comment)

## joining the all comment data frame to mamanPaz data(full data by all comments)
mamanPaz_data <- mamanPaz_data |> 
    left_join(all_comment, by = c("id" = "id_name")) |> 
    unnest(chef, names_repair = "unique") |> 
    select(-c("id...21", "state...26", "scoreAvg...31",
              "scoreCount...30", "otherImageUrls...24",
              "imageUrl...23")) 
