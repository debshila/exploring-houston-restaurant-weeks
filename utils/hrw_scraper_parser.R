# Exploring Houston Restaurant Weeks, 2019 --------------------------------
# Choosing restaurants to
# Get necessary packages
library(tidyverse)
library(rvest)
library(jsonlite)
library(stringr)
library(leaflet)

# Scrape HRW page ---------------------------------------------------------
url <- "https://www.houstonrestaurantweeks.com/"
hrw_pg <- read_html(url)

#Using CSS selectors to scrape the restaurants
restaurant_html <- html_nodes(hrw_pg,'li a')

# Get all restaurant urls
urls <- restaurant_html %>%
    xml_attr("data-href")
urls <- urls[!is.na(urls)]

restaurant_names <- restaurant_html %>%
    html_text() %>%
    trimws(.)

restaurant_names <- restaurant_names[13:length(restaurant_names)]

restaurant_coord <- bind_rows(lapply(13:length(restaurant_html), function(idx){
    url <- restaurant_html[[idx]] %>%
        xml_attr("data-href")
    coordinates <- str_extract(restaurant_html[[idx]], pattern = "-\\d+\\.\\d+, \\d+\\.\\d+")
    data.frame(url, coordinates, stringsAsFactors = FALSE) %>%
        mutate(coordinates = gsub("\\s", "", coordinates)) %>%
        separate(coordinates, into = c("longitude", "latitude"), sep = ",") %>%
        mutate(latitude = as.numeric(latitude),
               longitude = as.numeric(longitude),
               name = gsub("https://www.houstonrestaurantweeks.com/restaurants/|/$","",url))
}))


# Parse single restaurant -------------------------------------------------------

course_idx_names <- paste0(c("first", "second", "third", "fourth"), collapse = "|")

parse_menu <- function(menu_txt) {
    # Split by course
    split_meal <- strsplit(menu_txt, split = "course")
    curbside_or_takeout <- grepl("curbside|take-out",split_meal[[1]])
    restaurant_info <-
        trimws(unlist(strsplit(split_meal[[length(split_meal)]], "\n")))

    restaurant_info <- restaurant_info[restaurant_info != ""]

    address <- grep(" TX| Texas", restaurant_info, value = TRUE)
    phone <- grep("^\\(\\d{3}\\)", restaurant_info, value = TRUE)
    walk_ins <-
        any(grepl("Walk-Ins welcome", restaurant_info, ignore.case = TRUE))
    reservations <-
        grep("reservations are",
             restaurant_info,
             ignore.case = TRUE,
             value = TRUE)

    split_course <-
        bind_rows(lapply(1:(length(split_meal) - 1), function(meal_idx) {
            meal <- split_meal[[meal_idx]]
            split_by_course <- strsplit(meal, split = "Course")
            tags <- trimws(unlist(strsplit(split_by_course[[1]][1], "\n")))
            tags <- tags[(tags != "") &
                             !grepl(course_idx_names, tags, ignore.case = TRUE)]
            # weekday_only <-
            #     grepl("Please check with the restaurant about HRW \\w service on weekends|Monday through Friday",
            #              meal,
            #              ignore.case = TRUE)

            cost <-as.numeric(trimws(gsub(
                    " per person|\\$", "", tags[grepl("^\\$", tags)], ignore.case = TRUE
                )))
            cuisine_desc <- paste0(tags[grepl("\\(", tags)], collapse = "_")
            cuisine_desc <-
                trimws(
                    gsub(
                        "Restaurant will donate \\$\\d to the Houston Food Bank from each lunch sold. Food Bank will provide 9 meals from this donation.",
                        "",
                        cuisine_desc
                    )
                )
             split_by_item <- #if(length(split_by_course) == 2){
            #     course <- split_by_course[[2]]
            #     items <- trimws(unlist(strsplit(course, split = "\n")))
            #     items <- items[(items != "") ]
            #     course_idxs <- grep(course_idx_names, items, ignore.case = TRUE)
            #     lapply(1:length(course_idxs), function(course_idx){
            #         tmp <- items[(course_idxs[course_idx]+1):(course_idxs[course_idx + 1] -1)]
            #         items <- tmp[seq(1, length(tmp), by = 2)]
            #         description <- tmp[seq(2, length(tmp), by = 2)]
            #         descrip <- ifelse((length(description) < length(item)),
            #                c(description, rep("NA", length(item) - length(description))),
            #                description)
            #         df <- data.frame(matrix(NA, nrow = 1:length(items), ncol = 6))
            #         df <- data.frame(meal_idx,
            #                    course_num = course_idx,
            #                    item,
            #                    cost,
            #                    cuisine_desc,
            #                    description,
            #                    stringsAsFactors = FALSE,
            #                    check.names = TRUE)
            #
            #     })
            #
            #
            #
            #
            # } else {
                bind_rows(lapply(2:length(split_by_course[[1]]), function(course_idx) {
                    course <- split_by_course[[1]][course_idx]
                    items <- trimws(unlist(strsplit(course, split = "\n")))
                    items <- items[(items != "") &
                                       !grepl(course_idx_names, items, ignore.case = TRUE)]

                    # print(course_idx)
                    item_name_idx <- grep("^[[:upper:]]+|\\s$", items)
                    # item_name_idx <- grep("first|second|third|fourth", items)
                    item_description_idx <-   item_name_idx + 1
                    menu <- data.frame(meal_idx,
                        course_num = course_idx - 1,
                        item = items[item_name_idx],
                        cost, cuisine_desc,
                        description = items[item_description_idx],
                        curbside_or_takeout,
                        # weekday_only,
                        stringsAsFactors = FALSE,
                        check.names = TRUE)
                    return(menu)
                }))

            # }
         }))

    data.frame(split_course, address, phone, walk_ins, reservations)
}



# Scrape and parse individual restaurant menus -----------------------------
# all_menu_text <- lapply(unique(urls), function(url){
#     tryCatch({
#         txt <- url %>%
#             read_html() %>%
#             html_nodes("div .menuLeft .menuBox .tab_content") %>%
#             html_text()
#     },
#     error = function(e){})
# })
#
# all_menus <- bind_rows(lapply(all_menu_text, function(menu){
#     # print(url)
#     tryCatch({
#         parse_menu(menu)
#     },
#     error = function(e){})
# }))

all_menus <- bind_rows(
    lapply(restaurant_coord$url, function(url){
    # print(url)
        tryCatch({
            txt <- url %>%
                read_html() %>%
                html_nodes("div .menuLeft .menuBox .tab_content") %>%
                html_text()

            data.frame(url, parse_menu(txt), stringsAsFactors = FALSE)
        },
        error = function(e){})
}))

urls_left <- urls[!(urls %in% unique(all_menus$url))]
urls_left <- urls_left[!is.na(urls_left)]

# Max number of restaurant tags
# max(unlist(lapply(unique(all_menus$cuisine_desc), function(tag){
#     length(unlist(strsplit(tag, "_")))
# })))

all_menus <- all_menus %>%
    left_join(restaurant_coord, by = "url") %>%
    group_by(url) %>%
    mutate(meal = if_else(cost == 45, "Dinner", "lunch/brunch")) %>%
    rename(restaurant_url = url)

# all_menus %>%
#     write_csv("~/Box Sync/hrw/hrw_menus.csv")

all_menus %>%
    write_csv("./hrw_menus_2020.csv")


offerings <- all_menus %>%
    group_by(name, latitude, longitude) %>%
    summarise(meals = n_distinct(meal_idx),
              course = n_distinct(course_num),
              items = n_distinct(item)) %>%
    arrange(desc(items))

offerings_by_meal <- all_menus %>%
    group_by(name, meal_idx) %>%
    summarise(course = n_distinct(course_num),
              items = n_distinct(item)) %>%
    arrange(desc(items), name)

lunch_br <- offerings %>%
    filter(course > 1)


library(yelpr)
key <- readLines("~/Box Sync/hrw/yelp_api_key.txt")


library(httr)
library(purrr)

res <- POST("https://api.yelp.com/oauth2/token",
            body = list(grant_type = "client_credentials",
                        client_id = Sys.getenv(key[1]),
                        client_secret = Sys.getenv(key[2])))
token <- content(res)$access_token

yelp <- "https://api.yelp.com"

term <- "coffee"
location <- "Vancouver, BC"
limit <- 3
(url <-
        modify_url(yelp, path = c("v3", "businesses", "search"),
                   query = list(term = term, location = location, limit = limit)))
res <- GET(url, add_headers('Authorization' = paste("bearer", token)))
http_status(res)

yelp <- "https://api.yelp.com"
term <- "sports"
location <- "Philadelphia, PA"
categories <- NULL
limit <- 5
radius <- 8000
url <- modify_url(yelp, path = c("v3", "businesses", "search"),
                  query = list(term = term, location = location,
                               limit = limit,
                               radius = radius))
res <- GET(url, add_headers('Authorization' = paste("bearer", token)))

results <- content(res)

access_token <- get_access_token(client_id = key[1], client_secret = key[2])
response <- POST("https://api.yelp.com/oauth2/token",
                 query = list(grant_type = "client_credentials",
                              client_id = key[1],
                              client_secret = key[2]),
                 encode = "json")
stop_for_status(response)
token <- content(response, as = "parsed")
token$access_token
yelp::business_search(latitude = offerings$latitude[1], longitude = offerings$longitude[1])


tmp <- business_search(api_key = key,
                    location = "Houston",
                    latitude = offerings$latitude[1],
                    longitude = offerings$longitude[1],
                    term = "houston restaurant weeks",
                    limit = 1)

business_search_review(api_key = key,
                unique(all_menus$name)[3])
