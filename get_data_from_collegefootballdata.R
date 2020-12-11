# # Python code
# import requests
# response = requests.get(
#   "https://api.collegefootballdata.com/games",
#   params={"year": 2019, "seasonType": "both"}
# )


library(httr)
getout <- httr::GET(  "https://api.collegefootballdata.com/games",
            # config=list("year"= 2019, "seasonType"= "both")
            # httr::add_headers("year"= 2019, "seasonType"= "both")
            query=list("year"= 2019, "seasonType"= "both")
)
httr::status_code(getout)
getout
getout$request
# getout$content
# rawToChar(getout$content)
seas <- jsonlite::fromJSON(rawToChar(getout$content))
seas %>% str
