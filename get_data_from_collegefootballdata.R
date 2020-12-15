# # Python code
# import requests
# response = requests.get(
#   "https://api.collegefootballdata.com/games",
#   params={"year": 2019, "seasonType": "both"}
# )

if (F) {
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
}

update_data_year <- function(year) {
  getout <- httr::GET(  "https://api.collegefootballdata.com/games",
                        # config=list("year"= 2019, "seasonType"= "both")
                        # httr::add_headers("year"= 2019, "seasonType"= "both")
                        query=list("year"= year, "seasonType"= "both")
  )
  stopifnot(httr::status_code(getout) < 400)
  cat("downloaded file successfully\n")
  seas <- jsonlite::fromJSON(rawToChar(getout$content))
  seas$`away_line_scores[0]` <- sapply(seas$away_line_scores, function(x){x[1]})
  seas$`away_line_scores[1]` <- sapply(seas$away_line_scores, function(x){x[2]})
  seas$`away_line_scores[2]` <- sapply(seas$away_line_scores, function(x){x[3]})
  seas$`away_line_scores[3]` <- sapply(seas$away_line_scores, function(x){x[4]})
  seas$`home_line_scores[0]` <- sapply(seas$home_line_scores, function(x){x[1]})
  seas$`home_line_scores[1]` <- sapply(seas$home_line_scores, function(x){x[2]})
  seas$`home_line_scores[2]` <- sapply(seas$home_line_scores, function(x){x[3]})
  seas$`home_line_scores[3]` <- sapply(seas$home_line_scores, function(x){x[4]})
  
  seas$away_line_scores <- NULL
  seas$home_line_scores <- NULL
  seas
  readr::write_csv(x=seas, file=paste0("./data/GameResults/GameResults", year, ".csv"))
  cat("saved file successfully\n")
  return(invisible())
}
if (F) {
  update_data_year(2017)
}