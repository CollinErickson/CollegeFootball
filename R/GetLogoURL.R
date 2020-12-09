schoolnametologo <- function(school) {
  # paste0("https://i.turner.ncaa.com/sites/default/files/images/logos/schools/bgl/west-virginia.svg")
  school2 <- tolower(gsub(pattern=" ", replacement="-", school, fixed = T))
  school2 <- gsub(pattern="state", replacement="st", x=school2)
  url <- paste0("https://i.turner.ncaa.com/sites/default/files/images/logos/schools/bgl/",
                school2,
                ".svg")
  url <- ifelse(RCurl::url.exists(url), 
                url, 
                # "https://upload.wikimedia.org/wikipedia/commons/thumb/d/dd/NCAA_logo.svg/1042px-NCAA_logo.svg.png" # NCAA logo
                "https://a.espncdn.com/combiner/i?img=/redesign/assets/img/icons/ESPN-icon-football-college.png&w=288&h=288&transparent=true"
  )
  url
}
if (F) {
  schoolnametologo("Notre Dame")
  schoolnametologo("West Virginia")
  schoolnametologo("West Virg")
}