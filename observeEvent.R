library(shiny)

update_response1 <- function(session, responses, city) {
  updateSelectInput(session, "response1",
                    choices = unique(responses$name_date[which(responses$city == city)]),
                    selected = unique(responses$name_date[which(responses$city == city)])[1])
}

update_response2 <- function(session, responses, city, response1) {
  updateSelectInput(session, "response2",
                    choices = c("", unique(responses$name_date[which(responses$city == city &
                                                                responses$name_date != response1)])),
                    selected = "")
}