library(readxl)
library(shinyalert)

# Function to process EQuiPD xlsx file
process_file <- function(path, file, rec, org, responses) {
  file <- file[which(file$Response != "Score (0-3)"), ]
  rec <- rec[which(rec$Response != "Score" & !is.na(rec$Response)), ]

  # Get city
  city <- file[["Response"]][which(file[["short_q"]] == "City")]
  if (city == "Not listed")
    city <- file[["Response"]][which(file[["short_q"]] == "Other_city")]
  if (city == "") {
    shinyalert("Unsuccessful", "You must input a city name attached to your assessment.", "error")
    return("City name not selected")
  }
  
  
  # Get name and date
  assessment_date <- file[["Response"]][which(file[["short_q"]] == "Date")]
  if (length(assessment_date) == 0 || assessment_date == "")
    assessment_date <- format(Sys.time(), "%m/%d/%y")
  name_date <- file[["Response"]][which(file[["short_q"]] == "Date")]
  upload_date <- format(Sys.time(), "%b %d %Y %H:%M %Z")
  filedate <- format(Sys.time(), "%m%d%y-%H%M")
  version <- file[["EQuiPD"]][1]
  filename <- paste0(city, filedate)
  
  
  # Check for similar city/name/date combo
  if (nrow(responses[which(responses$city == city & 
                           responses$name_date == name_date),]) > 0) {
    shinyalert("Unsuccessful", "A file with the same assessment date has be previously uploaded.
               Consider changing the 'Date completed' input in the excel file or
               adding an additional identifier, e.g. '1/18/2026 v2'.", "error")
    return("Duplicated response")
  }
  
  
  # Share?
  share <- file[["Response"]][which(file[["short_q"]] == "Share")]
  if (share == "No") {
    shinyalert("Unsuccessful", "'Are you willing to share your results with other cities' must
               be set to 'Yes'.", "error")
    return("Must allow sharing")
  }
  
  
  # Gather data
  dat <- data.frame()
  for (c in org$Abbreviation[which(org$Organization == "Category")]) {
    
    # Calc category score
    score <- NA
    idx <- which(file[[c]] == "1")
    if (length(idx) > 0)
      score <- mean(as.numeric(file[["Response"]][idx]), na.rm = TRUE)
    dat <-
      rbind(dat, data.frame(score = score,
                            n_questions = length(idx),
                            category = org$Full_Name[which(org$Abbreviation == c)],
                            domain = NA))
    
    
    for (d in org$Abbreviation[which(org$Organization == "Domain" |
                                     org$Organization == "Equity Theme")]) {
      
        # Calc category/domain score
        score <- NA
        idx <- which(file[[c]] == "1" & file[[d]] == "1")
        if (length(idx) > 0)
          score <- mean(as.numeric(file[["Response"]][idx]), na.rm = TRUE)
        dat <-
          rbind(dat, data.frame(score = score,
                                n_questions = length(idx),
                                category = org$Full_Name[which(org$Abbreviation == c)],
                                domain = org$Full_Name[which(org$Abbreviation == d)]))
    } # end loop over domain/theme
  } # end loop over category
  dat$city = city
  dat$name_date = name_date
  dat$share = share
  dat$filename = filename
  dat$version = version
  
  
  
  # Save file overview
  sheet_id <- as.character(read.csv("google_sheet_id", header = F)[1])
  sheet_append(sheet_id, dat, sheet = "Summary")
  
  
  
  # Store response data
  file <- merge(file, select(rec, "short_q", "Recommendations"), 
                by = "short_q", all.x = TRUE)
  qs <- which(file$DS == 1 | file$CB == 1 | file$PS == 1 | file$IM == 1 | 
                file$AP == 1 | file$SE == 1)
  sheet_add(sheet_id, filename)
  sheet_write(file[qs, ], sheet_id, filename)
  
  return(TRUE)
}
