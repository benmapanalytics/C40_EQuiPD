# C40_EQuiPD: Technical documentation

**Equity in Air Quality Policy and Data Ecosystem - Shiny App**
Currently deployed at: https://dan-benmapanalytics.shinyapps.io/c40_air_quality_health_and_equity_ecosystem/

Steps to setup and publish this Shiny App:

0. Download and open this project in the RStudio program 
   (https://posit.co/download/rstudio-desktop/)
   You will also need R (https://cran.r-project.org/) installed.
   This application was developed using R software version 4.3.3.

1. Edit the 'password' file replacing 'type1toenter' with a password of your
   choosing. This is the password to pass the landing page and use the
   application.

2. Create a Google Sheets file to store results.
   Identify the Google sheet ID: this is the string of text after 
   '.com/spreadsheets/d/' until the next '/'.
   For example: docs.google.com/spreadsheets/d/1XpJqcy-YlFD3wRtsb-kWSG9f6sCQlkSxBXSXnTqjMAw/
   has ID: 1XpJqcy-YlFD3wRtsb-kWSG9f6sCQlkSxBXSXnTqjMAw
   Copy your sheet id into the 'google_sheet_id' document (replacing existing 
   text).

3. Run the following three lines in the R Console
   library(googlesheets4)
   options(gargle_oauth_cache = ".secrets")
   gs4_auth(cache = ".secrets")
   
   You will be directed to a web browser. Log in with an account that has
   access to the Google Sheet specified in step 2 and grant access when
   prompted. Then enter this email into the 'access_email' file (replacing
   existing text).
   
4. Open app.R and click 'Run App' at top of screen to test application.
   
4. Create account on https://shinyapps.io
   Follow instructions online to connect and publish this application
   https://docs.posit.co/shinyapps.io/guide/getting_started/
   Redeploy app with updates using: `rsconnect::deployApp()`