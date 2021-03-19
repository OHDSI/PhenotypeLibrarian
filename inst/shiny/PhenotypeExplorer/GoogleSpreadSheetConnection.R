library(googlesheets4)

# AUROTHIZATION
googlesheets4::gs4_auth(path = Sys.getenv('cohortDiagnosticsAuthorizationJson'))

# READING FROM EXISTING spread_sheet
googlesheets4::read_sheet(Sys.getenv('cohortDiagnosticsCommentsGoogleSheets'))

# CREATING NEW spread_sheet, WITH SHEET NAMED FLOWERS, Brackets at the end are necessary to show the sheet ID
(ss <- googlesheets4::gs4_create("cohort-Test", sheets = list(flowers = head(iris))))

# WRITING INTO CREATED spread-sheet NEW SHEET NAMED autos
mtcars %>% 
  googlesheets4::sheet_write(ss, sheet = "autos")
# OUTPUT:
# Spreadsheet name: cohort-Test
# ID: 1N537zCsVBx3ZT3cK4Tn7eNHSJNyI8hW4AP-Xt4Ytdb0
# Locale: en_US
# Time zone: Etc/GMT
# # of sheets: 1

# Reading the  values from spreadsheet
googlesheets4::read_sheet("1O3L-IakLeamhV-sA1U90mtnxWMWOyvzu0Yn3O0sifoM",sheet = "autos")

# Appending rows into google sheet
df <- data.frame("dateTimeEntry" = Sys.time(),"menuItem" = "search", "databaseId" = "123" ,"cohortId" = "abc", "userName" = Sys.info()[['effective_user']],
                 "userPriority" = 0, "commentPriority" = 0, "userAuthenticated" = FALSE, "markdown" = "<h1>Hello</h1>*#BB")
googlesheets4::sheet_append(Sys.getenv('cohortDiagnosticsCommentsGoogleSheets'),data = df)

# Creating the service Account
# ********************************************************************************
# From the Developers Console, in the target GCP Project, go to IAM & Admin > Service accounts.
# Give it a decent name and description.
# For example, the service account used to create the googledrive docs has name “googledrive-docs” and description “Used when generating googledrive documentation”.
# Service account permissions. Whether you need to do anything here depends on the API(s) you are targetting. You can also modify roles later and iteratively sort this out.
# For example, the service account used to create the googledrive docs does not have any explicit roles.
# The service account used to test bigrquery has roles BigQuery Admin and Storage Admin.
# Grant users access to this service account? So far, I have not done this, so feel free to do nothing here. Or if you know this is useful to you, then by all means do so.
# Do Create key and download as JSON. This file is what we mean when we talk about a “service account token” in the documentation of gargle and packages that use gargle.
# Move this JSON to an appropriate place for storing sensitive information. Treat it like a username & password combo! If you have no preference, store service account tokens below ~/.R/gargle/, which also holds the default cache folder for OAuth2 tokens managed by gargle.
# You will notice the downloaded JSON file has an awful name, so I always create a symlink that uses the service account’s name, to make it easier to tell what this file is.

