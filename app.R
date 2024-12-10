# Load the modules UI & server
source("R/ui.R")
source("R/server.R")
# Start the Application 
shinyApp(ui = ui, server = server)