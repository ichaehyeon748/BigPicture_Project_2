library(shiny)

source("pca_core.R")
source("tab_shoe_search.R", local = TRUE)
source("tab_explain_pca.R", local = TRUE)
source("tab_our_shoes.R",   local = TRUE)
source("tab_data1.R",       local = TRUE)
source("tab_ai.R",          local = TRUE)

model <- load_pca_model("pca_model.rds")

ui <- navbarPage(
  title = "Big Picture",
  id = "main_tabs",
  tabPanel("Shoe Search", shoe_search_ui()),
  tabPanel("Explain PCA", explain_pca_ui("explain_pca")),
  tabPanel("Our Shoes",   our_shoes_ui("our_shoes")),
  tabPanel("Data 1",      data1_ui("data1")),
  tabPanel("AI",          ai_ui("ai"))
)

server <- function(input, output, session) {
  photo_info <- shoe_search_server(
    input, output, session, model,
    shoes_dir     = "Raw_Data/shoes",      
    selfcheck_dir = "Raw_Data/selfcheck"   
  )
  
  explain_pca_server("explain_pca", photo_info, model, root_session = session)
  our_shoes_server("our_shoes", NULL, model)
  data1_server("data1", NULL, model)
  ai_server("ai", NULL, model)
}

shinyApp(ui, server)
