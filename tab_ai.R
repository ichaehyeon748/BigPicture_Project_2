ai_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("AI"),
    p("준비 중입니다.")
  )
}

ai_server <- function(id, shared, model) {
  moduleServer(id, function(input, output, session) {
    # 나중에 여기에 로직이 들어갑니다
  })
}