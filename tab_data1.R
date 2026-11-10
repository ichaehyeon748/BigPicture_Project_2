data1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Data 1"),
    p("준비 중입니다.")
  )
}

data1_server <- function(id, shared, model) {
  moduleServer(id, function(input, output, session) {
    # 나중에 여기에 로직이 들어갑니다
  })
}