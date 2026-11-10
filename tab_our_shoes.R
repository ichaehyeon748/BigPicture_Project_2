our_shoes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Our Shoes"),
    p("준비 중입니다.")
  )
}

our_shoes_server <- function(id, shared, model) {
  moduleServer(id, function(input, output, session) {
    # 나중에 여기에 로직이 들어갑니다
  })
}