library(shiny)
ui <- fluidPage(
  sidebarPanel(
    sliderInput("w.fg", "Field Goal % Weight",
                min = 0, max = 20, step = .5, value = 10),
    sliderInput("w.ft", "Free Throw % Weight",
                min = 0, max = 20, step = .5, value = 10),
    sliderInput("w.3pm", "Three Point % Weight",
                min = 0, max = 20, step = .5, value = 10),
    sliderInput("w.reb", "Rebound Weight",
                min = 0, max = 20, step = .5, value = 10),
    sliderInput("w.ast", "Assist Weight",
                min = 0, max = 20, step = .5, value = 10),
    sliderInput("w.stl", "Steal Weight",
                min = 0, max = 20, step = .5, value = 10),
    sliderInput("w.blk", "Block Weight",
                min = 0, max = 20, step = .5, value = 10),
    sliderInput("w.pts", "Points Weight",
                min = 0, max = 20, step = .5, value = 10)
  )
)

server <- function(input, output) {
  weight.server <- c(input$w.fg, input$w.ft, input$w.3pm, input$w.reb, input$w.ast, input$w.stl, input$w.blk, input$w.pts)
  fbb.data$ovr.r.custom <- rowSums(fbb.data[,which(names(fbb.data) == "fg.r"):which(names(fbb.data) == "pts.r")] * weight.server)
  fbb.data$cost.custom <- fbb.data$ovr.r.custom/sum(fbb.data$ovr.r.custom[1:(num.teams*rost.size)])*((doll.per - doll.min)*num.teams)
  
}

shinyApp(ui = ui, server = server)