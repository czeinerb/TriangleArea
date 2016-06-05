library(shiny)

shinyUI(
    fluidPage(
        sidebarLayout(
        sidebarPanel(
            h3("Area of a triangle")
            , numericInput("ax", "X coordinate of 1st corner :", value = 0)
            , numericInput("ay", "Y coordinate of 1st corner :", value = 5)
            , numericInput("bx", "X coordinate of 2nd corner :", value = 2)
            , numericInput("by", "Y coordinate of 2nd corner :", value = 0)
            , numericInput("cx", "X coordinate of 3rd corner :", value = 6)
            , numericInput("cy", "Y coordinate of 3rd corner :", value = 8)
            , sliderInput("gran", "Granularity (G):",
                        min = 5, max = 100, value = 10, step = 1)
            , p(strong(em("Documentation:",a("Calculating the area of a triangle.",href="README.html"))))
        ),
    mainPanel(
        h4("Instructions: ")
        , p("Enter or amend the coordinates of the triangle and adjust the granularity
            , which determines the number of data points for the the approximation. (Number of data points are Granularity^2)")
        , h5("Note:")
        , helpText("> If the triangle is not fully in the 1st qudrant then it is transposed there.")
        , helpText("> Calculations can take a few seconds with large number of data points.")
        , helpText("> Dots inside the triable are coloured green while outside is blue.")
        , br()
        , h4("Plot of the triangle with 'measurement' dots:")
        , plotOutput("triangleWithDotsPlot")
        , h3("Transposed coordinates:")
        , h5(textOutput("coordsTransp"))
        , h3("Area of a triangle calculated")
        , h5("  ..with with Heron's formula:")
        , h4(textOutput("areaHeron"))
        , h5("  ..with geometric approximation:")
        , h4(textOutput("areaGeom"))
        , br()
    )
    )
))
