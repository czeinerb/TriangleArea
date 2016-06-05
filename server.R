# server.R

library(shiny)

# set default values
x <- c(0,2,6,0) 
y <- c(5,0,8,5)

normalise.triangle <- function(x, y) {
    # When a coordinate is mising then it is set to zero
    x[is.na(x)] <- 0
    y[is.na(y)] <- 0

    # if the polygon is not fully in the 1st qudrant then transpose
    x <- x - min(x)
    y <- y - min(y)
    
    # If the polygon is not closed -> close it
    isclosed <- ifelse(x[length(x)] == x[1],T,F)
    isclosed <- isclosed & ifelse(y[length(y)] == y[1],T,F)
    isclosed
    x <- c(x, if(!isclosed) {x[1]})
    y <- c(y, if(!isclosed) {y[1]})
    
    # write coordinates to a list to had it over to the plot functions
    m <- cbind(x, y)
    
    # debug
    print(m)

    return(m)
    }



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   

    output$triangleWithDotsPlot <- renderPlot({
        x <- c(input$ax, input$bx, input$cx, input$ax)
        y <- c(input$ay, input$by, input$cy, input$ay)            
        
        # prepare, transpose coordinates
        m <- normalise.triangle(x, y)
        
        ## Create data points
        gran <- input$gran + 1 #granurality
        if (gran < 2) {gran <- 2}
        print(gran)
        
        x <- m[,1]; y <- m[,2]
        maxx <- max(x); maxy <- max(y)
        scaleX <- maxx/gran 
        scaleY <- maxy/gran 
        pX <- seq(0, maxx, scaleX); #pX <- pX[c(-1,-length(pX))]
        pY <- seq(0, maxy, scaleY); #pY <- pY[c(-1,-length(pY))]
        
        grid <- data.frame(x = as.numeric(), y = as.numeric())
        for (i in (1:length(pX))) {
            for (j in (1:length(pY))) {
                grid0 <- c(x = pX[i], y = pY[j])
                grid <- rbind.data.frame(grid, grid0)
            }
        }
        names(grid) <- c("x","y")
        
        ## PLot the triangle with the coloured gridpoints
        plot(m, type="b")
        dots.IN <- 0
        for (k in (1:dim(grid)[1])) {
            side <- NULL
            ## Point inside or outside of polygon?
            for (line in (1:(length(x)-1))) {
                s0 <- ((x[line+1] - x[line]) * (grid[k,2] - y[line]) 
                       - (y[line+1] - y[line]) * (grid[k,1] - x[line]))
                side <- c(side, s0)
            }
            IN_OUT <- ifelse(sum(abs(side)) == abs(sum(side))
                             , "green", "blue") # "in", "out"
            if (IN_OUT == "green") {dots.IN <- dots.IN +1}
            # Draw plots with the relevant colour
            points(grid[k,1], grid[k,2], pch = 19, col= IN_OUT)    
        }
        

    })

    output$coordsTransp <- renderText({
        x <- c(input$ax, input$bx, input$cx);
        y <- c(input$ay, input$by, input$cy);           
        
        # prepare, transpose coordinates
        m <- normalise.triangle(x, y);
                
        paste("1st corner: ", m[1,1], ", ", m[1,2]
              , "; 2nd corner: ", m[2,1], ", ", m[2,2]
        , "; 3rd corner: ", m[3,1], ", ", m[3,2])
    })

    output$areaHeron <- renderText({
        x <- c(input$ax, input$bx, input$cx, input$ax);
        y <- c(input$ay, input$by, input$cy, input$ay);           
        
        # prepare, transpose coordinates
        m <- normalise.triangle(x, y);
        x <- m[,1]; y <- m[,2];
        
        ## Triagle area 
        AreaH <- round((abs(x[1]*y[2] + x[2]*y[3] + x[3]*y[1] 
                            - x[1]*y[3] - x[2]*y[1] - x[3]*y[2]) / 2), 2)
        
        paste(as.character(AreaH), "squared units")
    })
    
    output$areaGeom <- renderText({
        x <- c(input$ax, input$bx, input$cx, input$ax)
        y <- c(input$ay, input$by, input$cy, input$ay)            
        
        # prepare, transpose coordinates
        m <- normalise.triangle(x, y)
        
        ## Create data points
        gran <- input$gran + 1 #granurality
        if (gran < 2) {gran <- 2}
        print(gran)
        
        x <- m[,1]; y <- m[,2]
        maxx <- max(x); maxy <- max(y)
        scaleX <- maxx/gran 
        scaleY <- maxy/gran 
        pX <- seq(0, maxx, scaleX); #pX <- pX[c(-1,-length(pX))]
        pY <- seq(0, maxy, scaleY); #pY <- pY[c(-1,-length(pY))]
        
        grid <- data.frame(x = as.numeric(), y = as.numeric())
        for (i in (1:length(pX))) {
            for (j in (1:length(pY))) {
                grid0 <- c(x = pX[i], y = pY[j])
                grid <- rbind.data.frame(grid, grid0)
            }
        }
        names(grid) <- c("x","y")
        
        ## PLot the triangle with the coloured gridpoints
        plot(m, type="b")
        dots.IN <- 0
        for (k in (1:dim(grid)[1])) {
            side <- NULL
            ## Point inside or outside of polygon?
            for (line in (1:(length(x)-1))) {
                s0 <- ((x[line+1] - x[line]) * (grid[k,2] - y[line]) 
                       - (y[line+1] - y[line]) * (grid[k,1] - x[line]))
                side <- c(side, s0)
            }
            IN_OUT <- ifelse(sum(abs(side)) == abs(sum(side))
                             , "green", "blue") # "in", "out"
            if (IN_OUT == "green") {dots.IN <- dots.IN +1}
            # Draw plots with the relevant colour
            points(grid[k,1], grid[k,2], pch = 19, col= IN_OUT)    
        }
        
        ## Triagle area 
        AreaApprox <- round((dots.IN * (max(x) * max(y)) / dim(grid)[1]), 2)
        
        paste(as.character(AreaApprox), "squared units")
    })

})

