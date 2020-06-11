1#html note: <p style = 'font-family:Times,garamond,serif;
# font-size:28px;font-style:italic;'>

library(shiny)
source("curlparentheses.R",local = TRUE)
source("str2latex.R", local = TRUE)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                   * {
                    font-family: Comic Sans MS,garamond,serif;
                    font-weight: 500;
                    line-height: 1.1;
                    color: #000000;
                    }
                    "))
  ),
  
  withMathJax(),
  
  # App title ----
  titlePanel(title="Riemann Sum"),
  
  # Sidebar layout ----
  sidebarLayout(
    
    # Sidebar objects ----
    sidebarPanel(
      
      # Input: function ----   
      textInput(inputId = "curve",
                   HTML("<p>Enter the function f(x)</p>"), 
                   value="sin(x)"),
      
      # Input: Select method ----
      selectInput(inputId = "method", 
                  "Select a method",
                  choices = c("Left sum"="left",
                               "Right sum"= "right",
                              "Mid-point sum" = "mid",
                              "Whatever" = "whatever",
                              "Quiz: which method was used?" = "quiz"),
                  selected = "quiz"),
      
      # Input: Select the number of subintervals ----
      numericInput(inputId = "n",
                   "Number of subintervals",
                   value = 5,
                   min = 1,
                   step = 1
      ),
      
      # Input: Select the left endpoint ----
      textInput(inputId = "a","Left endpoint",value ="0"), 
      
      # Input: Select the right endpoint ----
      textInput(inputId = "b","Right endpoint",value ="3*pi"), 
      
      # Input: Checkbox: show sum ----
      checkboxInput(inputId = "calculation", 
                    "Show the numeric sum", FALSE),
      
      # Input: Checkbox: aspect ratio ----
      checkboxInput(inputId = "asp", 
                    "Aspect Ratio = 1", FALSE)
    ), # end of sidebar panel
    
    # Main panel----
    mainPanel(
      
      # Display textblock 1 ----
      htmlOutput(outputId = "textblock1"),
      
      
      # Display regression graph ----
      plotOutput(outputId = "graph1"),
      
      # Display textblock 2 ----
      htmlOutput(outputId = "textblock2")
      
      
    ) #end of mainPanel
    
  ) #end of sidebarlayout
  
) #end of fluidpage

# Server ----
server <- function(input, output){
  ##################################################
  # The integrand ----
  # If function returns FALSE, userdata is NULL. 
  f <- function(x){
    #Replace e by exp(1) to allow e as a number
    fcn <- gsub("e","exp(1)",input$curve)
    fcn <- gsub("ln","log",fcn)
    #y <- eval(parse(text=fcn)) + x*0
    y <- tryCatch(eval(parse(text=fcn)) + x*0, error=function(e){FALSE})
    return(y)
  }
  
  ##################################################
  #Reactive values ----
  values <- reactiveValues()
  observe(values$a <- tryCatch(eval(parse(text=input$a)), error=function(e){0}))
  observe(values$b <- tryCatch(eval(parse(text=input$b)), error=function(e){0}))
  
  ##################################################
  # Calculations ----
  userdata <- reactive({
    req(input$a, input$b, input$n, input$curve)
    
    a <- values$a
    b <- values$b
    if (a>=b | isFALSE(f(a))) {return(NULL)}
    else
    {
    n <- input$n
    dx <- (b - a) / n # step size
    div <- a + (0:n) * dx # All division points
    xleft <- div[1:n]     # All lower left x-coordinates
    ybottom <- rep(0,n)   # All lower left y-coordinates
    xright <- div[2:(n+1)] # All upper right x-coordinates
    if(input$method == "right") x <- xright       # Right sum
    else if(input$method == "left") x <- xleft # Left sum
    else if(input$method == "mid") x <- xleft+dx/2 # Midpoint sum
    else if(input$method == "whatever") x <- runif(n, xleft, xright) # Random sum
    else if(input$method == "quiz") {
      toss <- sample.int(4,1)
      if(toss == 1) x <- xleft
      else if(toss == 2) x <- xright
      else if(toss == 3) x <- xleft + dx/2
      else x <- runif(n, xleft, xright)
    }
    else stop('Please choose one of the following methods:
              "l", "r", "m" or "rand".')
    ytop <- f(x) # Heights
    area <<- sum(ytop)*dx # Area
    
    #ymin <- min(0,min(ytop))
    #ymax <- max(0, max(ytop,f(b)) )
    df <- data.frame(xleft,ybottom,xright,ytop,x)
    colnames(df) <- c("xleft","ybottom","xright","ytop","")
    return(df)
    }#END OF ELSE
  })
  
  ##################################################
  # Output: Textblock 1 ----
  output$textblock1 <- renderUI({
    if (is.null(input$curve) || input$curve == ""){
      HTML("Please type in a curve.")
    }
      else {
       
      withMathJax(
        HTML(
          paste(
            "<center><p style='font-size:28px'>",
            "\\( f(x) =", str2latex(input$curve),
            "\\)</p></center><br>"
            ) # END OF PASTE
        )# END OF HTML
      ) # END OF MATHJAX
    }# END OF ELSE
  }) # END OF TEXTBLOCK 1
  
  ##################################################
  # Output: graph 1----
  output$graph1 <- renderPlot(
    
    if(is.null(userdata())){return(NULL)}
    else {

    # extract data from reactive
    xyxyx <- as.data.frame(userdata())
    
    # Plot the curve
    # [option: asp = 1]
    curve(f(x),from = values$a, to = values$b, 
          xlim=c(values$a,values$b),
          lwd=2,
          ylab=expression(f(x)),
          asp = as.numeric(input$asp)
          )


    #Plot the choice of points if n is less than 10
    if(input$n<=10 & input$method != "quiz"){
      matlines(matrix(c(xyxyx[,5],xyxyx[,5]),ncol=input$n,byrow=TRUE),
               matrix(c(rep(0,input$n),xyxyx[,4]),ncol=input$n,byrow=TRUE),
               lty=2,lwd=3,col="blue")          # Dotted lines
      points(xyxyx[,5],rep(0,input$n),col="blue",pch=20)
    }
    
    # Plot the rectangular boxes 
    # rect(xleft,ybottom,xright,ytop, ...)
    # rect(xyxyx[,1],xyxyx[,2],xyxyx[,3],xyxyx[,4],
    #      density = 20, angle=45, col="red")
    red <- subset(xyxyx, ytop>=0, 1:4)
    blue <- subset(xyxyx, ytop<0, 1:4)
    par(new=TRUE)
    if(dim(blue)[1]>0){
      rect(blue[,1],blue[,2], blue[,3], blue[,4],
           density = 20, angle = 45, col = "blue")
    }
    if (dim(red)[1]>0){
      rect(red[,1],red[,2], red[,3], red[,4],
           density = 20, angle = 45, col = "red")
    }
    
  })#end renderplot
  
  ##################################################
  # Output: Textblock 2 ----
  output$textblock2 <- renderUI({
    
    #Run the reactive function
    userdata()
    if (input$calculation == TRUE){
      withMathJax(paste("$$\\sum_{k=1}^{",input$n,"}
                        f(x^\\ast_k)\\Delta x
                        =",sprintf("%.4f",area),
                        "$$")
        ) # END OF MATHJAX
    }# END OF IF
    else return(NULL)
  }) # END OF TEXTBLOCK 2
  
} #end server

shinyApp(ui = ui, server = server)
