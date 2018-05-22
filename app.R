# Simulating Chuck-A-Luck: A Demonstration of the Central Limit Theorem
# Coded by Roger Johnson, South Dakota School of Mines & Technology, 
# roger.johnson@sdsmt.edu; June 5, 2015

ui <- fluidPage(
  
  # Application title
  titlePanel("Playing Chuck-A-Luck"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      helpText("In Chuck-A-Luck the operator will throw three dice. You choose
               a value you think is likely to occur. In a $1 bet, if your
               value comes up three times, you win $3; two times, $2, and one time,  
               $1 (all winnings net). If your value fails to appear, you lose the $1 
               you bet."),
      sliderInput("games","Number of Games Played?", min=1, max=500,
                  value=100, round=TRUE),
      sliderInput("reps","Number of Replications?", min=1, max=10000, sep="",
                  value=5000, round=TRUE),
      selectInput("scale","Vertical Axis Scaling",
                  choices=c("Frequency" = "freq", "Density" = "dens"),
                  multiple = FALSE),
      checkboxGroupInput("fit","Fit by a Normal Curve", choices=c(" " = "norm"),
                         selected=NULL),
      actionButton("button","Run again!")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("FitPlot"),
      tableOutput("stats")
    ) 
  )
)

server <- function(input,output) {
  
  # Chuck-A-Luck probability mass function:
  x <- c(3,2,1,-1)
  p <- c(1/216,15/216,75/216,125/216)
  mu <- sum(x*p)
  variance <- sum(x*x*p) - mu*mu
  sigma <- sqrt(variance)
  
    output$FitPlot <- renderPlot({
      total <- NULL
      # Generate the net winnings in input$reps replications of input$games games:
      for (i in 1:input$reps)
        total[i] <- sum(sample(x,input$games,replace=TRUE,prob=p))
      
      vertical <- isolate({input$scale})
      if ("freq" %in% vertical) frequency <- TRUE else frequency <- FALSE
      
      selection <- isolate({input$fit})
      # Add fitted normal curve
      if ("norm" %in% selection) {
        m <- input$games*mu
        s <- sqrt(input$games)*sigma
        x <- hist(total, freq = frequency,
                  main=bquote(paste("Total Winnings in ",.(input$games)," Bets")),
                  xlab=bquote(paste("Total Net Winnings, ",.(input$reps)," Replications")))
        binwidth <- x$breaks[2] - x$breaks[1]
        if (frequency) scalefactor <- binwidth*input$reps else scalefactor <-1
        curve(dnorm(x,mean=m,sd=s)*scalefactor,add=TRUE)
      } else
      hist(total, freq = frequency,
           main=bquote(paste("Total Winnings in ",.(input$games)," Bets")),
           xlab=bquote(paste("Total Net Winnings, ",.(input$reps)," Replications")))
      
      output$stats <- renderTable({ 
        d <- total[1:input$reps]
        getMat <- matrix(c(
                    "Minimum",format(min(d),nsmall=2),
                    "Maximum",format(max(d),nsmall=2),
                    "Mean",format(round(mean(d),2),nsmall=2),
                    "Std Dev",format(round(sd(d),2),nsmall=2),
                    "Q1",format(round(quantile(d,0.25,type=6,na.rm=TRUE)[[1]],2),nsmall=2),
                    "Q2",format(round(quantile(d,0.50,type=6,na.rm=TRUE)[[1]],2),nsmall=2),
                    "Q3",format(round(quantile(d,0.75,type=6,na.rm=TRUE)[[1]],2),nsmall=2),
                    "Fraction at Least Zero",format(round(sum(d>=0)/input$reps,2),nsmall=2)
                    ), nrow=8, ncol=2, byrow=TRUE)
        colnames(getMat) <- c("Statistic","Value")
        getMat
      })
      
      input$button
      
    })
    
}

shinyApp(ui = ui, server = server)
