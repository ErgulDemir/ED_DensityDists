library(shiny)

ui <- fluidPage(

  titlePanel(strong("Probabilty-Density Distributions")),
  hr(),
  
  sidebarPanel(
   
    helpText(p(em("Probability-density function is a function representing 
                   the relative distribution of frequency or probability 
                   of a random variable in a specific classes of intervals."))),
    hr(),

    selectInput("select", h4(strong("Select a distribution and enter the parameters:")),
                c("Normal", "Student t","Snedecor F", "Pearson chi-square", 
                  "Poisson", "Uniform")),
    
    conditionalPanel("input.select == 'Normal'",
      numericInput("mean", "Mean", width = 200, value = 0),
      numericInput("sd", "Std.Dev.", width = 200, value = 1)
    ),

    conditionalPanel("input.select == 'Student t'",
      numericInput("dft", "Degree of freedom", width = 200, value = 20)
    ),

    conditionalPanel("input.select == 'Snedecor F'",
      numericInput("df1", "df1 (Between subject)", width = 200, value = 1),
      numericInput("df2", "df2 (Within Subject)", width = 200, value = 20)
    ),

    conditionalPanel("input.select == 'Pearson chi-square'",
      numericInput("dfk", "Degree of freedom", width = 200, value = 5)
    ),

    conditionalPanel("input.select == 'Poisson'",
      numericInput("lam", "Lambda", width = 200, value = 1)
    ),

    conditionalPanel("input.select == 'Uniform'",
      numericInput("min", "Lower bound", width = 200, value = 0),
      numericInput("max", "Upper bound", width = 200, value = 1),
    ),

    hr(),
    actionButton("go", "Draw", class = "btn-primary")

  ),

  mainPanel(
    plotOutput("plot", width = 750, height = 500),
    hr(),
    imageOutput("image", width = 300, height = 200)
  )

)


server <- function(input, output){

  output$plot <- renderPlot({
    
    input$go
    
    if(input$select == "Normal"){ 

      isolate({
      xv <- rnorm(10000, input$mean, input$sd)
      ym <- max(hist(xv)$density)
      hist(xv, freq = FALSE, ylim = c(0, ym*1.2),
           xlab = "x", main = "Normal distribution")
      curve(dnorm(x, input$mean, input$sd), lwd = 2.5, add = TRUE)
      })

    } else if(input$select == "Student t"){

      isolate({
      xv <- rt(10000, input$dft)
      ym <- max(hist(xv)$density)
      hist(xv, freq = FALSE, ylim = c(0, ym*1.2),
           xlab = "x", main = "Student t distribution")
      curve(dt(x, input$dft), lwd = 2.5, add = TRUE)
      })

    } else if(input$select == "Snedecor F"){

      isolate({
      xv <- rf(10000, input$df1, input$df2)
      ym <- max(hist(xv)$density)
      hist(xv, freq = FALSE, ylim = c(0, ym*1.2),
           xlab = "x", main = "F distribution") 
      curve(df(x, input$df1, input$df2), lwd = 2.5, add = TRUE)
      })

    } else if(input$select == "Pearson chi-square"){

      isolate({
      xv <- rchisq(10000, input$dfk)
      ym <- max(hist(xv)$density)
      hist(xv, freq = FALSE, ylim = c(0, ym*1.2),
           xlab = "x", main = "Chi-Square distribution")
      curve(dchisq(x, input$dfk), lwd = 2.5, add = TRUE)
      })

    } else if(input$select == "Poisson"){

      isolate({
      xv <- rpois(10000, input$lam)
      ym <- max(hist(xv)$density)
      hist(xv, freq = FALSE, ylim = c(0, ym*1.2),
           xlab = "x", main = "Poisson distribution") 
      })

    } else if(input$select == "Uniform"){

      isolate({
      xv <- runif(10000, input$min, input$max)
      ym <- max(hist(xv)$density)
      hist(xv, freq = FALSE, ylim = c(0, ym*1.2),
           xlab = "x", main = "Uniform distribution")
      curve(dunif(x, input$min, input$max), lwd = 2.5, add = TRUE)
      }) 
    } 
  
  })

 output$image <- renderImage({
    
    input$go
    
    if(input$select == "Normal"){ 
      outputArgs = list(src = "www//normal.png")

    } else if(input$select == "Student t"){
      outputArgs = list(src = "www//student.png")


    } else if(input$select == "Snedecor F"){
      outputArgs = list(src ="www//f.png")

    } else if(input$select == "Pearson chi-square"){
      outputArgs = list(src = "www//chi.jpg")

    } else if(input$select == "Poisson"){
      outputArgs = list(src = "www//poisson.png")

    } else if(input$select == "Uniform"){
      outputArgs = list(src = "www//uniform.png")

    } 
  
  }, deleteFile = FALSE)

}


shinyApp(ui, server)

