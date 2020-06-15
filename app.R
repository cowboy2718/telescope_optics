# Telescope Optics
# Developed by Tony Gojanovic
# June 15, 2020
# The following shiny program determines basic optical performance characteristics of a telescope.

library(shiny)
ui <- fluidPage(
  titlePanel("Telescope Optics Calculator"),
  sidebarLayout(
    sidebarPanel(
      numericInput('obj_dia', 'Telescope Aperature in mm', 102, min = 0, max = 10000, step = 1),
      numericInput('obj_fl', 'Focal Length in mm',1000,min=0,max=100000,step = 1),
      numericInput('eye_fl','Eyepiece focal length in mm',20,min=0,max=100,step=1), 
      numericInput("eye_fov",'Eyepiece field of view in degrees',50,min = 20,max=100,step=1),
      sliderInput('eye_dia',"Pupil diameter of the observer",7,min=5,max=10,step=1),
      submitButton('Calculate')
    ),
    mainPanel(withMathJax(),
              # Output summary
              h3("Instructions"),
              p("Input the Input the telescope optic parameters. Press calculate and performance characteristics will be calculated"),
              h4("Focal ratio:"),
              verbatimTextOutput("focal_ratio"),
              h4("Magnifcation:"),
              verbatimTextOutput("magnifcation"),
              h4("Field of view (degrees):"),
              verbatimTextOutput("telefov"),
              h4("Resolving power (arcseconds):"),
              verbatimTextOutput("dawes_limit")
              
    )
  )
)

# The following function takes user input to calculate optical performance.

  #Determine the focal ratio
  f_ratio <- function(obj_dia,obj_fl) {
    return(round(obj_fl/obj_dia,1))
  }  
  
  # Determine the magnification
  mag<-function(obj_fl,eye_fl){
    magnification<-obj_fl/eye_fl
    magnification <- round(magnification,1)
    return(magnification)
  }
  
  # Determine the field of view
  fov<-function(obj_fl,eye_fl,eye_fov){
    mag<-obj_fl/eye_fl
    field<-eye_fov/mag
    field<-round(field,2)
    return(field)
  }
  
  # Determine the resolving power (Dawes Limit)
  dawes<-function(obj_dia){
    resolution <- 115.8/obj_dia
    resolution<-round(resolution,2)
    return(resolution)
    
  }
    
  
server <- function(input, output) {
  output$focal_ratio <- renderPrint({f_ratio(input$obj_dia,input$obj_fl)})
  output$magnifcation <- renderPrint({mag(input$obj_fl,input$eye_fl)})
  output$telefov <- renderPrint({fov(input$obj_fl,input$eye_fl,input$eye_fov)})
  output$dawes_limit <- renderPrint({dawes(input$obj_dia)})
}
shinyApp(ui, server)

# Reference for shiny layout
# 