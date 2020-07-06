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
      sliderInput('pupil_dia',"Pupil diameter of the observer",7,min=4,max=8,step=0.5),
      submitButton('Calculate')
    ),
    mainPanel(withMathJax(),
              # Output summary
              h3("Instructions"),
              p("Input the Input the telescope optic parameters. Press calculate and performance characteristics will be calculated."),
              strong("Note"),
              p("Telescope performance is highly dependent on seeing and transparency conditions."),
              h3("Eyepiece performance"),
              h4("Magnifcation:"),
              verbatimTextOutput("magnifcation"),
              h4("Field of view (degrees):"),
              verbatimTextOutput("telefov"),
              h4("Exit pupil diameter mm:"),
              verbatimTextOutput("exit_pup"),
              h4("Lowest useful magnification based on observer's pupil diameter:"),
              verbatimTextOutput("min_mag"),
              h3("Telescope performance"),
              h4("Focal ratio:"),
              verbatimTextOutput("focal_ratio"),
              h4("Theoretical Resolving power (Dawes limit in arcseconds):"),
              verbatimTextOutput("dawes_limit"),
              h4("Gathering power (light grasp) compared to human eye for a given pupil diameter:"),
              verbatimTextOutput("GL"),
              h4("Approximate Limiting magnitude (under dark, moonless skies):"),
              verbatimTextOutput("limit_mag"),

              h4("Notes"),
              strong("Suggested pupil diameters by age"),
              tags$ul(
                tags$li("20 years or less = 7.5 mm"),
                tags$li("30 years = 7.0 mm"),
                tags$li("35 years = 6.5 mm"),
                tags$li("45 years = 6.0 mm"),
                tags$li("60 years = 5.5 mm"),
                tags$li("80 years = 5.0 mm")
              ),
              tags$a(href="http://www.rocketmime.com/astronomy/Telescope/telescope_eqn.html", "Click here for telescope math equations")
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
    return(round(magnification,1))
  }
  
  # Determine the field of view
  fov<-function(obj_fl,eye_fl,eye_fov){
    mag<-obj_fl/eye_fl
    field<-eye_fov/mag
    return(round(field,2))
  }
  
  # Determine the resolving power (Dawes Limit)
  dawes<-function(obj_dia){
    resolution <- 115.8/obj_dia
    return(round(resolution,2))
  }
  
  # Determine the light graph or gathering power
  light_grasp<-function(obj_dia,pupil_dia){
    gather <- (obj_dia/pupil_dia)^2
    return(round(gather,1))
  } 
  
  # Determine limiting magnitude
  lim_mag<-function(obj_dia){
    limiting_mag<-2+5*(log10(obj_dia))
    return(round(limiting_mag,2))
  }
  
  # Determine exit pupil
  ep <- function(obj_dia,eye_fl,obj_fl){
    exit_pupil <- eye_fl/(obj_fl/obj_dia)
    return(round(exit_pupil,1))
  }
  
  # Determine minimum magnification
  min_val <- function(obj_dia,pupil_dia){
    min_magnification <- obj_dia/pupil_dia
    return(round(min_magnification,1))
    
  }
    
server <- function(input, output) {
  output$focal_ratio <- renderPrint({f_ratio(input$obj_dia,input$obj_fl)})
  output$magnifcation <- renderPrint({mag(input$obj_fl,input$eye_fl)})
  output$telefov <- renderPrint({fov(input$obj_fl,input$eye_fl,input$eye_fov)})
  output$dawes_limit <- renderPrint({dawes(input$obj_dia)})
  output$GL <- renderPrint({light_grasp(input$obj_dia,input$pupil_dia)})
  output$limit_mag <- renderPrint({lim_mag(input$obj_dia)})
  output$exit_pup <- renderPrint({ep(input$obj_dia,input$eye_fl,input$obj_fl)})
  output$min_mag <- renderPrint({min_val(input$obj_dia,input$pupil_dia)})
}
shinyApp(ui, server)

# Reference for shiny layout
# 