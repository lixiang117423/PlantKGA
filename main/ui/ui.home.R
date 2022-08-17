tabPanel(
  icon("home"),
  fluidRow(
    column(tags$img(src = "Antioquia.png", width = "200px", height = "260px"), width = 2),
    column(

      br(),
      p("Through this application, it is intended to develop a learning environment for anyone who is starting in the study of statistical modeling, 
                                          specifically linear regression through the method of ordinary least squares. 
                                          In any case, we will focus on the procedure (graphics, interpretations, hypotheses, etc.) and not on the mathematical processes.",
        strong("But do not worry!"), "you will find alternatives to learn all these technical aspects independently.",
        style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"
      ),
      br(),
      p("The data used in this application are publicly available on the page of the", em("Anuario Estadístico de Antioquia"), "by the administrative planning department. 
                                          The data extracted from this public entity correspond to a series of social, educational, sports and safety variables in the rural areas of Antioquia in 
                                          Colombia for the year 2016.", style = "text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
      width = 8
    ),
    column(
      br(),
      tags$img(src = "Gobernacion.png", width = "200px", height = "130px"),
      br(),
      br(),
      p("For more information please check the", em("Anuario Estadístico de Antioquia's"), "page clicking",
        br(),
        a(href = "http://www.antioquiadatos.gov.co/index.php/anuario-estadistico-de-antioquia-2016", "Here", target = "_blank"),
        style = "text-align:center;color:black"
      ),
      width = 2
    )
  ),
  hr(),
  tags$style(".fa-database {color:#E87722}"),
  h3(p(em("Dataset "), icon("database", lib = "font-awesome"), style = "color:black;text-align:center")),
  fluidRow(column(DT::dataTableOutput("RawData"),
    width = 12
  )),
  hr(),
  p(em("Developed by"), a(href = "https://www.web4xiang.top/", "Xiang LI", target = "_blank"), style = "text-align:center; font-family: times")
)
