tabPanel(
  "GO",
  tabsetPanel(
    tabPanel(
      "Demo",
      br(),
      br(),
      sidebarLayout(
        sidebarPanel(
          p("Is there a linear relationship between the response variable and this independent variable? no? then:", style = "color:black;text-align:justify"),
          tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: coral; border-top: 1px coral; border-bottom: 1px coral;border-left: 1px coral}")),
          tags$style(HTML(".js-irs-1 .irs-max, .js-irs-1 .irs-min {background:papayawhip}")),
          sliderInput("Transformacion1", p("Try power transformations to achieve the linear relationship we are looking for"),
            value = 1,
            min = -3,
            max = 3,
            step = 0.01
          ),
          br(),
          column(
            br(),
            tags$head(tags$style("#correlacion1{color: black;
                                                                        font-size: 15px;
                                                                        text-align: center;
                                                                        }")),
            textOutput("correlacion1"),
            br(),
            p("This coefficient is a measure of the strength and direction of the linear relationship, so we want to achieve a coefficient closer to |1|", style = "color:black;text-align:justify"),
            br(),
            width = 12, style = "background-color:papayawhip;border-left:8px solid coral;border-top:1px solid black;border-bottom:1px solid black;border-right: 1px solid black"
          ),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br()
        ),
        mainPanel(column(plotOutput("Dispersion1"), width = 12, style = "border:1px solid black"))
      ),
      br()
    ),
    tabPanel(
      "Setting",
      br(),
      # 参数设置
      column(
        width = 6,
        # title
        h4("Parameter Setting", align = "center"),
        HTML("<hr style='background-color: #282828'>"),
        splitLayout(
          cellWidths = c("50%", "50%"),
          # upload file
          fileInput(
            "uploadfile.go",
            label = h5("Upload", align = "center"),
            # label = HTML('<h6 style="text-align:right">Upload file</h6>'),
            accept = NULL,
            buttonLabel = "View..."
          ),
          # species id
          textInput(
            "speciesid.go",
            label = h5("Species ID")
          )
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          # pvalue cutoff
          numericInput(
            "pvalue.go",
            label = h5("Pvalue cutoff"),
            min = 0,
            max = 1,
            step = 0.1,
            value = 0.05
          ),
          # qvalue
          numericInput(
            "qvalue.go",
            label = h5("qvalue cutoff"),
            min = 0,
            max = 1,
            step = 0.01,
            value = 0.05
          )
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          # min genes
          numericInput(
            "min.gene.go",
            label = h5("minGene"),
            value = 1
          ),
          # max genes
          numericInput(
            "max.gene.go",
            label = h5("maxGene"),
            value = NULL
          )
        ),

        # adjust method
        selectInput(
          "adjust.method.go",
          label = h5("Pvalue adjust method"),
          width = "95%",
          choices = list(
            "holm" = "holm",
            "hochberg" = "hochberg",
            "hommel" = "hochberg",
            "bonferroni" = "bonferroni",
            "BH" = "BH",
            "BY" = "BY",
            "fdr" = "fdr",
            "none" = "none"
          ),
          selected = "BH"
        ),
        # submit
        actionButton(
          "upload.setting.go",
          width = "95%",
          label = "Submit",
          icon = icon("arrow-up")
        )
      ),
      # 上传数据预览
      column(
        width = 6,
        DT::dataTableOutput(
          "upload.preview.go"
        )
      ),
      br()
    ),

    # 输出结果表格
    tabPanel(
      "Table",
      br(),
      br(),
      column(
        width = 3,
        # title
        h4("Parameter Setting", align = "center"),
        HTML("<hr style='background-color: #282828'>"),
        selectInput(
          "table.type.go",
          label = h5("Output file format"),
          choices = list(
            "Excel" = "excel",
            "TXT" = "txt",
            "CSV" = "csv"
          ),
          selected = "excel"
        ),
        # col names
        selectInput(
          "save.colname.go",
          label = h5("Save column names"),
          choices = list(
            "Yes" = "yes",
            "No" = "No"
          ),
          selected = "yes"
        ),

        # ronam names
        selectInput(
          "save.rowname.go",
          label = h5("Save row names"),
          choices = list(
            "Yes" = "yes",
            "No" = "no"
          ),
          selected = "no"
        ),

        # submit
        actionButton(
          "upload.table.go",
          width = "95%",
          label = "Submit",
          icon = icon("arrow-up")
        ),
        br(),
        br(),

        # download
        downloadButton(
          "download.tab.go",
          label = h5("Download table"),
          style = "width:95%;"
        )
      ),

      # 表格预览
      column(
        width = 9
      )
    ),

    # 绘图结果表格
    tabPanel(
      "Plot",
      br(),
      br(),
      column(
        width = 3,
        # figure type
        selectInput(
          "fig.type.go",
          label = h5("Figure type"),
          choices = list(
            "Bar" = "bar",
            "Point" = "point"
          ),
          selected = "point"
        ),
        # legend posi
        selectInput(
          "legend.posi.go",
          label = h5("Legend position"),
          choices = list(
            "Right" = "right",
            "Left" = "left",
            "Top" = "top",
            "Bottom" = "bottom"
          ),
          selected = "right"
        ),

        # fill color low
        colourInput(
          "low.color.go",
          label = h5("Fill color for min"),
          "blue",
          returnName = TRUE,
          closeOnClick = TRUE
        ),
        # fill color max
        colourInput(
          "max.color.go",
          label = h5("Fill color for max"),
          "red",
          returnName = TRUE,
          closeOnClick = TRUE
        ),
        # submit
        actionButton(
          "upload.plot.go",
          width = "95%",
          label = "Submit",
          icon = icon("arrow-up")
        ),
        br(),
        br(),
        # download
        downloadButton(
          "download.plot.go",
          label = h5("Download plot"),
          style = "width:95%;"
        )
      ),
      column(
        width = 9,
        style = "border:1px solid black",
        plotOutput("plot.go")
      )
    )
  )
)
