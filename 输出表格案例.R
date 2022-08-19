# 输出基因名称
output$upload.gene.preview.go <- DT::renderDataTable(
  df.gene.go(),
  options = list(
    lengthMenu = list(c(10, 20, 50, 100), c("10", "20", "50", "100")), pageLength = 10,
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
      "}"
    ),
    columnDefs = list(list(className = "dt-center", targets = "_all"))
  ),
  filter = "top",
  selection = "multiple",
  style = "bootstrap",
  class = "cell-border stripe",
  rownames = FALSE,
  escape = FALSE
)

df.gene.go <- eventReactive(
  input$upload.setting.go,
  {
    tab.gene.go <- read.csv(input$uploadfile.go$datapath,
                            header = FALSE,
                            stringsAsFactors = TRUE,
                            encoding = "UTF-8"
    ) %>%
      as.character()
    tab.gene.go <<- tab.gene.go
  }
)
