# 参数设置界面提交成功显示
observeEvent(input$upload.setting.go, {
  sendSweetAlert(
    session = session,
    title = "Setting submit",
    text = "Data uploaded, setting coorrect!",
    type = "success"
  )
})

# 保存用户的数据和参数
observeEvent(
  input$upload.setting.go,
  {
    # 创建文件夹
    file.time <- Sys.time() %>%
      as.character() %>%
      stringr::str_replace_all(" ", "") %>%
      stringr::str_replace_all("-", "") %>%
      stringr::str_replace_all(":", "")
    # 创建文件夹
    dir.create(paste0("userdata/", file.time))

    # 读入数据
    df.gene.go <- read.table(input$uploadfile.go$datapath,
                             header = FALSE,
                             stringsAsFactors = TRUE,
                             encoding = "UTF-8"
    ) %>%
      magrittr::set_names("geneid")
    
    # 读入参数
    df.para <- data.frame(
      species = input$speciesid.go,
      pvalue = input$pvalue.go,
      qvalue = input$qvalue.go,
      min = input$min.gene.go,
      max = input$max.gene.go,
      method = input$adjust.method.go
    )
    
    save(df.gene.go,df.para, file = paste0("./userdata/", file.time, "/data.RData"))
    
    # 显示选择的物种的图片
    output$go.species.image <- renderUI(
      {
        tags$image(src = "Antioquia.png", height = "550px", width = "600px")
      }
    )
  }
)
