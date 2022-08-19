# 参数设置界面提交成功显示
observeEvent(input$upload.setting.go, {
  sendSweetAlert(
    session = session,
    title = "Analysis is running......",
    text = "After complete, a image will appear on the right :)",
    type = "success"
  )
})

# 下载示例数据
output$downlaod.example.go <- downloadHandler(
  filename = "GO.example.input.txt",
  content = function(file) {
    file.copy("./data/test.gene.txt", file)
  }
)
# 保存用户的数据和参数
observeEvent(
  input$upload.setting.go,
  {
    # 创建文件夹
    file.time <<- Sys.time() %>%
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

    save(df.gene.go, df.para, file = paste0("./userdata/", file.time, "/data.RData"))

    # 显示选择的物种的图片
    output$go.species.image <- renderUI({
      tags$image(
        src = paste0("image/", input$speciesid.go, ".png"),
        height = "550px",
        width = "660px",
        `border-radius` = "100%"
      )
    })

    # 开始进行GO富集分析

    # 加载选择物种的GO表格
    load(paste0("data/species/", input$speciesid.go, ".go.RData"))

    # 加载GO条目的注释信息
    load("data/go.RData")

    # 合并文件
    df.gene %>%
      dplyr::left_join(df.go, by = "goid") -> df.go

    # 测试上一步是否生效
    # save(df.base, file = "test.RData")

    # 开始富集分析
    go.rich <- clusterProfiler::enricher(
      gene = df.gene.go$geneid,
      TERM2GENE = df.go[c("goid", ifelse(input$id.type.go == "gene", "geneid", "pep.id"))],
      TERM2NAME = df.go[c("goid", "term")],
      pvalueCutoff = input$pvalue.go,
      qvalueCutoff = input$qvalue.go,
      pAdjustMethod = input$adjust.method.go,
      minGSSize = input$min.gene.go,
      maxGSSize = ifelse(is.null(input$max.gene.go),
        length(unique(df.gene.go$geneid)),
        input$max.gene.go
      )
    )

    go.rich@result$a <- 1
    go.rich@result$b <- 2

    for (i in 1:nrow(go.rich@result)) {
      go.rich@result$a[i] <- as.numeric(stringr::str_split(go.rich@result$GeneRatio[i], "/")[[1]][1])
      go.rich@result$b[i] <- as.numeric(stringr::str_split(go.rich@result$GeneRatio[i], "/")[[1]][2])
    }

    go.rich@result$GeneRatio <- go.rich@result$a / go.rich@result$b
    go.res <- go.rich@result %>% 
      dplyr::filter(
        pvalue <= input$pvalue.go,
        qvalue <= input$qvalue.go
      )

    # 保存结果
    save(go.res, file = paste0("userdata/", file.time, "/go.res.RData"))
  }
)

# 展示结果表格
observeEvent(
  input$upload.table.go,
  {
    # 加载数据
    load(paste0("userdata/", file.time, "/go.res.RData"))

    # 保存数据
    if (input$table.type.go == "excel") {
      writexl::write_xlsx(go.res,
        path = paste0("userdata/", file.time, "/go.res.xlsx"),
        col_names = ifelse(input$save.colname.go == "yes", TRUE, FALSE)
      )
    } else if (input$table.type.go == "txt") {
      data.table::fwrite(go.res,
        file = paste0("userdata/", file.time, "/go.res.txt"),
        sep = "\t",
        col.names = ifelse(input$save.colname.go == "yes", TRUE, FALSE),
        row.names = ifelse(input$save.rowname.go == "yes", TRUE, FALSE)
      )
    } else {
      data.table::fwrite(go.res,
        file = paste0("userdata/", file.time, "/go.res.csv"),
        sep = ",",
        col.names = ifelse(input$save.colname.go == "yes", TRUE, FALSE),
        row.names = ifelse(input$save.rowname.go == "yes", TRUE, FALSE)
      )
    }

    # 输出表格
    output$go.res.table <- DT::renderDataTable(
      DT::datatable(
        go.res %>%
          dplyr::select(1:2, 5:7),

        # 表格选项
        options = list(
          lengthMenu = list(c(5, 10, 20, 50), c("5", "10", "20", "500")), pageLength = 10,
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
    )
  }
)

# 下载富集分析的结果
output$download.tab.go <- downloadHandler(
  if (input$table.type.go == "excel") {
    filename <- "GO.results.xlsx"
  } else if (input$table.type.go == "txt") {
    filename <- "GO.results.txt"
  } else {
    filename <- "GO.results.csv"
  },
  content = function(file) {
    if (input$table.type.go == "excel") {
      file.copy(paste0("userdata/", file.time, "/go.res.xlsx"), file)
    } else if (input$table.type.go == "txt") {
      file.copy(paste0("userdata/", file.time, "/go.res.txt"), file)
    } else {
      file.copy(paste0("userdata/", file.time, "/go.res.csv"), file)
    }
  }
)
