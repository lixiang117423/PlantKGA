# load data
load("./data/meta.table.rda")

# function to turn txt into link
ToLink <- function(txt,link) {
  paste0("<a href='",link,"'"," target='_blank'",">",txt,"</a>")
}

# main
output$RawData <- DT::renderDataTable(
  DT::datatable(
    meta.table %>%
    dplyr::mutate(Version = as.character(Version)) %>%
    dplyr::mutate(Link = ToLink("Click download", DownloadLink)) %>%
    dplyr::select(
      Plant_latin,
      Plant_Chinese,
      Order,
      Family,
      Version,
      Link
    ),
  options = list(
    lengthMenu = list(c(10, 20, 50,100), c("10", "20", "50","100")), pageLength = 10,
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
