library(jsonlite)
library(tidyverse)
library(purrr)
library(ggplot2)
library(plyr)
library(gtools)
library(tidyr)
library(tidyverse)
library(kableExtra)
library(shiny)

temp = list.files(pattern="*.json")
m_data <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(m_data) <- c('video_n', 'score', 'video')
var = length(temp)
for (y in 1: var) {
  json_data <- read_json(path=temp[y])
  my_list <- unlist(json_data, recursive = FALSE)
  l = length(json_data)
  v <- my_list[["video"]]
  t <- my_list[["score"]]
  if (grepl("p1204_5",v)==TRUE){
    m_data[nrow(m_data) + 1,] <- c(as.integer(as.numeric(substr(v, 25, 26))), t, v)
  }
  else if (grepl("p1204_4",v)==TRUE){
    m_data[nrow(m_data) + 1,] <- c(as.integer(as.numeric(substr(v, 25, 26))), t, v)}
  else if (grepl("p1204_2",v)==TRUE){
    m_data[nrow(m_data) + 1,] <- c(as.integer(as.numeric(substr(v, 25, 26))), t, v)}
  else {
    m_data[nrow(m_data) + 1,] <- c(as.integer(as.numeric(substr(v, 7, 9))), t, v)
  }
}


m_data$video_n <- as.integer(m_data$video_n)
m_data <- arrange(m_data, video_n)
z <- nrow(m_data)/6
table_data <- data.frame(matrix(ncol = 7, nrow = z))
colnames(table_data) <- c('week', 'd1', 'd2', 'd3', 'd4', 'd5', 'd6')
p <- nrow(m_data)
for (r in 1: p) {
  table_data$week[r] <- r
  table_data$d1[r] <- m_data$score[(r*6) - 5]
  table_data$d2[r] <- m_data$score[(r*6) - 4]
  table_data$d3[r] <- m_data$score[(r*6) - 3]
  table_data$d4[r] <- m_data$score[(r*6) - 2]
  table_data$d5[r] <- m_data$score[(r*6) - 1]
  table_data$d6[r] <- m_data$score[r*6]
  print(r*6)
}

table_data$d1 <- as.numeric(table_data$d1)
table_data$d2 <- as.numeric(table_data$d2)
table_data$d3 <- as.numeric(table_data$d3)
table_data$d4 <- as.numeric(table_data$d4)
table_data$d5 <- as.numeric(table_data$d5)
table_data$d6 <- as.numeric(table_data$d6)
td2 <- as.data.frame(table_data)
write.csv(table_data,"p1204_table_4ph.csv", row.names = FALSE)
write.csv(m_data,"p1204_raw.csv", row.names = FALSE)

td2[] <- lapply(td2, function(x) paste("zz", x, sep=" "))

print.xtable(xtable(td2), file = "./Data.txt")

shinyApp(
  ui = fluidPage(
    fluidRow(
      column(12,
             dataTableOutput('table')
      )
    )
  ),
  server = function(input, output) {
    output$table <- renderDataTable(table_data)
    output$downloadPlot <- downloadHandler(
      filename = function() { paste(input$dataset, '.png', sep='') },
      content = function(file) {
        png(file)
        print(renderDataTable(table_data))
        dev.off()
      })
  }
)



color_cells <- function(value) {
  ifelse(value > 3.8, "background-color: green;",
         ifelse(value < 2.5, "background-color: red;", ""))
}

# Apply the color_cells function to the 'Value' column
table_data$d1 <- cell_spec(table_data$d1, background = color_cells(table_data$d1))
table_data$d2 <- cell_spec(table_data$d2, background = color_cells(table_data$d2))
table_data$d3 <- cell_spec(table_data$d3, background = color_cells(table_data$d3))
table_data$d4 <- cell_spec(table_data$d4, background = color_cells(table_data$d4))
table_data$d5 <- cell_spec(table_data$d5, background = color_cells(table_data$d5))
table_data$d6 <- cell_spec(table_data$d6, background = color_cells(table_data$d6))
# Create a table using kable and add formatting
styled_table <- kable(table_data, format = "html", escape = FALSE) %>%
  kable_styling()

# Display the table directly
print(styled_table)

