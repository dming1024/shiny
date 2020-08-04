
library(shiny)
options(shiny.usecairo = FALSE)
data <- read.table("volcano.txt",
                   header = F,
                   stringsAsFactors = F)
colnames(data)=c("genename","log2FC","pvalue")
ui <- fluidPage(
  titlePanel("Volcano"),
  fluidRow(
    column(
      4,
      sliderInput(
        'pvalue',
        label = 'P value',
        min = 0,
        max = 0.5,
        value = 0.05,
        step = 0.01
      )
    ),
    
    column(
      4,
      sliderInput(
        'log2foldchange',
        'Log2FoldChange',
        min = 1,
        max = 5,
        value = 1,
        step = 0.5
      )
    ),
    
    column(3,
           selectInput(
             'up_color',
             'up_regulated:',
             choices = c('red', 'blue', 'yellow', 'green')
           )),
    
    column(3,
           selectInput(
             'down_color',
             'down_regulated:',
             choices = c("blue", "green", "red", "yellow")
           )),
    
    column(3,
           textInput('x_label',
                     'xLables:',
                     value = "log2FC")),
    column(3,
           textInput('y_label',
                     'yLabels:',
                     value = "PValue")),
    column(3,
           textInput('title',
                     'Title:',
                     value = "volcano")),
    column(
      10,
      textInput('gene_list',
                "GeneList:",
                placeholder = "Genename")
    ),
    
    #    column(12,
    #           plotOutput("plot", height = "600px", width = "90%")
    #          )
    mainPanel(
      tabsetPanel(
        tabPanel("plot",plotOutput("plot",height = "600px",width="90%")),
        tabPanel("summary",tableOutput("table"))
      )
    )
  )
)
server <- function(input, output) {
  output$plot <- renderPlot({
    plot(
      data$log2FC,
      -log10(data$pvalue),
      pch = 16,
      xlab = input$x_label,
      ylab = input$y_label,
      main = input$title,cex.lab=1.5,cex.main=2
    )
    degs <-
      data[data$pvalue < input$pvalue[1] &
             abs(data$log2FC) > input$log2foldchange[1],]
    points(
      degs$log2FC,-log10(degs$pvalue),
      col = ifelse(degs$log2FC > 0, input$up_color, input$down_color),
      pch = 16
    )
    abline(v = c(-input$log2foldchange[1], input$log2foldchange[1]),
           lty = 10)
    abline(h = -log10(input$pvalue[1]), lty = 8)
    gene_list = input$gene_list
    gene_list = unlist(strsplit(gene_list, split = ","))
    if (length(gene_list) > 0 & length(gene_list) < 20) {
      data_selected = data[data$genename %in% gene_list,]
      text(data_selected$log2FC,-log10(data_selected$pvalue),
           data_selected$genename)
    }
  })
  
  output$table<-renderTable({
    degs <-
      data[data$pvalue < input$pvalue[1] &
             abs(data$log2FC) > input$log2foldchange[1],]
    table(ifelse(degs$log2FC<0,"up","down"))})
}

shinyApp(ui, server)