library(shiny)
library(data.table)

ui <- fluidPage(

  titlePanel("Infracțiunile din Statele Unite ale Americii comise în anul 1973"),
  
  p("Aplicația noastră utilizează date din DataFrame-ul USArrests, creat pe baza unui studiu efectuat pe un eșantion de 100,000 persoane în anul 1973: crime, asalturi și agresiuni sexuale."),
  p("Puteți găsi în partea stângă o listă drop-down din care puteți alege unul dintre cele 50 de state pentru a vedea detaliat ratele de arest pentru fiecare dintre cele 3 categorii menționate mai sus."),
  p("În partea din dreapta există mai multe modalități de reprezentare și vizualizare a tuturor informațiilor din setul de date ales."),
  br(),

  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("var", 
                  label = "Alegeți un stat pentru a vedea statistici detaliate",
                  choices = row.names(USArrests),
                  selected = "Alabama"),
      textOutput("selected_var"),
      br(),
      plotOutput(outputId = "statePlot"),
      
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Grafic",  plotOutput(outputId = "generalPlot")),
                  tabPanel("Diagramă de împrăștiere", plotOutput(outputId = "scatterPlot")),
                  tabPanel("Tabel", dataTableOutput('table')),
                  ),
      
    )
  )
)

server <- function(input, output) {

  output$generalPlot <- renderPlot({
    
    state.names <- row.names(USArrests)
    barplot(USArrests$Assault,names.arg = state.names, las = 2, ylab = "Rata de arest per 100,000", main = "Rata de arest din SUA în 1973", ylim = c(0,350), col = "green")
    barplot(USArrests$Rape, las = 2, col = "blue", add = TRUE)
    barplot(USArrests$Murder, las=2, col = "red", add = TRUE)
    
    legend(x = "topright", legend = c('Asalt','Agresiune sexuală','Crimă'),col=c("green","blue","red"),pch=15)

    })
  
  output$table <- renderDataTable({
    
    keep <- c("Murder", "Assault", "Rape")
    
    df <- USArrests[keep]
    
    colnames(df) <- c('Rata de crimă','Rata de asalt','Rata de agresiune sexuală')
    
    as.data.table(df, keep.rownames="Stat")})
  
  output$statePlot <- renderPlot({
    
    keep <- c("Assault", "Rape", "Murder")
    
    barplot(t(USArrests[input$var,][keep]), beside=TRUE,  ylab = "Rata de arest per 100,000", main = paste("Rata de arest din ", input$var, ", 1973", sep=""), ylim = c(0,350), col=c("green", "blue", "red"))
    
      legend(x = "topright", legend = c('Asalt','Agresiune sexuală','Crimă'),col=c("green","blue","red"),pch=15, cex = .75)
    
  })
  
  output$scatterPlot <- renderPlot({
    plot(y = USArrests$Murder, x = USArrests$Assault, xlab = "Rata de asalt", ylab = "Rata de crimă", main = "Rata de crimă vs. rata de atac, SUA, 1973")
  })
  
  output$selected_var <- renderText({ 
    paste('Statul selectat: ', input$var, sep="")
  })

}

shinyApp(ui = ui, server = server)