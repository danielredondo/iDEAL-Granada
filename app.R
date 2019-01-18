library(tidyverse)
library(xml2)
library(rvest)
library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("flatly"),
                titlePanel(tags$h2(tags$b("iDEAL")), windowTitle = "iDEAL"),
                tags$h4("Web de ", tags$a("Daniel Redondo", href = "http://www.danielredondo.com")),
                sidebarLayout(
                    sidebarPanel(width = 4,
                               h3("Página web de la noticia:"),
                               textInput(inputId = "url", 
                                           label = NULL,
                                           value = "https://www.ideal.es/culturas/informe-depende-nueva-20190118202616-nt.html"
                                           )
                               ),
                    mainPanel(uiOutput("noticia"))
                    )
)

server <- function(input, output) {

      # Web scraping
      titulo <- reactive(input$url %>% read_html() %>% html_nodes('h1') %>% html_text() %>% as.data.frame() %>% pull(1) %>% levels())
      subtitulo_y_categoria <- reactive(input$url %>% read_html() %>% html_nodes('h2') %>% html_text() %>% as.data.frame() %>% pull(1) %>% levels())
      subtitulo <- reactive(subtitulo_y_categoria()[4])
      categoria <- reactive(subtitulo_y_categoria()[3])
      cuerpo <- reactive({
            cuerpo_original <- input$url %>% read_html() %>% html_nodes('p') %>% html_text()
            # Ordenamos el cuerpo
            cuerpo_recortado <- cuerpo_original[31:(length(cuerpo_original) - 1)]
            body <- cuerpo_recortado[1]
            for (i in 2:length(cuerpo_recortado)){
              gsub("  ", "", cuerpo_recortado[i])
              if(cuerpo_recortado[i]!="") body <- rbind(body, cuerpo_recortado[i])
            }
       return(body)
      })
      
      imprimir_noticia <- reactive({
        req(input$url)
        aux_t <- titulo() %>% tags$b() %>% tags$h3() %>% paste()
        aux_s <- subtitulo() %>% tags$i() %>% tags$h4() %>% paste()
        aux_c <- categoria() %>% tags$u() %>% tags$h4() %>% paste()
        salida <- rbind(aux_t, aux_s, aux_c)
        for (i in 1:length(cuerpo())) salida <- rbind(salida, paste(tags$p(cuerpo()[i], "\n \n")))
        return(salida) 
      })
      
      output$noticia <- renderUI(HTML(imprimir_noticia()))
}

shinyApp(ui = ui, server = server)
