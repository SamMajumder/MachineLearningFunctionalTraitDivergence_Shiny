#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny) 
library(bslib)
library(tidyverse)
library(here)
library(ggplot2)
library(plotly)
library(RColorBrewer)


### Loading the datasets ### 

Gini_Importance <- readRDS("Gini_Importance_combined.RDS") %>% 
                           rename(Phylogeny_Level = `Phylogenetic Level`) %>% 
                           mutate(Groups = "All")

Optimal_subset <- readRDS("Optimal_subset.RDS") %>% 
                      rename(Phylogeny_Level = `Phylogenetic Level`) %>% 
                      mutate(Groups = "Optimal Subset")

Most_Divergent <- readRDS("Most_Divergent.RDS") %>% 
                      rename(Phylogeny_Level = `Phylogenetic Level`) %>% 
                      mutate(Groups = "Most Divergent") %>% 
                      select(meanImp,Feature:Groups) %>% 
                      rename(Overall = meanImp) %>% 
                      rename(Features = Feature)


### Merge the three datasets ### 

Importances <- rbind(Gini_Importance,Optimal_subset,Most_Divergent)


Importances$Trait_type <- factor(Importances$Trait_type, 
                                    levels = c("Leaf","Whole Plant","Flower"))

######### 

Glossary <- read.csv("Glossary.csv") %>% 
                       rename(Features = ABBREVIATIONS) %>% 
                       rename(Trait_name = TRAITS)


Importances <- Importances %>% inner_join(Glossary,
                           by="Features")

## removing the units ##
Importances$Trait_name <- gsub("\\(.*?\\)", "", Importances$Trait_name)

#### Load the 3D plots ###

# Load plot from HTML file

Genus3D <- readRDS("Genus_3d.RDS")

Perennials3D <- readRDS("Perennials_3d.RDS")

Annuals_3D <- readRDS("Annual_3d.RDS")

Southeastern_3D <- readRDS("Southeastern_3d.RDS")

## UI ## 

ui <- fluidPage(
  theme = bs_theme(),
  
  titlePanel("Trait Divergence in wild sunflower"),
    
    tabsetPanel(
      tabPanel("About",
               h1("Description"),
               p("This accompanying shiny app showcases some of the key results 
               from the study titled - 'A Machine Learning approach to study plant 
               functional trait divergence', authored by Sambadi Majumder and
               Dr. Chase Mason. More information about this study can be found 
               here:doi: https://doi.org/10.1101/2023.03.16.533012.
               
               The study takes a novel approach to identifying the 
               functional traits that contribute most to interspecific phenotypic 
               divergence in multivariate trait space. The app has two tabs that 
               provide insights into the predictability or repeatability of 
               evolution and can be used in various contexts across basic and 
               applied plant science.
               
               The 'Trait ranked based on importance' tab showcases trait rankings 
               at the genus, large perennial, and southeastern perennial phylogeny 
               levels, as well as the optimal subset of ecologically relevant traits 
               and the most divergent traits in a multivariate trait space. 
               By applying machine learning techniques, this approach can identify 
               divergent traits more objectively and with higher accuracy than 
               previous methods.
               
               If such an approximate consensus regarding important traits could 
               be found, it would immensely help researchers in comparing 
               ecophysiological studies in different systems, conducting meta-analysis 
               across studies, and forecasting future vegetation dynamics under a 
               changing climate (Westoby, 1998). The information provided by this 
               shiny application can be used to compare ecophysiological studies 
               in different systems, conduct meta-analysis, and forecast future 
               vegetation dynamics under a changing climate, thereby helping to 
               bridge the gap between ecological theory and practice.
                 
               The 'Divergence of Traits' tab shows how species potentially 
               diverge in a multivariate trait space and visualizes the trade-offs. 
               This tab provides a useful tool for understanding the relationships 
               among plant functional traits and how they contribute to the phenotypic 
               divergence of species.Overall, this shiny application provides valuable 
               insights into the ecological strategies of plant species, which can help 
               researchers compare studies in different systems, conduct meta-analysis, 
               and forecast future vegetation dynamics under a changing climate."),
               
               h2("References"),
               
               p("Westoby, M. 1998.A leaf-height-seed (LHS) plant ecology strategy scheme.Plant and Soil 199: 
                     213–227.")
              ),
      
      tabPanel("Traits ranked based on importance",
           ## Hierarchical dropdowns #####
           fluidRow(
             column(width = 6,
                    selectInput("Groups",
                                "Groups",
                                choices = unique(Importances$Groups))),
             column(width = 6,
                    selectInput("Phylogeny_Level",
                                "Phylogeny_Level",
                                choices = NULL))
           ),
               plotlyOutput("Importance",
                            height = "800px")
              ),
      tabPanel("Divergence of Traits",
               ## The plots ###
               fluidRow(
                 column(width = 6,
                        plotlyOutput("Genus")),
               column(width = 6,
                     plotlyOutput("Perennial")),
               column(width = 6,
                      plotlyOutput("Annual")),
               column(width = 6,
                 plotlyOutput("Southeastern"))
              )
               
          )
      )
  )



server <- function(input,output,session){  
  
  ### server logic for hierarchical dropdowns ###
  
  ## Phylogeny Levels 
  
  Phylogeny_Level_choices <- reactive({
    
    Importances %>% 
      filter(Groups == input$Groups) %>% 
      pull(Phylogeny_Level)
  })
  
  ### Update Phylogenetic levels dropdown based on selected Group ###
  
  observe({
    updateSelectInput(session, "Phylogeny_Level",
                      choices = Phylogeny_Level_choices())
  }) 
  
  
  
  #### Variable Importance plot ###
  
  output$Importance <- renderPlotly({
    
    p1 <- Importances %>%  
                filter(Groups == input$Groups,
                       Phylogeny_Level == input$Phylogeny_Level) %>%
      ggplot(aes(x=reorder(Features,
                           Overall), 
                 y = Overall, 
                 fill = Trait_type,
                 text = paste("Trait", Trait_name, sep = ": "))) +
      geom_bar(stat = "identity",
               color ="black") + 
      scale_fill_manual(values = c("#A6D854",
                                   "#E5C494",
                                   "#FFD92F"),
                        name= 'Trait type') +
      labs(x= "Traits",
           y= "Variable Importance") +
      coord_flip() + 
      theme_bw() + theme(legend.position = c(0.78, 0.15),
                         legend.background = element_rect(fill = "white", 
                                                          color = "black")) +
      theme(text = element_text(size = 10)) 
    
    ggplotly(p1)
    
  })  
  
  # Load the Genus Plot
  output$Genus <- renderPlotly({
     Genus3D
  })
  
  # Load Perennial Plot
  output$Perennial <- renderPlotly({
     Perennials3D
  })
  
  
  ### Load the Annual Plot 
  output$Annual <- renderPlotly({
    Annuals_3D
  })
  
  ### Load the Southeastern Perennial Plot 
  output$Southeastern <- renderPlotly({
    Southeastern_3D
  })
  

  
} 

### Run the app ##

shinyApp(ui,server)










