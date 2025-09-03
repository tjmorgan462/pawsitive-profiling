# Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tm)
library(tidytext)
library(textdata)
library(wordcloud)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(bslib)

# Read in data with read.csv() function
dogs <- read.csv("dog_data.csv")

# Remove unnecessary rows 
# Removed rows: status, data id, physical lifespan, physical size, coat length, 
dog_data_clean <- dogs[ , -c(1, 2, 3, 13, 14, 19)]

# Rename the rows for readability
dog_data_clean <- dog_data_clean %>%
  rename(
    Name = data.general.name, 
    Group = data.general.group, 
    Personality_Trait = data.general.personalityTraits,
    Short_Description = data.general.shortDescription,
    Long_Description = data.general.longDescription, 
    Popularity = data.general.popularity, 
    Height = data.general.height,
    Weight = data.general.weight,
    General_Lifespan = data.general.lifespan,
    Drooling = data.physical.droolingFrequency,
    Coat_Style = data.physical.coatStyle, 
    Coat_Texture = data.physical.coatTexture, 
    Double_Coat = data.physical.doubleCoat, 
    Family_Affection = data.behavior.familyAffection, 
    Child_Friendly = data.behavior.childFriendly, 
    Sociability = data.behavior.dogSociability, 
    Friendliness_Strangers = data.behavior.friendlinessToStrangers, 
    Playfulness = data.behavior.playfulness, 
    Protective_Instincts = data.behavior.protectiveInstincts, 
    Adaptibility = data.behavior.adaptability, 
    Barking_Frequency = data.behavior.barkingFrequency, 
    Shedding_Amount = data.care.sheddingAmount, 
    Grooming_Frequnecy = data.care.groomingFrequency, 
    Exercise_Needs = data.care.exerciseNeeds, 
    Mental_Stimulation_Needs = data.care.mentalStimulationNeeds, 
    Care_Training = data.care.trainingDifficulty, 
    Indoor_Image1 = data.images.small.indoors, 
    Outdoor_Image1 = data.images.small.outdoors, 
    Studio_Image1 = data.images.small.studio, 
    Indoor_Image2 = data.images.large.indoors, 
    Outdoor_Image2 = data.images.large.outdoors,
    Studio_Image2 = data.images.large.studio)

# Change column into numeric to get ready for data visualization 
dog_data_clean <- dog_data_clean %>%
  mutate(
    Popularity = as.numeric(Popularity),
    Height = as.numeric(Height), 
    Weight = as.numeric(Weight), 
    General_Lifespan = as.numeric(General_Lifespan), 
    Drooling = as.numeric(Drooling),
    Family_Affection = as.numeric(Family_Affection), 
    Child_Friendly = as.numeric(Child_Friendly), 
    Sociability = as.numeric(Sociability), 
    Friendliness_Strangers = as.numeric(Friendliness_Strangers), 
    Playfulness = as.numeric(Playfulness), 
    Protective_Instincts = as.numeric(Protective_Instincts), 
    Adaptibility = as.numeric(Adaptibility), 
    Barking_Frequency = as.numeric(Barking_Frequency), 
    Shedding_Amount = as.numeric(Shedding_Amount), 
    Grooming_Frequnecy = as.numeric(Grooming_Frequnecy), 
    Exercise_Needs = as.numeric(Exercise_Needs), 
    Mental_Stimulation_Needs = as.numeric(Mental_Stimulation_Needs), 
    Care_Training = as.numeric(Care_Training))

# Filter Variables
all_groups <- unique(dog_data_clean$Group) %>% str_sort()

# Define UI
ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Pawsitive Profiling"),
                    dashboardSidebar(
                      
                      # Line Break
                      br(),
                      
                      # Add HTML to add text
                      tags$p(class = "sidebar-text", 
                             "Welcome to the Dog Dashboard! Please use the widget below to select which group you would like the graphs to reflect."),
                      
                      # Year Dropdown
                      pickerInput(
                        inputId = "group_filter", 
                        label = h4("Group"), 
                        choices = all_groups,
                        selected = all_groups,
                        multiple = TRUE, 
                        options = list(
                          `actions-box` = TRUE,
                          `selected-text-format` = "count > 2"
                        )
                      ),
                      
                      # Line Break
                      br(),
                      
                      # Reset Button
                      actionButton(inputId = "reset", 
                                   label = "Reset", 
                                   icon = icon("sync")),
                      
                      # Line Break
                      br(),
                      
                      # Line Break
                      br()
                    ),
                    dashboardBody(
                      fluidRow(
                        tabsetPanel(
                          tabPanel("Word Cloud", plotOutput("word_cloud_1")),
                          tabPanel("Height vs Weight", plotlyOutput("plotly_1")),
                          tabPanel("Popularity", plotlyOutput("plotly_2")),
                          tabPanel("Heat Map", plotlyOutput("plotly_3"))
                        ),
                        absolutePanel(
                          bottom = 10,
                          left = 10,
                          width = "auto",
                          fixed = TRUE,
                          draggable = FALSE,
                          style = "font-size: 14px; color: white; background-color: transparent; z-index: 1000;",
                          paste("Source: https://registry.dog/")
                        )
                      )
                    )
)

# Define server
server <- function(input, output, session) {
  
  filtered_group_df <- reactive({
    dog_data_clean %>% 
      filter(Group %in% input$group_filter)
  })
  
  # Word Cloud
  output$word_cloud_1 <- renderPlot({
    # Word cloud for all dogs
    Dog_word_freq <- filtered_group_df() %>%
      filter(Short_Description != "") %>%
      unnest_tokens(word, Short_Description) %>%
      anti_join(stop_words, by = "word") %>%
      count(word, sort = TRUE)

    # Define custom color palette
    custom_colors <- c("steelblue", "steelblue4","gold4", "gold3","gold2", "gold")
    
    wordcloud(
      words = Dog_word_freq$word,
      freq = Dog_word_freq$n,
      max.words = 100,
      random.order = FALSE,
      colors =  custom_colors,
      scale=c(3.5,0.25))
  })
  
  # Graph height vs weight scatterplot
  output$plotly_1 <- renderPlotly({
    # Graph height vs weight scatterplot
    filtered_group_df() %>% 
      ggplot(aes(x = Height, y = Weight, color = Group, text = Name)) +
      geom_point(size = 3) +  # Add points to the scatterplot
      labs(title = "Height vs Weight of Different Adoptable Dog Breeds", 
           x = "Height (in)", 
           y = "Weight (lbs)") + 
      scale_color_manual(values = c("Hound" = "firebrick", "Terrier" = "red", 
                                    "Companion" = "magenta4", "Working" = "steelblue", 
                                    "Sporting" = "orange", "Herding" = "gold2", 
                                    "Guardian" = "forestgreen")) +  # Optional: Manually set colors
      theme_minimal() + # Set a minimal theme
      theme(plot.title = element_text(hjust = 0.5))   #Center title
  })
  
  # Ranking popularity
  
  # Change the Ranking System of the Popularity
  # Use mutate() function to create a copy of Popularity column, named "Popularity_chr)
  filtered_group_df_two <- reactive({ 
    filtered_group_df() %>% 
      group_by(Group, Popularity) %>%
      summarize(Count = n(), .groups = 'drop') 
  })
  
  output$plotly_2 <- renderPlotly({
    filtered_group_df_two() %>% 
      ggplot(aes(x = Popularity, y = Count, fill = Group)) +
      geom_bar(stat = "identity", position = "stack") +  # Create side-by-side bars for each type
      scale_x_continuous(breaks = 1:5, 
                         labels = c("Most Unpopular", "Unpopular", "Neutral", "Popular", "Most Popular")) +
      labs(title = "Dog Breed Popularity Assigned Group",
           x = "Popularity",
           y = "Count",
           fill = "Group") +
      scale_fill_manual(values = c("Companion" = "magenta4", "Guardian" = "forestgreen", "Herding" = "gold2", "Hound" = "firebrick", "Sporting" = "orange", "Terrier" = "red", "Working" = "steelblue")) +  # Custom colors
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Heat Map
  output$plotly_3 <- renderPlotly({
    # Manipulate data
    filtered_group_df() %>%
      filter(Family_Affection != "", Child_Friendly != "") %>%
      count(Family_Affection, Child_Friendly) %>%
      ggplot(aes(x = Family_Affection, y = Child_Friendly, fill = n)) +
      geom_tile() +
      scale_fill_gradient(low = "gold2", high = "firebrick2") +  # Customize colors
      theme_minimal() +
      labs(title = "Heatmap of Family Affection vs Child Friendly Ratings for Dog Breeds",
           x = "Family Affection Rating",
           y = "Child Friendly Rating",
           fill = "Frequency")
  })
    
  # Create an Observer Event for the Reset Button aka Click Event
  observeEvent(input$reset, {
    
    #  1. Reset Group     
    updatePickerInput(
      session = session, 
      inputId = "group_filter", 
      selected = all_groups)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
