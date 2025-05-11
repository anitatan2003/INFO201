library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)

#file 
df1 <- read.csv("finalDataset2.csv")
df <- read.csv("finalDataset.csv")

#intro page
homepg <- tabPanel( "Homepage",
  fluidPage(
  h1("Quality of Life in the US from 1984-2019"),
  p("We all know and understand that the quality of life has significantly improved over the past couple of decades. But, what do trends, numbers, and statistics have to show about this fact? Although quality of life is inherently a subjective measure of an individual’s well-being, we have developed a general definition that analyzes standards of living in the United States from the years of 1984 to 2019. We will analyze housing prices, usage of SNAP food benefits, life expectancy, cereal production, and GDP. It is anticipated that these values slowly improve throughout the decades, but are there any surprising trends that we may find to tell us something that isn’t shown with numbers alone?
    "),
  h5("Below are some articles that sparked our interest and motivation on this topic!"),
  
  #1st article
  a("Measuring Quality of Life", href="https://www.berkeleywellbeing.com/quality-of-life.html"),
  img(src = "quality.png", width='200px', height='150px'),
  p("This article talks about the theory and research surrounding quality of life."),
  br(),
  a("Improved Nutritional Outcomes and Lower Health Care Costs with SNAP Benefits", 
    href="https://www.cbpp.org/research/food-assistance/snap-is-linked-with-improved-nutritional-outcomes-and-lower-health-care"),
  p("New and emerging research links the Supplemental Nutrition Assistance Program (SNAP, formerly food stamps), the nation’s most important anti-hunger program, with improved health outcomes and lower health care costs. This research adds to previous work showing SNAP’s powerful capacity to help families buy adequate food, reduce poverty, and help stabilize the economy during recessions."),
  br(),
  a("Health trends in the US", href="https://www.healthdata.org/data-visualization/us-health-map"),
  img(src = "UShealthmap.png", width='200px', height='150px'),
  p("Explore health trends in the US at the county level between 2000 and 2019 for 
    life expectancy by race and ethnicity and mortality by race and ethnicity"),
  br(),
  a("Do housing prices affect residents' health?", href="https://www.frontiersin.org/articles/10.3389/fpubh.2021.816372/full#:~:text=The%20study%20finds%20that%20overall%2C%20rising%20housing%20prices,own%20a%20house%20show%20the%20greatest%20adverse%20effect."),
  p("This paper aims to explore the effect and mechanism of rising housing prices on residents' physical and mental health. Using data from the China Family Panel Studies from 2014 to 2018, we investigate the impact and mechanism of rising housing prices on the mental and physical health of urban residents through multiple grouping regression and analysis of variance."),
  br(),
  a("Using gross domestic product to measure the health of the econnomy", href="https://usafacts.org/data/topics/economy/economic-indicators/gdp/gross-domestic-product/"),
  p("Observing GDP can tell how the economy is doing. A growing GDP implies an improving economy. This website 
    provides plots for the changes in US GDP."),
  br()
  )
)

#controls for cotpg
cotcontrols <- sidebarPanel(
  h1("Control Panel"),
  selectInput(
    inputId = "trend",
    label = "Select a trend",
    choices = c("AVG SNAP Participation"="AVG.SNAP.Participation",
                "AVG SNAP Benefit per Person"="AVG.SNAP.Benefit.per.Person",
                "Median Household Value"="Median.Household.Value",
                "Population"="Population",
                "Cereal Production"="Cereal.Production",
                "GDP_PC"="GDP_PC" ,
                "Life Expectancy"="Life.Expectancy")
  )
)

#change over time pg
cotpg <- tabPanel("Change over Time",
  fluidPage(
    titlePanel("Change Over Time"),
    p("There are different factors that affect quality of life. Some of the ones we have selected to analyze include: SNAP Benefit Participation & usage per person, Median Household Value, Population, GDP, Cereal Production, and Life Expectancy.
    Below are different lineplots that show the changes in different factors affecting quality of life. These are all recorded for the years of 1984-2019. 
      Feel free to explore and notice the increasing trends on these plots!"),
    sidebarLayout(cotcontrols,
      mainPanel(
        h4("Trends over time in the US (1984-2019)"),
        plotOutput("cotscatter"),
        p("As mentioned earlier, all of the plots show a strong positive correlation and exhibit significant growth over time. Notably, the AVG SNAP Participation plot reveals a substantial increase in the number of individuals participating in SNAP food benefits between the years 2004 and 2013.

The reasons behind this significant jump in SNAP participation remain uncertain and can be attributed to various factors. On our homepage, an article titled 'Improved Nutritional Outcomes and Lower Health Care Costs with SNAP Benefits' highlights the correlation between increased SNAP usage and reduced healthcare expenses. This observation raises the possibility of a connection between SNAP benefits and improved health outcomes, considering the rising life expectancy.

However, it is important to note that life expectancy data encompasses both SNAP users and non-users. Therefore, the existing evidence is insufficient to establish a definitive claim about the link between SNAP usage and life expectancy. Nevertheless, when considering the consistently positive growth depicted in all the plots, it is reasonable to conclude that the quality of life has experienced a remarkable improvement since 1984.")
      )
    )
  )
)

#zoom pg
zoompg <- tabPanel( "Zoom out",
  fluidRow(
    column(
      width = 8,
      offset = 2,
      tags$div(
        class = "jumbotron",
        h1("Welcome to the Change in the quality of life in the US", class = "display-4"),
        p("Explore the data trends and insights related to Mean Housing Cost and GDP over the years."),
        p("Select the diagram to be displayed below.")
      )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      # Input element 1: Numeric input
      
      # Input element 3: Checkbox group
      checkboxGroupInput("diagram_input", "Select diagram to be displayed :", 
                         choices = c("Line Graph comparing the GDP over the years (1984 - 2019)", 
                                     "Line Graph comparing the Mean Housing Cost over the years (1984 - 2019)" ), 
                         selected = "Line Graph comparing the GDP over the years (1984 - 2019)")
    ),
    
    mainPanel(
      # Display user selection
      h3("Using Zoom out data story narrative "),
      br(),
      h4("01 - Mean Housing Cost"),
      p(
        "The three highest mean housing costs were $61136, $63179, and $68703. These mean housing costs were respectively recorded in the years 2017, 2018, and 2019. This means that there was a 12% percentage difference in the past three years. Conversely, the mean housing cost in 1984 was $22415, which means that over a 35-year period, there was a 95.25% increase."
      ),
      
      br(),
      br(),
      h4("02 - GDP Per Capita over a 19-year period"),
      p(
        "In 2000, the GDP per capita was $44726.9654, by 2001 it rose to $44728.59748, then finally, in 2002, it was recorded as $45087.36728. It steadily rose from 2000 to 2001 then had a notable increase in 2002. This trend carried on as by 2019, the GDP per capita was recorded as $55753.14437. This means that over a 19-year period, there was a 22.35% increase in GDP per capita. Overall, 2019 recorded the highest GDP per capita."
      ),
      
      br(),
      h4("03 - Mean Housing Cost and Total Benefits"),
      p(
        "The highest recorded total benefits received in the US was $76066.32 in 2013. 2013's corresponding mean housing cost was $53585. There was a difference of $22481.32 between total benefits received and the mean housing cost. Over a 35-year period, there was a difference of $11718.90 in 1984 and a difference of $13081.22 in 2019."
      ),
      
      br(),
      p("Below is the plot you selected"),
      
      plotOutput("linePlot")
    )
  )
  
)

#contrast pg
conpg <- tabPanel("Contrast",
                  titlePanel("Contrast in Quality of Life"),
                  br(),
                  p("Life Expectancy is a big factor that contributes to calculating a nation's quality of life. When it comes to 
    the Unites States, we can notice an increase in life expectancy over the years. Some factors
    like SNAP food benefits that members of the population receive, changes in housing prices, along with cereal production
    can have an effect on life expectancy."),
                  p("To visualize this change, the following graph compares the life expectancy in the U.S from the 
    years selected on the side bar."),    # page intro text
                  br(),
                  
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("dropdown1", "Select a year:", choices = df$Fiscal.Year),
                      selectInput("dropdown2", "Select another year:", choices = df$Fiscal.Year),
                      br(),
                      p("According to our data collected, life expectancy has had an increse since 1984. Going from 74.56341 years to
        78.7878 years of age. Along with this, we can observe that cereal production has also had a major increase
        over the decades, going from 314,749,500 to 467,951,140. Total population SNAP benefits have also increased as 
        more members of the population are receiving these, in 2019 there was 55,621.88 compared to 10,696.10 in 1984."),
                      br(),
                      p("With this data, we can make the connection that all these factors have an effect on the life expectancy
        of the nation, and on the quality of life overall.")
                      
                    ),
                    
                    mainPanel(
                      textOutput("selectedOption1"),
                      textOutput("selectedOption2"),
                      h3("Comparing Life Expectancy"),
                      plotlyOutput(outputId = "scatter"),
                      
                    ),
                    
                    position = "right"
                  )
)

#summary takeaways & about pg
sumpg <- tabPanel( "Summary & Takeaways",
                   titlePanel("Summary & Takeaways"),
                   
                   mainPanel(
                     align = "right",
                     textOutput("text"),
                     br(),
                     
                     fluidRow(
                       column(
                         width = 8,
                         plotOutput("plot")
                       ),
                       column(
                         width = 4,
                         align = "right",
                         br(),
                         p("The following scatterplot visualizes the population change in the U.S over the years, 
          which shows us that an increase in quality of life over time also leads to an increase of population 
          over the years.")
                       )
                     ),
                     
                     fluidRow(
                       column(
                         width = 12,
                         p("Authors of data sets used: John M., Gapminder.com, and Zillow"),
                         p("Funding: No funding was used for this project"),
                         p("Sources: kaggle.com, gaminder.com, and zillow.com"),
                         br(),
                         br(),
                         br(),
                         p("Project by Taise Nish, Valeria Fierro, & Anita Tan")
                       )
                     )
                   )
)
  
#ui
ui <- navbarPage("Home",
  homepg,
  cotpg,
  zoompg,
  conpg,
  sumpg
)

#server 
server <- function(input, output) {
  #cotpg
  output$cotscatter <- renderPlot({
    p <- ggplot(data = df1, aes(x=Year, y=df1[[input$trend]]))+
      geom_point()+
      geom_line()+
      labs(title=paste(input$trend, "over time"),
           y=input$trend)
    
    return(p)
  })
  
  #zoompg
  output$linePlot <- renderPlot({
    if ("Line Graph comparing the Mean Housing Cost over the years (1984 - 2019)" %in% input$diagram_input) {
      p <- ggplot(df, aes(x = Fiscal.Year, y = MEHOINUSA646N )) +
        geom_line() +
        labs(x = "Year", y = "Mean Housing Costs", title = "Mean Housing Costs Over Time")
    }
    
    else if ("Line Graph comparing the GDP over the years (1984 - 2019)" %in% input$diagram_input) {
      p <- ggplot(df, aes(x = Fiscal.Year, y = GDP_PC )) +
        geom_line() +
        labs(x = "Year", y = "GDP", title = "GDP Over Time")
    }
    
    return(p)
  })
  
  #contrast pg
  output$selectedOption1 <- renderText({
    paste("You have selected to compare life expectancy in the U.S in the years", 
          input$dropdown1, "and ", input$dropdown2)
  })
  
  output$scatter <- renderPlotly({
    selected_years <- c(input$dropdown1, input$dropdown2)  
    filtered_df <- df[df$Fiscal.Year %in% selected_years, ]  
    
    p <- ggplot(data = filtered_df, aes(x = Fiscal.Year, y = lifeExpectancy)) +
      geom_point(aes(color = Fiscal.Year)) +
      labs(x = "years", y = "Life Expectancy") +
      theme_minimal()
    
    p <- ggplotly(p, tooltip = "text")
    return(p)
    
  })
  
  #sumpg
  output$text <- renderText({
    "In conclusion, we want to study how the quality of life has changed over a 30 year period in the 
          United States as it relates to different factors. While the face value of the information
          is important; other calculations and facts must be considered."
  })
  
  output$plot <- renderPlot({
    ggplot(df, aes(x = GDP_PC, y = Fiscal.Year)) + 
      geom_point()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

