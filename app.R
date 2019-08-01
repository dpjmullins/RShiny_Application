# Shiny App Code

## The 'app.R' file for each app should live in its own directory

# Libraries
library(shiny)
library(ggplot2)

## Set up dataset
dataset <- as.data.frame(Titanic)
total_passengers <- sum(dataset$Freq)
total_survivors <- sum(dataset[dataset$Survived == "Yes",]$Freq)

# Define UI (User Interface) function for the shiny app
## fluidPage() automaticallu adjusts to a user's window
ui <- fluidPage(

    ## Set the title of the app
    titlePanel("Investigating the survival rates on the Titanic based on travel class"),
  
    ## This create an app layout with a sidebar and a main area
    ## The sidebar typically contain input controls
    sidebarLayout(
  
        ## Sidebar panel for controlling inputs
        sidebarPanel(
            
            ## Select Class
            radioButtons(
                inputId = "Class",
                label = "Pick the class of travel:",
                choices = list(
                    `1st` = "1st",
                    `2nd` = "2nd",
                    `3rd` = "3rd",
                    `Crew` = "Crew"
                )
            )#,
  
        ),
  
        ## The main panel contains output elements
        mainPanel(
            h3("The percentage survival rates for the selected class"),
            p("Observe the effect of changing the", strong(span("class of travel", style = "color:green")), "in the opposite radio buttons on the plot."),

            ## Output Plot
            plotOutput(outputId = "survivalPlot")
  
        )
    )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
    
    ## The renderPlot() function makes this a reactive expression that changes the plot based on user input
    output$survivalPlot <- renderPlot({
      
        ## The data is subsetted different based on input at the following line
        data_subset <- dataset[which(dataset$Class %in% input$Class),]
        ## The percentage survival rate for the specific class is calculated
        survival_per <- sum(data_subset[data_subset$Survived == "Yes",]$Freq) / sum(data_subset$Freq)
        ## The survival rate across the overall boat is calculated
        survival_all <- sum(data_subset[data_subset$Survived == "Yes",]$Freq) / total_survivors 
        ## The data is formatted to be compatible with ggplot2
        plot_data.df <- 
            data.frame(
                survive = rep(c(paste("Percentage of",input$Class,"class that survived",sep=" "), "Percentage of total survivors"),times=2), 
                chances = rep(c(survival_per,survival_all),times=2),
                xaxis = rep(c(0,100),each=2)
            )


        outPlot <- 
            ggplot(data = plot_data.df, aes(x = xaxis, y = chances, label = paste(round(chances*100,1), "%", sep=" ")) ) +
            ## Area plot which will be filled under the line
            geom_area(fill = "skyblue", colour = "skyblue") +
            ## Set up y-axis
            geom_hline( aes(yintercept = 1), colour = "black" ) +
            scale_y_continuous(name = "variable", limits = c(0, 1)) +
            ## Create facets
            facet_wrap(~survive) +
            ## Convert to circular plot
            coord_polar(theta = "x") +
            ## Annotate percentages
            geom_text(aes(y=0), size = 5) +
            theme_bw() + theme(
                panel.grid = element_blank(), panel.border = element_blank(),
                axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
                text = element_text(size=20)
            )
        outPlot
    })

}

## Call the ui and server functions
shinyApp(ui = ui, server = server)

## Code to run app
### 'display.mode = "showcase"' displays the code alongside the app
#runApp("../shiny_app", display.mode = "showcase")
