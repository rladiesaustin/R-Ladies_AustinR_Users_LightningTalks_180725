# CREATE A FUNCTION TO MAKE CHOROPLETH MAPS 

library(plotly)

create_choropleth_map <- function(df, plot_title, legend_title){ # assume df has a column for country names called "Country", and a column for the metric to map called "Metric"
  
  # black boundaries
  l <- list(color = toRGB("black"), width = 0.7)
  
  # specify map projection/options
  g <- list(
    showframe = TRUE,
    showcoastlines = TRUE,
    coastlinecolor = toRGB("grey"),
    showcountries = TRUE,
    countrycolor = toRGB("grey"),
    projection = list(type = 'Mercator')
  )
  
  
  # create the plot
  p <- plot_geo(df) %>% # pipe-operator
    add_trace(
      z = ~Metric, color = ~Metric, colors = 'Blues',
      text = ~Metric, locations = ~Country, marker = list(line = l), locationmode = 'country names' # other locationmode options include: "ISO-3", "USA-states"
    ) %>%
    colorbar(title = legend_title, tickprefix = '') %>%
    layout(
      title = plot_title,
      geo = g
    )
  
  return(p)
  
}

# SUPPORTING FUNCTIONS FOR MANIPULATING THE DATA USED IN THE SHINY APP 

library(tidyr)

getTitles <- function(df){ # return a dataframe whose rows are the input df column names separated into plot and legend titles (data name and unit name)
  
  # get names of columns with device data
  columns <- colnames(df)
  columns <- columns[3:length(columns)]
  
  # separate names into plot title and legend title
  titles <- separate(data.frame("columns" = columns), columns, c("legendTxt", "titleTxt"), "\\.\\.")
  titles$titleTxt <- sapply(titles$titleTxt, function(x) gsub("\\.", " ", x))
  titles$legendTxt <- sapply(titles$legendTxt, function(x) gsub("\\.", " ", x))
  
  # make the original column names the row names 
  rownames(titles) <- columns
  return(titles)
  
}

getYears <- function(df){ # return unique year values in df
  
  sort(unique(df$Year))
  
}

getDevices <- function(titles){ # return a named vector where the name is the title text and the data is the original column name 
  
  devices <- rownames(titles)
  names(devices) <- titles$titleTxt
  return(devices)
  
}

# CREATE THE SHINY APP

library(shiny)

# read in the data 
devices_df <- read.csv('MedicalDevices.csv', stringsAsFactors = FALSE)
titles <- getTitles(devices_df)

# Define UI for application that draws a map
ui <- fluidPage(
   
   # Application title
   titlePanel("Medical Devices Around the World"),
   
   # Sidebar with user inputs 
   sidebarLayout(
      sidebarPanel(
        selectInput("year", "Select Year:", getYears(devices_df)),
        selectInput("device", "Select Device:", getDevices(titles))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("map")
      )
   )
)

# Define server logic required to draw the map
server <- function(input, output) {
  
  plot_df <- reactive({ # get the data for the selected device and year
    selected_df <- devices_df[, c("Country", "Year", input$device)]
    colnames(selected_df)[colnames(selected_df)==input$device] <- "Metric"
    selected_df <- selected_df[(selected_df$Year == input$year)&!is.na(selected_df$Metric),]
    return(selected_df)
  })
  
   map_plot <- reactive({ # create the choropleth map for the selected inputs
     textDF <- titles[input$device,]
     temp <- create_choropleth_map(plot_df(), textDF$titleTxt, textDF$legendTxt)
     return(temp)
   })
   
   output$map <- renderPlotly({ # render the map output
     map_plot()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

