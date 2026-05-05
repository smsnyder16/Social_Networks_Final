
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# Mock Trial Network

library(shiny)
library(bslib)

library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)

library(visNetwork)


# Define UI for application that draws a histogram
ui <- navbarPage(
  "Mock Trial Network",
  
  # Introduction
  tabPanel("Introduction",
           fluidPage(
             h3("Introduction"),
             p("This is a network of the Middlebury mock trial team for the 2026 competition 
      year. It documents both formally assigned connections within the team (Attorney and Witness pairs
      for competition) and members' informal interactions outside of meetings."),
             p("From analyzing the different network structures that I created with this data,
I learned many things about the composition of Middlebury mock trial. *data analysis*"),
             p("The Data tab will tell you what is included in this data. It will include definitions
of each edge type as well as overall counts for each node attribute. Finally, it will detail the collection
process that I went through to obtain all of this data."),
             p("The Formal Network tab will present a network visualization of only the directed, formal edges
 between nodes. You can toggle through sizing nodes by betweeness and degree centrality. The Informal Network tab
instead shows only the undirected informal edges, and presents the same ability to choose betweeness or degree centrality to size nodes. The Full Network tab visualizes both formal and informal edges. It is directed to show directions of the
informal edges, and therefore shows every formal tie pointing both ways."),
             p("The interactive bar chart visualizes differences in the mean centrality measures of nodes depending on their role. 
You can choose between degree and betweeness centrality. The stacked bar chart is based on the 3 top clusters
in the informal network, and compares node attributes within clusters.")
 ) ),
  
  # Data Collection
  tabPanel("Data",
           fluidPage(
             h3("Data Description"),
             p("There are 28 nodes, 18 formal edges, and 196 informal edges."),
             
             h4("What is in this data?"),
             selectInput("select_connection", 
                         "Select an option", 
                         choices = list(
                           "Nodes" = "Members on the Middlebury mock trial team 2026",
                           "Formal edges" = "They were paired to compete together as an Attorney-Witness pair. Some members played multiple roles. These are undirected",
                           "Informal edges" = "They interact outside the club, weighted 1 for since mock trial ended, 2 for in the past month, and 3 for in the past week. These are directed."
                         )),
             textOutput("connection_text"), 
             
             h4("Node Attributes"),
             selectInput("select_node", 
                         "Select an option", 
                         choices = list(
                           "Year" = "Freshman: 19, Sophomore: 4, Junior: 3, Senior: 2",
                           "Major" = "Anthropology: 1, Biology: 1, Economics: 2,  English: 4, Gender Studies: 1, International and Global Studies: 1, International Politics and Economics: 6, Neuroscience: 1, Philosophy: 2, Political Science: 1, Political Science and Economics: 2, Undeclared: 6",
                           "Did mock trial in high school" = "Yes: 11, No: 17",
                           "Roles played" = "Attorney: 14, Witness: 11, Both: 3"
                         )),
             textOutput("node_text"),
             
             h3("Data Collection"),
             p("I sent out a survey to all 28 members. The survey first asked participants for their names, which were later replaced by random numbers. 
I then asked each participant their year, allowing 4 choices (freshman, sophomore, junior, and senior). 
 I asked them to write their major. I asked if they did mock trial in high school, only allowing a yes or no. 
 For the edges, I asked who they had talked to outside of mock trial and not about mock trial for 3 different time periods: 
since mock trial ended, in the past month, and in the past week. Participants were presented with a list of all 28 members on the team, 
instructed to click on whoever fit the description for each question, including a 'none of the above' option.")
           )
  ),
  
  # Formal Network
  tabPanel("Formal Network",
           fluidPage(
             h3("Formal Network"),
             p("This is  the formal network alone. Look at degree centrality and betweenness centrality, 
and notice first how the numbers are relatively low for both, showing how most nodes do not have many connections and very few play key bridge roles. 
Switch between the two network measures to see that those who are highly connected in the network also act as bridges between nodes."),
             selectInput("size_f", "Centrality measure",
                         choices = c("Degree Centrality" = "degree",
                                     "Betweenness Centrality" = "betweenness")),
             plotOutput("network_f", height = "500px")
           )
  ),
  
  # Informal Network
  tabPanel("Informal Network",
           fluidPage(
             h3("Informal Network"),
             p("This is the informal network alone. See how the degree and betweenness measurements 
  are generally much higher here, showing nodes have more connections and more frequently act as bridges 
  than in the formal network. Notice that some nodes with high degrees also have high betweenness, but 
  this does not apply to all of them. This means that not every highly connected node acts as a bridge."),
             
             selectInput("size_inf", "Centrality measure",
                         choices = c("Degree Centrality" = "degree",
                                     "Betweenness Centrality" = "betweenness")),
             plotOutput("network_inf", height = "500px")
           )
  ),
  
  # Full Network
  tabPanel("Full Network",
           fluidPage(
             h3("Full Network"),
             p("This is the full network. Color the nodes by attribute (Roles, mock trial experience, and year) to understand more about who is connected to who. 
Note that a few key attorneys are highly connected, but overall witness roles are more well connected (with higher average betweenness and degree centrality). This makes sense due to their generally outgoing and usually theatrical personalities; however, attorneys are mostly the ones in leadership roles, explaining the small, well connected group of them.
See that the majority of people did not do mock trial before getting to Middlebury, and those that didn't can be just as well connected as those who did not. 
Finally, note that we are predominantly a younger club, specifically freshman heavy. There are only 2 seniors and 3 juniors overall. Although the freshmen do have the majority of well connected spots because of their high numbers, you can see that key upperclassmen– likely those on the board– are also in well connected positions."),
             selectInput("node_attr", "Color nodes by:",
                         choices = c(
                           "Attorney vs Witness" = "Attorney.Witness",
                           "Did Mock Trial in high school" = "Did.mock.trial.in.high.school",
                           "Year" = "Year"
                         )),
             plotOutput("net_mt", height = "500px")
           )
  ),
  
  # Bar Chart
  tabPanel("Ineractive Bar Chart",
           fluidPage(
             radioButtons(
               "metric",
               "Centrality Measure",
               choices = c(
                 "Degree Centrality" = "mean_degree",
                 "Betweenness Centrality" = "mean_betweenness"
               )
             ),
             plotOutput("bar_plot", height = "600px")
           )
  ),
 
 
 tabPanel("Stacked Bar Charts",
          fluidPage(
            selectInput(
              "stack_var",
              "Choose attribute",
              choices = c(
                "Role" = "Attorney.Witness",
                "Year" = "Year",
                "Mock Trial in HS" = "Did.mock.trial.in.high.school"
              )
            ),
            plotOutput("stackedbar_plot", height = "600px")
          )
 )
)



# Section 2. The server section defines how our app works. Here's where we will put all the network analysis. 

server <- function(input, output) {
  
  # CARD 1 
  
  output$connection_text <- renderText({
    paste("", input$select_connection)
  })
  
  output$node_text <- renderText({
    paste("", input$select_node)
  })
  
  # CARD 2 
  
  #this is where you put read csv
  
  inf_network <- reactive({
    mock_nodes <- read.csv("~/Desktop/Social Networks/Mock_Trial/MT_nodes.csv") 
    mock_edges_inf <- read.csv("~/Desktop/Social Networks/Mock_Trial/MT_edges_informal.csv") 
    mock_edges_inf <- mock_edges_inf[, c("source", "target", "weight")]
    net_mt_inf <-  graph_from_data_frame(d=mock_edges_inf, vertices = mock_nodes, directed = TRUE)
    
    net_mt_inf <- net_mt_inf |> 
      as_tbl_graph()|> 
      activate(nodes) |> 
      mutate(
        degree = centrality_degree(), 
        betweenness = centrality_betweenness())
    
    net_mt_inf
  })
  
  f_network <- reactive({
    mock_nodes <- read.csv("~/Desktop/Social Networks/Mock_Trial/MT_nodes.csv") 
    mock_edges_f <- read.csv("~/Desktop/Social Networks/Mock_Trial/MT_edges_formal.csv")
    
    net_mt_f <- graph_from_data_frame(d=mock_edges_f, vertices = mock_nodes, directed = FALSE)
    
    net_mt_f <- net_mt_f |> 
      as_tbl_graph()|> 
      activate(nodes) |> 
      mutate(
        degree = centrality_degree(), 
        betweenness = centrality_betweenness())
    
    net_mt_f
  })
  
  network <- reactive({
    mock_nodes <- read.csv("~/Desktop/Social Networks/Mock_Trial/MT_nodes.csv") 
    
    mock_edges_inf <- read.csv("~/Desktop/Social Networks/Mock_Trial/MT_edges_informal.csv") |>
      mutate(type = "informal")
    
    mock_edges_f <- read.csv("~/Desktop/Social Networks/Mock_Trial/MT_edges_formal.csv") |>
      mutate(type = "formal") |> rename(source = Source, target = Target)
    
    mock_edges_f2 <- mock_edges_f |>
      rename(source = target, target = source)
    
    mock_edges_f <- bind_rows(mock_edges_f, mock_edges_f2)
    
    mock_edges_inf <- mock_edges_inf |> mutate(type = "informal") |>
      select(source, target, weight, type)
    
    mock_edges_f <- mock_edges_f |> mutate(weight = 1, type = "formal") |>
      select(source, target, weight, type)
    
    edges_mt_both <- rbind(mock_edges_inf, mock_edges_f)
    
    edges_mt_both <- edges_mt_both |> 
      mutate(type = factor(type, levels = c("informal", "formal")))
    
    net_mt <- graph_from_data_frame(d=edges_mt_both, vertices = mock_nodes, directed = TRUE)
    
    mt_tidy <- net_mt |> as_tbl_graph()|> activate(nodes) |> 
      mutate(degree = centrality_degree(), betweenness = centrality_betweenness())
    mt_tidy
  })


  
  output$network_inf <- renderPlot({
    net_mt_inf <- inf_network() 
    
inf<- ggraph(net_mt_inf, layout = "auto") +
   geom_edge_link(aes(width=weight), alpha = 0.8, color = "plum", 
arrow=arrow(length = unit(2, "mm")),end_cap = circle(2, "mm")) + 
      geom_node_point(aes(size = .data[[input$size_inf]]),
                      color = "orange", alpha=.9) +
scale_edge_width(range = c(.3, 1.5)) +
      scale_size_continuous(range = c(.5, 10)) + 
      labs(Nodes = input$size_inf) + 
      theme_graph()
    
    inf
  })
  
  output$network_f <- renderPlot({
    net_mt_f <- f_network() 
    
    f<- ggraph(net_mt_f, layout = "auto") +
      geom_edge_link(alpha = 0.7, color = "lightcoral") + 
      geom_node_point(aes(size = .data[[input$size_f]]),
                      color = "#A7D8F0") + 
      scale_size_continuous(range = c(.5, 10)) + 
      labs(Nodes = input$size_f) + 
      theme_graph()
    
    f
  })
  
  
  
  output$net_mt <- renderPlot({
    net_mt <- network() 
    
    mt<- ggraph(net_mt, layout = "auto") +
      geom_edge_link(aes(width = weight, color=type), alpha = 0.8, 
                     arrow=arrow(length = unit(2, "mm")),end_cap = circle(1.5, "mm")) + 
      geom_node_point(aes(size = degree, color= .data[[input$node_attr]])) + scale_edge_width(range = c(.3, 1.5)) + 
      scale_edge_color_manual(values= c(informal = "skyblue", formal = "grey30")) +
      scale_color_manual(values = c(
        "Attorney" = "red",
        "Witness" = "blue",
        "Both" = "purple",
        "Yes" = "green2",
        "No" = "red2",
        "Freshman" = "purple",
        "Sophomore" = "darkorange",
        "Junior" = "forestgreen",
        "Senior" = "darkred"
        
      ))
    
    mt + theme_void()
  })
  
  output$bar_plot <- renderPlot({
    
    mt_bar <- network() |> activate(nodes) |> as_tibble() |>
      group_by(Attorney.Witness) |> summarise(mean_degree = mean(degree),
                                              mean_betweenness = mean(betweenness)) |> select(Attorney.Witness, mean_degree, mean_betweenness) 
    
    
    ggplot(mt_bar, aes(x = Attorney.Witness, y = .data[[input$metric]], fill = Attorney.Witness)) +
      geom_col() + scale_fill_manual(values = c(
        "Attorney" = "red",
        "Witness" = "blue",
        "Both" = "purple")) + labs(
          title = "Average Degree and Betweenness by Role",
          x = "Role",
          y = "Average Value")
  })
  
  output$stackedbar_plot <- renderPlot({
      
mt_inf_clusters <- inf_network() |> activate(nodes) |>
mutate(cluster = group_walktrap(steps = 4)) |>
filter(cluster == 1 | cluster == 2 | cluster == 3)
      
cluster_bar <- mt_inf_clusters |> as_tibble() 
      
ggplot(cluster_bar, aes(
        x = factor(cluster), fill = .data[[input$stack_var]]
      )) + geom_bar() + scale_fill_manual(values = c(
        "Attorney" = "red",
        "Witness" = "blue",
        "Both" = "purple",
        "Freshman" = "purple",
        "Sophomore" = "darkorange",
        "Junior" = "forestgreen",
        "Senior" = "darkred",
        "Yes" = "green3",
        "No" = "red2")) +
        labs( x = "Cluster",
          y = "Number of people",
          fill = input$stack_var
        ) + theme_minimal()
    })

}


# Run the application 
shinyApp(ui = ui, server = server)

