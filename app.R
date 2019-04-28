# Capstone project 
# ESG Radar 
# Dashboard test 0.1 




# Required libraries 
library(shiny)
library(shinydashboard)
library(readr)
library(ggplot2)
library(ggthemes)
library(ggmap)
library(PBSmapping)
library(plotly)
library(dplyr)
library(tidyr)
library(forcats)
library(quantmod)
library(dygraphs)
library(xts)
#library(newsanchor)



######################## DATA ###############################

# Importing database from sustainalytics 
db <- read.csv("esg_scores_8Feb.csv") 

# Updating column name 
colnames(db)[1] <- "company_id"

# Updating dates
db$value_date <- as.Date(db$value_date, "%m/%d/%Y")


# Importing predictions for MDD 
mdd.p <- read.csv("p_mdd.csv") %>% 
  select(name= `ï..name`, p_mdd18m)

# Importing predictions for BS
bs.p <- read.csv("p_bs1y.csv") %>% 
  select(name= `ï..name`, X0, X1)

# Creating filter list
db.region <- db %>% distinct(region)
db.region.all <- c("All", levels(db.region$region))

db.size <- db %>% distinct(size)
db.size.all <- c("All", levels(db.size$size))

db.gics_sector <- db %>% distinct(gics_sector)
db.gics_sector.all <- c("All", levels(db.gics_sector$gics_sector))

db.ticker <- db %>% distinct(ticker)
db.name <- db %>% distinct(name)


# Importing the scores names
scores_names <- read_csv("C:/Users/Rebec/Desktop/MSBA/Capstone/Deployment/Dashboard_test/scores_names.csv")



###################### CHART SPECT ########################

# Pallete 
cbPalette <- c("#164D73", "#F7BD36", "#E04E70", "#164D73", "#8B8F9A")

# Set up chart settings
chartcolor <- "#8B8F9A"
boxlinecolor <- "#333333"
chartcolorpink <- "#E04E70"
chartcolorblue <- "#164D73"
chartcolorgold <- "#F7BD36"
chartcolorgreen <- "#16BF89"

# Basic charts specs 
myattributes <- theme_bw() +
  theme(legend.title=element_blank()) + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(color = "grey"), 
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),
        plot.caption=element_text(size=9,color = "grey")
  )



####################### APP ################################


# App UI broken into three key parts: Header, Sidebar and Body 


#################### APP HEADER ############################

# App header 
header <- dashboardHeader(
    title = "ESG Radar",
    titleWidth = 150,
    dropdownMenu(type = "notifications",
                 notificationItem(
                     text = "Breaking news",
                     icon("bolt")),
                 notificationItem(
                     text = "Check out the investment tips",
                     icon("comments-dollar"),
                     status = "success"),
                 notificationItem(
                     text = "Renew the suscription today",
                     icon = icon("exclamation-triangle"),
                     status = "warning")
    )
    )




#################### SIDE BAR ############################


# Side bar options 
sidebar <- dashboardSidebar(
  width = 150,
  sidebarMenu(
    menuItem("Home",
             tabName = "home",
             icon = icon("home")),
    
    # Dashboard menu bar 
    menuItem("Dashboard", 
             tabName = "dashboard", 
             icon = icon("dashboard"),
             menuSubItem('ESG Exploration',
                         tabName = 'subitem1',
                         icon = icon('line-chart')
             ),
             menuSubItem('Stock Search',
                         tabName = 'subitem2',
                         icon = icon('search')
             ),
             menuSubItem('Radar indicators',
                         tabName = 'subitem3',
                         icon = icon('business-time')
             ),
             menuSubItem('Radar Models',
                         tabName = 'subitem4',
                         icon = icon('lightbulb'))
    ),
    
    # Insights menu bar 
    menuItem("Insights", 
             tabName = "dashboard", 
             icon = icon("chess-knight"),
             menuSubItem('ESG Radar glance',
                         tabName = 'subitem9',
                         icon = icon('bullseye')
             ),
             menuSubItem('ESG Overview',
                         tabName = 'subitem10',
                         icon = icon('globe-americas')
             ),
             menuSubItem('The model',
                         tabName = 'subitem11',
                         icon = icon('seedling')
             ),
             menuSubItem('Key findings',
                         tabName = 'subitem12',
                         icon = icon('comment-dollar'))
    ),
    
    # Resources menu bar 
    menuItem("FAQ", 
             tabName = "faq",
             badgeLabel = "new",
             badgeColor = "green",
             icon = icon("question-circle"))
  ),
  
  # Custom CSS to hide the default logout panel
  tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))
  )
)




#################### APP BODY ############################

# App body 
body <- dashboardBody(
  
  # color as the rest of the header.
  tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
          background-color: #16BF89;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #16BF89;
        }
        .navbar {
          background-color: #263238 !important;
        }
      '))),
  
  # Box colors
  tags$style(HTML("
        .box.box-danger {
          border-top-color:#E04E70;}
                    
        .box.box-warning {
          border-top-color:#F7BD36;}
                    
        .box.box-success {
          border-top-color:#16BF89;}
                    
        .box.box-primary {
          border-top-color:#164D73;}
          
        .box.box-solid.box-danger>.box-header {
          background:#E04E70;}
                    
        .box.box-solid.box-warning>.box-header {
          background:#F7BD36;}
                    
        .box.box-solid.box-success>.box-header {
          background:#16BF89;}
                    
        .box.box-solid.box-primary>.box-header {
          background:#164D73;}
          
        .box.box-solid.bg-red>.box-header {
          background:#E04E70;}
                    
        .box.box-solid.bg-yellow>.box-header {
          background:#F7BD36;}
                    
        .box.box-solid.bg-green>.box-header {
          background:#16BF89;}
                    
        .box.box-solid.bg-light-blue>.box-header {
          background:#164D73;}"
  )),
  
  # InfoBox colors
  tags$style(
    type = 'text/css',
    '.bg-light-blue {background-color: #164D73!important; }'
  ),
  
  # Tab options from menu 
  tabItems(
    
    ################## HOME - BODY 
    tabItem(tabName = "home",
            fluidRow(img(src='Logo.png', align = "center")),
            br(),
            br(),
            
            # First column
            fluidRow(column(width = 6,
              box(title = "ESG Exploration", 
                         status = "warning", 
                         collapsible = TRUE,
                         width = NULL,
                         collapsed = TRUE,
                         solidHeader = TRUE,
                         "This dashboard focuses on ESG scores that can be filtered by the three main subsets utilized in our research (region, sector and size). Investors can use this section to understand which are the companies with the best ESG ranking overall and the regions with highest ESG scores based on the filters selection. Besides, they can also review ESG scores trends for the same filter choice."),
              
              box(title = "Stock Search", 
                        status = "primary", 
                        collapsible = TRUE,
                        width = NULL,
                        collapsed = TRUE,
                        solidHeader = TRUE,
                        "When investors are looking to understand a given company ESG performance and overall financial performance, they can use this dashboard to review this company’s main KPIs and trends. By choosing a company name, they will be able to analyse the overall scores, areas of concern based on sub scores, traditional financial metrics (such as P/E ratio, market cap and stock price) along with our model metrics (MDD_m18 and Black Swan). Moreover, they could review ESG scores trends and closing stock price trends."),
            
              box(title = "Radar Indicators", 
                        status = "danger", 
                        collapsible = TRUE,
                        width = NULL,
                        collapsed = TRUE,
                        solidHeader = TRUE,
                        "For investors looking to find opportunities of investment while managing risk, this section can support their needs. By selecting the level of exposure they feel more comfortable with for MDD_m18 they would be able to review a list of companies and their main metrics. Along with that, we have included other indicators that could be of interest for investors from an ethical point of view, but also significant in the prediction of MDD_m18 scores according to our research, such as “Carbon intensity” (sub score for Environmental scores), “Board diversity” (sub score for Social scores) and “Employee-Related Controversies or Incidents” (sub score for Governance scores). "),
              
              box(title = "Radar Models", 
                         status = "success", 
                         collapsible = TRUE,
                         width = NULL,
                         collapsed = TRUE,
                         solidHeader = TRUE,
                         "Investors interested in finding top and bottom predictions of companies according to specific regions, sectors or size, can use this dashboard to find the answer. They can also use the bottom table, which can be sorted by different metrics, to get a comprehensive list of companies to find interesting investment opportunities while mitigating potential risks.")),
            
              # News box
              column(width = 6,
                         box(title = "News",
                         width = NULL,
                         color = "grey"
                         #uiOutput("news.tab")
                         ))
                            )),
    
    tabItem(tabName = "dashboard",
            h2("Dashboard tab content")),
    
    
    
    ########################   PAGE 1 - BODY 
    tabItem(tabName = "subitem1",
            fluidRow(box(title = "Exploration Dashboard", 
                         width = 12,
                         status = "warning", 
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         column(4,selectInput('inregion', 'Regions', 
                                              choices = db.region.all, 
                                              selected = db.region.all[1], 
                                              selectize=TRUE)),
                         column(4,selectInput('insector', 'Industry', 
                                              choices = db.gics_sector.all, 
                                              selected = db.gics_sector.all[1], 
                                              selectize=TRUE)),
                         column(4,selectInput('insize', 'Company size',
                                              choices = db.size.all, 
                                              selected = db.size.all[1],
                                              selectize=TRUE)))),
            
            # Chart on the left 
            fluidRow(box(title = "Ranking of Companies based on ESG scores", 
                         status = "warning", 
                         collapsible = TRUE,
                         plotOutput(outputId = "esg.rank", height = 250)),
                     
                     # Chart on the right     
                     box(title = "Map overview per country", 
                         status = "warning", 
                         collapsible = TRUE,
                         plotOutput("geomap", height = 250))),
            
            # Trends chart
            fluidRow(box(title = "ESG score trends", 
                         width = 12, 
                         status = "warning", 
                         collapsible = TRUE,
                         dygraphOutput("esg.trends", height = 250)))),
    
    
    
    #########################   PAGE 2 - BODY
    tabItem(tabName = "subitem2",
            
            # Heather of page 2 
            fluidRow(box(title = "ESG Radar Stock Search", 
                         width = 12,
                         status = "primary", 
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         column(4,selectInput('incompany', 'Company name', 
                                              c(Choose='', db.name),
                                              selected = 'Alphabet Inc',
                                              selectize=TRUE)))),
            
            # Info boxes of the stocks 
            fluidRow(infoBoxOutput("comp.region"),
                     infoBoxOutput("comp.industry"),
                     infoBoxOutput("comp.size")),
            
            # Chart on the left 
            fluidRow(box(title = "Company Overall scores", 
                         status = "primary", 
                         collapsible = TRUE,
                         plotOutput(outputId = "comp.scores.chart", height = 250)),
                     
                     # Chart on the right     
                     box(title = "Company Areas of concern", 
                         status = "primary", 
                         collapsible = TRUE,
                         plotOutput("comp.concern.chart", height = 250))),
            
            # Title above the Two charts box
            fluidRow(box(title = "Company KPIs",
                         width = 12,
                         height = 40,
                         background = "light-blue"
            )),
            
            # Table 
            tabPanel('Financial indicators',DT::dataTableOutput('ex2')),
            
            # Title above the Two charts box
            fluidRow(box(title = "Trends for Stock Price & ESG Scores",
                         width = 12,
                         height = 40,
                         background = "light-blue"
            )),
            
             # Two charts in one place 
            fluidRow(
              tabBox(
                title = "Trends over time",
                width = 12,
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1", 
                tabPanel("Stock price", 
                         dygraphOutput("stock.chart", 
                                       height = 300)),
                tabPanel("ESG scores", 
                         dygraphOutput("esg.chart", 
                                       height = 300))
              )
            )
            ),
    
    ################################ PAGE 3 - BODY
    tabItem(tabName = "subitem3",
            fluidRow(box(
              title = "Radar indicators",
              width = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              status = "danger",
              sliderInput("slider", "Select your MDD tolerance level:", 
                          min = 0, max = 1,
                          value = 0.5, step = 0.1, 
                          width = 400))),
            
            
            # Title above the Two charts box
            fluidRow(box(title = "Companies KPIs",
                         width = 12,
                         height = 40,
                         background = "red"
            )),
            
              tabPanel('Overview',DT::dataTableOutput('table_3'))
            ),
    
    ################################ PAGE 4 - BODY
    tabItem(tabName = "subitem4",
            fluidRow(box(title = "ESG Radar Models", 
                         width = 12,
                         status = "success", 
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         column(4,selectInput('inregion2', 'Regions', 
                                              choices = db.region.all, 
                                              selected = db.region.all[1], 
                                              selectize=TRUE)),
                         column(4,selectInput('insector2', 'Industry', 
                                              choices = db.gics_sector.all, 
                                              selected = db.gics_sector.all[1], 
                                              selectize=TRUE)),
                         column(4,selectInput('insize2', 'Company size',
                                              choices = db.size.all, 
                                              selected = db.size.all[1],
                                              selectize=TRUE)))),
            # Chart on the left 
            fluidRow(box(title = "Highest MDD Risk", 
                         status = "success", 
                         collapsible = TRUE,
                         plotOutput(outputId = "mdd.p.topchart", height = 250)),
                     
                     # Chart on the right     
                     box(title = "Lowest MDD Risk", 
                         status = "success", 
                         collapsible = TRUE,
                         plotOutput("mdd.p.bottomchart", height = 250))),
            # Title above the table
            fluidRow(box(title = "Companies overview",
                         width = 12,
                         height = 40,
                         background = "green"
            )),
            
            # Table 
            tabPanel('Overview',DT::dataTableOutput('ex1'))
            ),
    
    
    ################################## SUB 1 - BODY 
    tabItem(tabName = "subitem9",
            h2("ESG Radar at a glance"),
            br(),
            br(),
            # Upper right box
            fluidRow(box(title = span("ESG Scores", 
                                      style = "color: #164D73; font-size: 24px"),
                         status = "primary", 
                         height = 180,
                         collapsible = TRUE,
                         "ESG investing refers to the analysis of corporate environmental, social and governance factors to help make investment decisions. By analyzing ESG data, investors can potentially identify companies at risk of ESG incidents to create better investment portfolios.
"),
                     
                     # Upper left box     
                     box(title = span("Sustainalytics", 
                                      style = "color: #F7BD36; font-size: 24px"),
                         status = "warning", 
                         height = 180,
                         collapsible = TRUE,
                         "Sustainalytics ESG ratings measure how well companies manage environmental, social and governance issues that are most relevant to their industry. Each company is rated on over 70 indicators that range in score from 0-100, and roll up into the Environmental (E), Social (S), and Governance (G) pillars. In ESG Radar we have incorporated 10 years of data from Sustainalytics into our models and application. 
")),
            
            # Lower right box
            fluidRow(box(title = span("Models & Value proposition", 
                                      style = "color: #16BF89; font-size: 24px"),
                         status = "success", 
                         height = 180,
                         collapsible = TRUE,
                         "ESG Radar can be incorporated into investment portfolios to reduce overall risk from ESG factors. This information is valuable to investors, money managers, and companies that wish to proactively identify points of weakness and protect long-term shareholders."),
                     
                     # Lower left box     
                     box(title = span("Disclaimer", 
                                      style = "color: #E04E70; font-size: 24px"),
                         status = "danger", 
                         height = 180,
                         collapsible = TRUE,
                         "Our research shows that ESG factors are better at predicting downside risk than returns. Companies with poor ESG practices are more likely to experience downside risk, but companies with good ESG practices may not be more likely to outperform the overall market. Still, by removing downside risk from our investment portfolios we can improve our long-term investment performance with a lower risk profile."))),
    
    
    
    ################################## SUB 2 - BODY 
    tabItem(tabName = "subitem10",
            h2("ESG Overview"),
            br(),
            # Upper right box
            fluidRow(column(width = 12,
                    # Environment
                    box(img(src='environment.png', align = "center"),
                        status = "success", 
                        height = 150,
                        width = NULL,
                        br(),
                        br(),
                        "Environmental factors measure a company’s impact on the environment, including land, air, and water. Important issues include air pollution, carbon emissions, water scarcity, deforestation, and waste management."),
                     
                    #Social
                     box(img(src='social.png', align = "center"),
                         status = "warning", 
                         width = NULL,
                         height = 150,
                         br(),
                         br(),
                         "Social factors examine how a company manages relationships and builds trust with its employees, customers, and society. Important issues include gender diversity, data protection, labor standards, community relations, and health and safety."),
                     
                     # Governance     
                     box(img(src='governance.png', align = "center"), 
                         status = "primary", 
                         width = NULL,
                         height = 150,
                         br(),
                         br(),
                         "Governance factors look at a company’s structure and processes to ensure proper and responsible leadership. Important issues include executive pay, bribery and corruption, board diversity, business ethics, and political lobbying.")))),
    
    ################################## SUB 3 - BODY 
    tabItem(tabName = "subitem11",
            h2("The model behind the app"),
            br(),
            # Upper right box
            fluidRow(column(width = 12,
                            
                            #MDD
                            box(img(src='MDD.png', align = "center"),
                                status = "success", 
                                width = NULL,
                                br(),
                                br(),
                                tags$b("What is MDD?"),
                                br(),
                                "Maximum drawdown is defined as the maximum peak-to-trough decline of an investment during a specific time period.",
                                br(),
                                br(),
                                tags$b("How is the MDD score calculated?"),
                                br(),
                                "For MDD, we generated 1w, 1m, 3m, 6m, 18m, 1y and 3y values from each score date and selected 18m for the application as it had the best performance in our models.",
                                br(),
                                br(),
                                tags$b("How to read this metric?"),
                                br(),
                                "The MDD scores varies from zero to one. The closer the metric to one is indicating a higher prediction of a possible decline within 18 months.",
                                br()),
                            
                            # BS     
                            box(img(src='BS.png', align = "center"), 
                                status = "success", 
                                width = NULL,
                                br(),
                                br(),
                                tags$b("What is a Black Swan?"),
                                br(),
                                "A black swan is defined by Investopedia as “an event or occurrence that deviates beyond what is normally expected of a situation and is extremely difficult to predict. Black swan events are typically random and unexpected.” The common theme for each of these incidents was an accident or announcement, followed by a large and rapid decline in stock price.", 
                                br(),
                                br(),
                                tags$b("How is the Black Swan metric calculated?"),
                                br(),
                                "We used maximum drawdown as our primary indicator, along with a methodology that would identify events having a 0.01% chance of occurring.",
                                br(),
                                br(),
                                tags$b("How to read this metric?"),
                                br(),
                                "When the metric is zero it means that the model has not identified any probability of such an event, numbers higher than zero express the likelihood of such an event to happen every 1000. Note that the probabilities of these kind of events are very low in most of the cases that is why they are expressed in thousands.",
                                br())))),
    
    
    ################################## SUB 3 - BODY 
    tabItem(tabName = "subitem12",
            h2("Key findings from our research"),
            br(),
            # Upper right box
            fluidRow(column(width = 12,
                            
                            #MDD
                            box(title = tags$b("Key take-aways"),
                                status = "success", 
                                width = NULL,
                                tags$b("Return vs. Risk predictions performance"),
                                br(),
                                "* ESG factors are better at predicting downside risk than returns. This indicates that ESG factors may have more value in forecasting the downside as opposed to the upside.",
                                br(),
                                br(),
                                tags$b("Better performance for long-term"),
                                br(),
                                "* ESG factors work well for longer time periods (18m, 36m) versus shorter periods (1m). This indicates that non-financial factors can be an important factor in long-term investment performance, but are less likely to have impact over shorter periods.",
                                br(),
                                br(),
                                tags$b("Environmental factors influence"),
                                br(),
                                "* From our analysis, we find that environmental factors are of particular importance to our predictive models. Carbon intensity was one of the most relevant factors in all three of our models.",
                                br(),
                                br(),
                                tags$b("Company size influence in the predictions"),
                                br(),
                                "* Our models were better at predicting performance for large and mid cap stocks as opposed to small caps. This might be due to difficulty in obtaining ESG data on smaller companies.",
                                br(),
                                br(),
                                tags$b("Differences across sectors or industries"),
                                br(),
                                "* Our models found that some sectors worked better than others. Utilities was the best performing sector, and Healthcare was the worst performer.")))),
    
    tabItem(tabName = "faq",
            h2("FAQ tab content"),
            br(),
            # First column
            fluidRow(box(title = "Why should I use ESG metrics to make investment decisions?", 
                                status = "success", 
                                collapsible = TRUE,
                                width = 12,
                                collapsed = TRUE,
                                solidHeader = TRUE,
                                "ESG metricas can be incorporated into investment portfolios to reduce overall risk and improve investment performance. ESG metrics can be useful as a tool to make investment decision along with traditional financial indicators."),
                            
                            box(title = "How is ESG Radar different from a scoring data provider?", 
                                status = "success", 
                                collapsible = TRUE,
                                width = 12,
                                collapsed = TRUE,
                                solidHeader = TRUE,
                                "ESG Radar main goal is to identify factors that have a meaningful impact on investment performance and predict future performance based on this information. We use scoring data from Sustainalytics along with models that help us to make predictions based on that information. ESG Radar enables investors to effectively evaluate the impact of ESG factors on their investment decisions."),
                            
                            box(title = "Which are the key metrics I should be looking at?", 
                                status = "success", 
                                collapsible = TRUE,
                                width = 12,
                                collapsed = TRUE,
                                solidHeader = TRUE,
                                "The answer is depends. On your financial goals and priorities. We suggest that you spend sometime in the Insights tab where you can find detailed information about ESG along with the metrics that our model provides."),
                            
                            box(title = "How can I contact the support team?", 
                                status = "success", 
                                collapsible = TRUE,
                                width = 12,
                                collapsed = TRUE,
                                solidHeader = TRUE,
                                "You can email us at esg.radar@blackswanproject.com")))
  )
)



################################ SERVER ############################

server <- function(input, output) {
  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
      color = "purple"
    )
  })

  
  ################################################ PAGE  - HOME
  
  # save the api_key in the .Renviron file
  #set_api_key(api_key = "TOKEN NUMBER GOES HERE", 
  #            path = "~/.Renviron")
  
  
  ## Data adjustment 
  #finance_journals <- c("bloomberg", "business-insider", "financial-post", "financial-times", "fortune", "the-economist", "the-wall-street-journal")
  # results <- get_headlines(sources = finance_journals)[[2]]
  # 
  # ## Table
  # top_results <- results %>%
  #   select(title, url) %>%
  #   top_n(n= 5)
  # 
  # headlines <- top_results$title
  # news.url <- top_results$url
  # 
  # url.1 <- p(a(headlines[1], href=news.url[1]))
  # url.2 <- p(a(headlines[2], href=news.url[2]))
  # url.3 <- p(a(headlines[3], href=news.url[3]))
  # url.4 <- p(a(headlines[4], href=news.url[4]))
  # url.5 <- p(a(headlines[5], href=news.url[5]))
  # 
  # output$news.tab <- renderUI({
  #   tagList(url.1, url.2, url.3, url.4, url.5)
  # })
  # 
  ############################################### PAGE 1 - CHARTS  
  
  ############1.1. Ranking of companies for ESG scores
  output$esg.rank <- renderPlot({
    
    # Data conditional 
    # Conditional for dataset 
    inputinregion <- input$inregion
    
    if (inputinregion == 'All')
    {
      # All
      filterRegion <- !vector(mode="logical", length=dim(db)[1])
    } else {
      # Filter region
      filterRegion <- db$region == inputinregion
    }
    
    
    inputinsize <- input$insize
    
    if (inputinsize == 'All')
    {
      # All
      filtersize <- !vector(mode="logical", length=dim(db)[1])
    } else {
      # Filter region
      filtersize <- db$size == inputinsize
    }
    
    
    inputinsector <- input$insector
    
    if (inputinsector == 'All')
    {
      # All
      filtersector <- !vector(mode="logical", length=dim(db)[1])
    } else {
      # Filter region
      filtersector <- db$gics_sector == inputinsector
    }
    
    # Filtering
    rank <- db %>%
      filter(filterRegion & 
               value_date == '2018-08-01' & 
               filtersize &
               filtersector) %>%
      select(name, ESG) %>%
      mutate(ESGmean = round(mean(ESG),1)) %>% 
      top_n(n= 10, wt = ESG) %>%
      arrange_(~ desc(ESG))
    
    # Chart 
    esg.rank <- ggplot(data= rank, aes(y= ESG, x= reorder(name,ESG))) +
      geom_bar(stat = "identity", fill=chartcolor) +
      coord_flip() +
      myattributes +
      labs(title="",
           subtitle="",
           caption="Source: Sustainalytics",
           x="Companies",
           y="Last score") + 
      geom_errorbar(data= rank, 
                    aes(name, ymax = ESGmean, ymin = ESGmean),
                    size=1, 
                    inherit.aes = F, 
                    width = 1,
                    color=chartcolorpink)
    print(esg.rank)
  }, height="auto")
  
  
  
  
  ################# 1.3. 1.2. ESG heat map
  output$geomap <- renderPlot({
    
    # Data conditional 
    # Conditional for dataset 
    inputinregion <- input$inregion
    
    if (inputinregion == 'All')
    {
      # All
      filterRegion <- !vector(mode="logical", length=dim(db)[1])
    } else {
      # Filter region
      filterRegion <- db$region == inputinregion
    }
    
    
    inputinsize <- input$insize
    
    if (inputinsize == 'All')
    {
      # All
      filtersize <- !vector(mode="logical", length=dim(db)[1])
    } else {
      # Filter region
      filtersize <- db$size == inputinsize
    }
    
    
    inputinsector <- input$insector
    
    if (inputinsector == 'All')
    {
      # All
      filtersector <- !vector(mode="logical", length=dim(db)[1])
    } else {
      # Filter region
      filtersector <- db$gics_sector == inputinsector
    }
    
    # Creating the data frame
    geo.country <- db %>%
      filter(value_date == '2018-08-01' & 
               filterRegion &
               filtersize &
               filtersector) %>%
      mutate(country = fct_recode(country,
                                  "USA" = "United States",
                                  "Great Britain" = "United Kingdom")) %>%
      group_by(country) %>%
      summarize(n=mean(ESG, na.rm = TRUE)) %>% 
      #count(country)  %>% 
      arrange_(~ desc(n))
    
    # Transfor data frame
    country.data <- data.frame(region = factor(geo.country$country), data = geo.country$n)
    
    # Merging data with map data 
    mapdata <- map_data("world")
    mapdata <- left_join(mapdata, country.data, by="region")
    
    #clip polygons to map
    colnames(mapdata)[1:6] <- c("X","Y","PID","POS","region","subregion")
    
    # Map limits
    xlim<-c(-180, 180)
    ylim<-c(-70, 90)
    
    # Map chart
    geomap <- ggplot()+
      coord_map(xlim=xlim,ylim=ylim) +
      geom_polygon(data=mapdata,
                   aes(x=X, y=Y,
                       group=PID, 
                       fill=data),
                   alpha=0.6) +  
      theme(axis.title.x=element_blank(), 
            axis.text.x=element_blank(), 
            axis.ticks.x=element_blank()) + 
      theme(axis.title.y=element_blank(), 
            axis.text.y=element_blank(), 
            axis.ticks.y=element_blank()) + 
      scale_fill_continuous(guide = guide_colourbar(title = "mean of ESG score"))
    
    geomap
  })
  
  
  
  
  ####################### 1.3. Trends ESG scores
  output$esg.trends <- renderDygraph({
    
    # Conditional for dataset 
    inputinregion <- input$inregion
    
    if (inputinregion == 'All')
    {
      # All
      filterRegion <- !vector(mode="logical", length=dim(db)[1])
    } else {
      # Filter region
      filterRegion <- db$region == inputinregion
    }
    
    
    inputinsize <- input$insize
    
    if (inputinsize == 'All')
    {
      # All
      filtersize <- !vector(mode="logical", length=dim(db)[1])
    } else {
      # Filter region
      filtersize <- db$size == inputinsize
    }
    
    
    inputinsector <- input$insector
    
    if (inputinsector == 'All')
    {
      # All
      filtersector <- !vector(mode="logical", length=dim(db)[1])
    } else {
      # Filter region
      filtersector <- db$gics_sector == inputinsector
    }
    
    ## Data adjustment 
    trend.chart <- db %>%
                        filter(filterRegion &
                               filtersize &
                               filtersector) 
    
    # Summarizying data by specific criteria 
    trend.chart <- group_by(trend.chart, value_date) %>%
      summarise_at(.vars = vars(ESG,E,S,G),
                   .funs = c(mean="mean")) %>%
      gather(score_type,score, ESG_mean, E_mean, S_mean, G_mean) 
    
    # Transforming the data into a data frame 
    trend.chart <- data.frame(trend.chart)
    trend.chart <- reshape(trend.chart, idvar = "value_date", timevar = "score_type", direction = "wide") 
    names(trend.chart)[names(trend.chart) == "score.ESG_mean"] <- "ESG"
    names(trend.chart)[names(trend.chart) == "score.E_mean"] <- "E"
    names(trend.chart)[names(trend.chart) == "score.S_mean"] <- "S"
    names(trend.chart)[names(trend.chart) == "score.G_mean"] <- "G"
    
    # Updating date format 
    library(xts)
    trend.chart <- xts(x=trend.chart, order.by=trend.chart$value_date)
    trend.chart$value_date <- NULL
    
    
    ## Chart
    esg.trends <- dygraph(trend.chart, main="Evolution of ESG Scores by Sustainalytics ranking") %>%
      dyAxis("y", label = "Scores") %>%
      dyOptions(axisLineWidth = 1.5, 
                drawGrid = TRUE) %>%
      dyAxis("y", label = "Scores") %>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.2) %>%
      dyRangeSelector()  %>%
      dyGroup(c("ESG",
                "E" ,
                "S", 
                "G"), 
              color = c(chartcolorpink, 
                        chartcolorgreen, 
                        chartcolorgold, 
                        chartcolorblue),
              strokeWidth=3)
    
    print(esg.trends)
    
  })
  
  
  
  
  ########################################## PAGE 2 - CHARTS ######
  
  ########2.1. Company information box 
  # Box 1 
  output$comp.region <- renderInfoBox({
    
    # Data procesing 
    comp.info <- db %>%
      filter(name == input$incompany,
             value_date == '2018-08-01') %>%
      select(ticker, name, region, country, size, 
             industry = gics_sector)
    
    comp.region <- comp.info$region 
    
    # Box outcome
    infoBox(
      "Region",
      comp.region, 
      icon = icon("map-marker-alt"),
      color = "light-blue")
  })
  
  # Box 2
  output$comp.industry <- renderInfoBox({
    
    # Data pre procesing 
    comp.info <- db %>%
      filter(name == input$incompany,
             value_date == '2018-08-01') %>%
      select(ticker, name, region, country, size, 
             industry = gics_sector)
    
    comp.industry <- comp.info$industry
    
    # Box outcome
    infoBox(
      "Industry",
      comp.industry, 
      icon = icon("industry"),
      color = "light-blue")
  })
  
  # Box 3  
  output$comp.size <- renderInfoBox({
    
    # Data procesing 
    comp.info <- db %>%
      filter(name == input$incompany,
             value_date == '2018-08-01') %>%
      select(ticker, name, region, country, size, 
             industry = gics_sector)
    
    comp.size <- comp.info$size
    
    # Box outcome
    infoBox(
      "Size",
      comp.size, 
      icon = icon("chart-bar"),
      color = "light-blue")
  })
  
  
  ########2.2. Overall scores
  output$comp.scores.chart <- renderPlot({
    
    ## Data adjustment --> Overall scores for given company vs. average
    comp.scores <- db %>%
      mutate(ESGmean.t = round(mean(ESG),1)) %>% 
      filter(name == input$incompany,
             value_date == '2018-08-01') %>%
      select(name, ESG, E, S, G, ESGmean.t) %>%
      gather(score_type,score, ESG, E, S, G)
    comp.scores
    
    # Changing score type to factor 
    comp.scores$score_type <- as.factor(comp.scores$score_type)
    
    
    ## Chart
    comp.scores.chart <- ggplot(data= comp.scores, 
                                aes(y= score, 
                                    x= reorder(score_type, score),
                                    fill= score_type)) +
      geom_bar(stat= "identity") +
      scale_fill_manual(values= c(ESG = chartcolorpink,
                                  E = chartcolorgreen,
                                  S = chartcolorgold,
                                  G = chartcolorblue)) +
      labs(title="",
           subtitle="",
           caption="Source: Sustainalytics",
           x="Score types",
           y="Latest scores") + 
      myattributes + 
      coord_flip() +
      theme(legend.direction = "horizontal", 
            legend.position = "bottom") +
      geom_errorbar(data= comp.scores, 
                    aes(score_type, 
                        ymax = ESGmean.t, 
                        ymin = ESGmean.t),
                    size=1, 
                    inherit.aes = F, 
                    width = 1,
                    color=boxlinecolor)

    print(comp.scores.chart)
  })
  
  ########## 2.5 Stock price evolution for a given company
  
  # The currently selected tab from the first box
  output$stock.chart <- renderDygraph({
    
    # Initial preparation  
    stock.data <- new.env()
    start.date <- as.Date("2001-01-01")
    end.date <- as.Date("2018-08-31")
    
    # Define tickers to look for 
    tickers <- c("GOOGL") 
    
    # Looking for all the information 
    getSymbols(tickers, env = stock.data, src = "yahoo", from = start.date, to = end.date)
    
    # Selecting closing prices
    stock.price <- Cl(stock.data$GOOGL)
    
    # Chart 
    stock.chart <- dygraph(stock.price, main="Evolution of prices for the selected stock") %>%
      dyAxis("y", label = "Prices") %>%
      dyOptions(axisLineWidth = 1.5, 
                fillGraph = TRUE, 
                drawGrid = TRUE) %>%
      dyAxis("y", label = "Prices") %>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.2) %>%
      dyRangeSelector()
    
    stock.chart
    
  })
  
  
  ########## 2.6 ESG scores evolution per company
  
  # The currently selected tab from the first box
  output$esg.chart <- renderDygraph({
    
    # Selecting specific company ticker
    trend.ticker <- db %>%
      filter(name == input$incompany) %>%
      select(value_date, ESG, E, S, G)
    
    # Updating date format 
    trend.ticker <- xts(x=trend.ticker, order.by=trend.ticker$value_date)
    trend.ticker$value_date <- NULL
    
    
    ## Chart
    dygraph(trend.ticker, main="Evolution of ESG Scores for the selected company by Sustainalytics") %>%
      dyAxis("y", label = "Scores") %>%
      dyOptions(axisLineWidth = 1.5, 
                drawGrid = TRUE) %>%
      dyAxis("y", label = "Scores") %>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.2) %>%
      dyRangeSelector()  %>%
      dyGroup(c("ESG",
                "E" ,
                "S", 
                "G"), 
              color = c(chartcolorpink, 
                        chartcolorgreen, 
                        chartcolorgold, 
                        chartcolorblue),
              strokeWidth=3)
    
  })
  
  
  ########2.3. Areas of concern
  output$comp.concern.chart <- renderPlot({
    
    # Matching scores values with their names for selected ticker
    comp.concern <- db %>%
      filter(name == input$incompany,
             value_date == '2018-08-01') %>%
      select(name, G.1.1, G.1.2, G.1.3, G.1.4, G.1.5, G.2.1, G.2.2, G.2.3, G.2.4, G.2.5, G.2.6, G.2.7, G.2.8, G.2.9, G.2.10, G.2.11, G.2.12, G.2.13, G.3.1, G.3.2, G.3.4, S.1.1, S.1.2, S.1.3, S.1.4, S.1.5, S.1.6, S.1.7, S.2.1, S.2.2, S.2.3, S.3.3, S.4.1, S.4.3, S.5.1, S.5.2, S.5.3, E.1.1, E.1.2, E.1.3, E.1.4, E.1.5, E.1.6, E.1.7, E.1.8, E.1.9, E.1.10, E.1.11, E.1.12, E.2.1, E.2.2, E.3.2) %>%
      gather(score_type,scores, G.1.1, G.1.2, G.1.3, G.1.4, G.1.5, G.2.1, G.2.2, G.2.3, G.2.4, G.2.5, G.2.6, G.2.7, G.2.8, G.2.9, G.2.10, G.2.11, G.2.12, G.2.13, G.3.1, G.3.2, G.3.4, S.1.1, S.1.2, S.1.3, S.1.4, S.1.5, S.1.6, S.1.7, S.2.1, S.2.2, S.2.3, S.3.3, S.4.1, S.4.3, S.5.1, S.5.2, S.5.3, E.1.1, E.1.2, E.1.3, E.1.4, E.1.5, E.1.6, E.1.7, E.1.8, E.1.9, E.1.10, E.1.11, E.1.12, E.2.1, E.2.2, E.3.2) %>%
      arrange_(~ scores) %>%
      filter(scores > 0) %>% # zero is not measured 
      top_n(n= -5, wt = scores) %>%
      inner_join(scores_names, by = c("score_type" = "indicator"))
    comp.concern
    
    
    ## Chart
    comp.concern.chart <- ggplot(data= comp.concern, 
                                 aes(y= scores, 
                                     x= reorder(indicator_name, scores),
                                     fill= type)) +
      geom_bar(stat= "identity") +
      scale_fill_manual(values= c(ESG = chartcolorpink,
                                  E = chartcolorgreen,
                                  S = chartcolorgold,
                                  G = chartcolorblue)) +
      labs(title="",
           subtitle="",
           caption="Source: Sustainalytics",
           x="Score types",
           y="Lastest scores") + 
      myattributes + 
      coord_flip() +
      theme(legend.direction = "horizontal", 
            legend.position = "bottom")
    

    print(comp.concern.chart)
  })
  
  
  
  
  ##################################################### PAGE 4 - CHARTS ##
  
  ######### 4.1. Top predictions
  output$mdd.p.topchart <- renderPlot({
    
    # Conditional for dataset 
    inputinregion <- input$inregion2
    
    if (inputinregion == 'All')
    {
      # All
      filterRegion <- !vector(mode="logical", length=dim(db)[1])
    } else {
      # Filter region
      filterRegion <- db$region == inputinregion
    }
    
    
    inputinsize <- input$insize2
    
    if (inputinsize == 'All')
    {
      # All
      filtersize <- !vector(mode="logical", length=dim(db)[1])
    } else {
      # Filter region
      filtersize <- db$size == inputinsize
    }
    
    
    inputinsector <- input$insector2
    
    if (inputinsector == 'All')
    {
      # All
      filtersector <- !vector(mode="logical", length=dim(db)[1])
    } else {
      # Filter region
      filtersector <- db$gics_sector == inputinsector
    }
    
    ## Data adjustment 
    table.comp <- db %>%
      filter(value_date == '2018-08-01'& 
               filterRegion &
               filtersize &
               filtersector) %>%
      select(name, ticker, region, country, gics_sector, size, ESG, E, S, G) %>%
      left_join(mdd.p, by= "name") %>%
      left_join(bs.p, by= "name") %>%
      na.omit() %>%
      select(name,
             ticker,
             region, 
             country, 
             sector = gics_sector, 
             size,
             ESG,
             E, S, G, 
             mdd_score = p_mdd18m, 
             bs_score = X0) %>%
      mutate(MDDmean = round(mean(mdd_score),1)) %>% 
      top_n(n= 10, wt = mdd_score) %>%
      arrange_(~ desc(mdd_score))
    

    ## Chart
    mdd.p.topchart <- ggplot(data= table.comp, aes(y= mdd_score, 
                                                   x= reorder(name,mdd_score))) +
      geom_bar(stat = "identity", fill=chartcolor) +
      coord_flip() +
      myattributes +
      labs(title="",
           subtitle="",
           caption="Source: Sustainalytics",
           x="Companies",
           y="MDD score") + 
      geom_errorbar(data= table.comp, 
                    aes(name, ymax = MDDmean, ymin = MDDmean),
                    size=1, 
                    inherit.aes = F, 
                    width = 1,
                    color=chartcolorpink)
    
    print(mdd.p.topchart)
  })
  
  
  
  ########## 4.2. Bottom predictions
  output$mdd.p.bottomchart <- renderPlot({

    # Conditional for dataset 
    inputinregion <- input$inregion2
    
    if (inputinregion == 'All')
    {
      # All
      filterRegion <- !vector(mode="logical", length=dim(db)[1])
    } else {
      # Filter region
      filterRegion <- db$region == inputinregion
    }
    
    
    inputinsize <- input$insize2
    
    if (inputinsize == 'All')
    {
      # All
      filtersize <- !vector(mode="logical", length=dim(db)[1])
    } else {
      # Filter region
      filtersize <- db$size == inputinsize
    }
    
    
    inputinsector <- input$insector2
    
    if (inputinsector == 'All')
    {
      # All
      filtersector <- !vector(mode="logical", length=dim(db)[1])
    } else {
      # Filter region
      filtersector <- db$gics_sector == inputinsector
    }
    
    ## Data adjustment 
    table.comp2 <- db %>%
      filter(value_date == '2018-08-01' & 
               filterRegion &
               filtersize &
               filtersector) %>%
      select(name, ticker, region, country, gics_sector, size, ESG, E, S, G) %>%
      left_join(mdd.p, by= "name") %>%
      left_join(bs.p, by= "name") %>%
      na.omit() %>%
      select(name,
             ticker,
             region, 
             country, 
             sector = gics_sector, 
             size,
             ESG,
             E, S, G, 
             mdd_score = p_mdd18m, 
             bs_score = X0) %>%
      mutate(MDDmean = round(mean(mdd_score),1)) %>% 
      top_n(n= -10, wt = mdd_score)
    
    ## Chart
    mdd.p.bottomchart <- ggplot(data= table.comp2, 
                                aes(y= mdd_score, 
                                    x= reorder(name,-mdd_score))) +
      geom_bar(stat = "identity", 
               fill=chartcolor) +
      coord_flip() +
      myattributes +
      labs(title="",
           subtitle="",
           caption="Source: Sustainalytics",
           x="Companies",
           y="MDD score") 
    
    print(mdd.p.bottomchart)
  })
  
  ###################### 4.3 TABLE
  

  # display 10 rows initially
  output$ex1 <- DT::renderDataTable({
    
    # Conditional for dataset 
    inputinregion <- input$inregion2
    
    if (inputinregion == 'All')
    {
      # All
      filterRegion <- !vector(mode="logical", length=dim(db)[1])
    } else {
      # Filter region
      filterRegion <- db$region == inputinregion
    }
    
    
    inputinsize <- input$insize2
    
    if (inputinsize == 'All')
    {
      # All
      filtersize <- !vector(mode="logical", length=dim(db)[1])
    } else {
      # Filter region
      filtersize <- db$size == inputinsize
    }
    
    
    inputinsector <- input$insector2
    
    if (inputinsector == 'All')
    {
      # All
      filtersector <- !vector(mode="logical", length=dim(db)[1])
    } else {
      # Filter region
      filtersector <- db$gics_sector == inputinsector
    }
    
    # Data to be displayed in the table 
    # Selecting specific region & TOp ten companies 
    table.comp <- db %>%
      filter(value_date == '2018-08-01'& 
               filterRegion &
               filtersize &
               filtersector) %>%
      select(name, ticker, region, country, gics_sector, size, ESG, E, S, G) %>%
      left_join(mdd.p, by= "name") %>%
      left_join(bs.p, by= "name") %>%
      na.omit() %>%
      select(name,
             ticker,
             region, 
             country, 
             sector = gics_sector, 
             size,
             ESG,
             E, S, G, 
             MDD_18m = p_mdd18m, 
             BlackSwan = X1) %>%
      mutate(MDD_18m = round(MDD_18m, 2)) %>%
      mutate(BlackSwan = round(BlackSwan * 1000, 2))
    
    DT::datatable(table.comp, options = list(pageLength = 10))
  })
  
  
  ###################### 2.KPI TABLE
  
  
  # display 10 rows initially
  output$ex2 <- DT::renderDataTable({
    
    # Data to be displayed in the table 
    ## Ticker input
    selected.ticker <- db %>%
                        filter(name == input$incompany,
                               value_date == '2018-08-01') %>%
                        select(ticker)
    selected.ticker <- as.character(selected.ticker[[1]])
    
    # Market data 
    main.kpis <- getQuote(selected.ticker, 
                          what = yahooQF(c("Previous Close", 
                                           "Market Capitalization", 
                                           "P/E Ratio"))) 
    
    #main.kpis$ticker <- row.names(main.kpis)
    main.kpis<- main.kpis %>% 
      rename(price_close = `P. Close`,
             market_cap = `Market Capitalization`,
             pe_ratio = `P/E Ratio`)  %>% 
      mutate( ticker = selected.ticker) %>%
      mutate_if(is.numeric, round, 2) %>%
      mutate_all(funs(prettyNum(., big.mark=","))) %>%
      select(ticker, everything())
    
    ## Value
    comp.kpis <- db %>%
      filter(ticker == selected.ticker,
             value_date == '2018-08-01') %>%
      full_join(main.kpis, by = "ticker") %>%
      left_join(mdd.p, by= "name") %>%
      left_join(bs.p, by= "name") %>%
      select(ticker, 
             name, 
             ESG, 
             E, 
             S, 
             G,
             MDD_18m = p_mdd18m,
             BlackSwan = X1,
             Closing_price = price_close, 
             Market_cap = market_cap, 
             PE_ratio = pe_ratio) %>%
      mutate(MDD_18m = round(MDD_18m, 2)) %>%
      mutate(BlackSwan = round(BlackSwan * 1000, 2))
    
    DT::datatable(comp.kpis, options = list(paging = FALSE, 
                                            searching = FALSE))
  })
  
  
  ###################### 3.KPIs OVERALL TABLE
  
  
  # display 10 rows initially
  output$table_3 <- DT::renderDataTable({
    
    # Table to display
    table.3 <- db %>%
      filter(value_date == '2018-08-01') %>%
      select(name, ticker, region, country, gics_sector, size, ESG, E, S, G, E.1.9, G.2.7.1, S.1.7) %>%
      left_join(mdd.p, by= "name") %>%
      left_join(bs.p, by= "name") %>%
      na.omit() %>%
      select(Name = name,
             Region = region, 
             Industry = gics_sector, 
             Size = size,
             ESG,
             E, S, G, 
             MDD_18m = p_mdd18m, 
             BlackSwan = X1,
             CI  = E.1.9, 
             BD  = G.2.7.1, 
             EC = S.1.7) %>%
      filter(MDD_18m > input$slider) %>%
      mutate(MDD_18m = round(MDD_18m, 2)) %>%
      mutate(BlackSwan = round(BlackSwan * 1000, 2))
    
    DT::datatable(table.3, options = list(pageLength = 10))
  })  
  
}




#################### RUN THE APP ############################

shinyApp(
  ui = dashboardPage(
    header,
    sidebar,
    body),
  server = server)
