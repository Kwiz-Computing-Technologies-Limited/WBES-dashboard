# app/view/about.R
# About Page Module

box::use(
  shiny[moduleServer, NS, tags, div, icon, h2, h3, h4, h5, p, a, hr,
        fluidRow, column, HTML],
  bslib[card, card_header, card_body]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "container-fluid py-4",
    
    fluidRow(
      column(12,
        h2(icon("info-circle"), " About This Dashboard", class = "text-primary mb-4")
      )
    ),
    
    fluidRow(
      class = "mb-4",
      column(8,
        card(
          card_header(icon("database"), " About the Data"),
          card_body(
            h4("World Bank Enterprise Surveys", class = "text-primary"),
            p("The Enterprise Surveys are the World Bank's flagship firm-level data collection, 
               providing standardized, comparable data on business environments across economies."),
            
            tags$ul(
              tags$li(tags$strong("Coverage:"), " 168 economies worldwide"),
              tags$li(tags$strong("Sample:"), " 253,000+ businesses surveyed since 2002"),
              tags$li(tags$strong("Indicators:"), " 150+ standardized indicators"),
              tags$li(tags$strong("Methodology:"), " Stratified random sampling"),
              tags$li(tags$strong("Frequency:"), " Rolling surveys, typically every 3-5 years")
            ),
            
            hr(),
            
            h5("Key Topic Areas"),
            fluidRow(
              column(6,
                tags$ul(
                  tags$li(icon("bolt"), " Infrastructure quality"),
                  tags$li(icon("university"), " Access to finance"),
                  tags$li(icon("balance-scale"), " Corruption & governance"),
                  tags$li(icon("industry"), " Firm performance")
                )
              ),
              column(6,
                tags$ul(
                  tags$li(icon("users"), " Labor & workforce"),
                  tags$li(icon("globe"), " Trade & exports"),
                  tags$li(icon("venus"), " Gender & ownership"),
                  tags$li(icon("lightbulb"), " Innovation")
                )
              )
            ),
            
            hr(),
            
            p(
              tags$strong("Official Data Source: "),
              a(href = "https://www.enterprisesurveys.org/en/survey-datasets",
                target = "_blank", "enterprisesurveys.org")
            )
          )
        )
      ),
      column(4,
        card(
          card_header(icon("code"), " Built With"),
          card_body(
            tags$ul(
              class = "list-unstyled",
              tags$li(icon("check", class = "text-success"), " R + Shiny"),
              tags$li(icon("check", class = "text-success"), " Rhino Framework (Appsilon)"),
              tags$li(icon("check", class = "text-success"), " bslib + Bootstrap 5"),
              tags$li(icon("check", class = "text-success"), " Plotly Interactive Charts"),
              tags$li(icon("check", class = "text-success"), " Leaflet Maps"),
              tags$li(icon("check", class = "text-success"), " World Bank API"),
              tags$li(icon("check", class = "text-success"), " testthat + Cypress Testing")
            )
          )
        ),
        tags$br(),
        card(
          card_header(class = "bg-secondary text-white", icon("building"), " Developed By"),
          card_body(
            class = "text-center",
            h4("Kwiz Computing Technologies", class = "text-primary"),
            p(class = "text-muted", "Boutique Data Science Consultancy"),
            p("Nairobi, Kenya"),
            hr(),
            div(class = "d-grid gap-2",
              a(href = "https://kwizresearch.com", target = "_blank",
                class = "btn btn-primary", icon("globe"), " kwizresearch.com"),
              a(href = "https://linkedin.com/in/jean-victor-kwizera", target = "_blank",
                class = "btn btn-outline-secondary", icon("linkedin"), " LinkedIn")
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(12,
        card(
          card_header(icon("file-contract"), " License & Citation"),
          card_body(
            fluidRow(
              column(6,
                h5("Data License"),
                p("World Bank Enterprise Survey data is publicly available under the ",
                  a(href = "https://www.worldbank.org/en/about/legal/terms-of-use-for-datasets",
                    target = "_blank", "World Bank Terms of Use"), "."),
                tags$pre(class = "bg-light p-2 rounded", style = "font-size: 0.85rem;",
'World Bank Group. Enterprise Surveys.
https://www.enterprisesurveys.org'
                )
              ),
              column(6,
                h5("Dashboard"),
                p("Developed by Kwiz Computing Technologies."),
                p("For commercial licensing or consultancy:"),
                tags$ul(
                  tags$li(icon("envelope"), " ", a(href = "mailto:jeankwizera@kwizresearch.com", "jeankwizera@kwizresearch.com")),
                  tags$li(icon("globe"), " ", a(href = "https://kwizresearch.com", target = "_blank", "kwizresearch.com"))
                )
              )
            )
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Static page - no server logic
  })
}
