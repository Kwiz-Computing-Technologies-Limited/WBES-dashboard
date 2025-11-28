# app/view/mod_about.R
# About Page Module

box::use(
  shiny[moduleServer, NS, tags, icon, div, h2, h3, h4, h5, p, span, strong, a, br, hr,
        ul, li, fluidRow, column, HTML],
  bslib[card, card_header, card_body]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  tags$div(
    class = "about-container",
    
    fluidRow(
      column(12,
        tags$div(
          class = "page-header mb-4",
          h2(icon("info-circle"), "About This Dashboard", class = "text-primary-teal"),
          p(class = "lead text-muted",
            "Business Environment Benchmarking using World Bank Enterprise Survey Data")
        )
      )
    ),
    
    fluidRow(
      class = "mb-4",
      column(8,
        card(
          card_header(icon("database"), "About the Data"),
          card_body(
            h4("World Bank Enterprise Surveys", class = "text-primary-teal"),
            p("The Enterprise Surveys are the World Bank's flagship firm-level data collection, 
               providing comparable, standardized data on the business environment across economies."),
            
            tags$ul(
              tags$li(tags$strong("Coverage:"), " 168 economies worldwide"),
              tags$li(tags$strong("Sample:"), " 253,000+ businesses surveyed since 2002"),
              tags$li(tags$strong("Indicators:"), " 150+ standardized indicators"),
              tags$li(tags$strong("Methodology:"), " Stratified random sampling with sampling weights"),
              tags$li(tags$strong("Frequency:"), " Rolling surveys, typically every 3-5 years per country")
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
                  tags$li(icon("lightbulb"), " Innovation & technology")
                )
              )
            ),
            
            hr(),
            
            p(
              tags$strong("Official Data Source: "),
              a(href = "https://www.enterprisesurveys.org/en/survey-datasets",
                target = "_blank",
                "enterprisesurveys.org",
                class = "text-primary-teal")
            )
          )
        )
      ),
      column(4,
        card(
          card_header(icon("code"), "Built With"),
          card_body(
            tags$div(
              class = "tech-stack",
              tags$div(
                class = "mb-3",
                tags$img(src = "https://www.r-project.org/logo/Rlogo.svg", 
                         height = "40", class = "me-2"),
                tags$strong("R Programming Language")
              ),
              tags$ul(
                class = "list-unstyled",
                tags$li(icon("check", class = "text-success"), " Rhino Framework (Appsilon)"),
                tags$li(icon("check", class = "text-success"), " Shiny + bslib"),
                tags$li(icon("check", class = "text-success"), " Plotly Interactive Charts"),
                tags$li(icon("check", class = "text-success"), " Leaflet Maps"),
                tags$li(icon("check", class = "text-success"), " DT DataTables"),
                tags$li(icon("check", class = "text-success"), " box Modular Design"),
                tags$li(icon("check", class = "text-success"), " testthat Testing")
              )
            )
          )
        ),
        br(),
        card(
          card_header(
            class = "card-header-secondary",
            icon("building"), "Developed By"
          ),
          card_body(
            tags$div(
              class = "text-center",
              h4("Kwiz Computing Technologies", class = "text-primary-teal"),
              p(class = "text-muted", "Boutique Data Science Consultancy"),
              p("Nairobi, Kenya"),
              hr(),
              tags$div(
                class = "d-grid gap-2",
                a(href = "https://kwizresearch.com", target = "_blank",
                  class = "btn btn-kwiz-primary",
                  icon("globe"), " kwizresearch.com"),
                a(href = "https://linkedin.com/in/jean-victor-kwizera", target = "_blank",
                  class = "btn btn-kwiz-outline",
                  icon("linkedin"), " LinkedIn")
              )
            )
          )
        )
      )
    ),
    
    fluidRow(
      class = "mb-4",
      column(12,
        card(
          card_header(icon("file-contract"), "License & Usage"),
          card_body(
            fluidRow(
              column(6,
                h5("Data License"),
                p("World Bank Enterprise Survey data is publicly available under the ",
                  a(href = "https://www.worldbank.org/en/about/legal/terms-of-use-for-datasets",
                    target = "_blank", "World Bank Terms of Use"), "."),
                p("Users should cite the source as:"),
                tags$pre(
                  class = "bg-light p-2 rounded",
                  style = "font-size: 0.85rem;",
'World Bank Group. Enterprise Surveys.
Available at: https://www.enterprisesurveys.org'
                )
              ),
              column(6,
                h5("Dashboard License"),
                p("This dashboard is developed by Kwiz Computing Technologies."),
                p("For commercial licensing, customization, or consultancy inquiries:"),
                tags$ul(
                  tags$li(
                    icon("envelope"), " ",
                    a(href = "mailto:info@kwizresearch.com", "info@kwizresearch.com")
                  ),
                  tags$li(
                    icon("globe"), " ",
                    a(href = "https://kwizresearch.com", target = "_blank", "kwizresearch.com")
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(12,
        card(
          card_header(icon("book-open"), "Recommended Reading"),
          card_body(
            fluidRow(
              column(4,
                h5(icon("chart-line"), " Methodology"),
                tags$ul(
                  tags$li(a(href = "https://www.enterprisesurveys.org/en/methodology",
                            target = "_blank", "Enterprise Surveys Methodology")),
                  tags$li(a(href = "https://www.enterprisesurveys.org/en/methodology/implementation-reports",
                            target = "_blank", "Country Implementation Reports"))
                )
              ),
              column(4,
                h5(icon("graduation-cap"), " Research"),
                tags$ul(
                  tags$li(a(href = "https://www.enterprisesurveys.org/en/publications",
                            target = "_blank", "Enterprise Surveys Publications")),
                  tags$li(a(href = "https://openknowledge.worldbank.org",
                            target = "_blank", "World Bank Open Knowledge Repository"))
                )
              ),
              column(4,
                h5(icon("blog"), " Kwiz Research Blog"),
                tags$ul(
                  tags$li(a(href = "https://kwizresearch.com/blog",
                            target = "_blank", "Data Quality Assessment Best Practices")),
                  tags$li(a(href = "https://kwizresearch.com/blog",
                            target = "_blank", "R for Environmental Data Science"))
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
    # Static page - no server logic required
  })
}
