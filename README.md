# World Bank Enterprise Surveys Dashboard

## Business Environment Benchmarking

![R](https://img.shields.io/badge/R-≥4.1.0-blue)
![Rhino](https://img.shields.io/badge/Rhino-1.9.0-green)
![License](https://img.shields.io/badge/License-Custom-orange)

Enterprise-grade Shiny dashboard for analyzing business environments across 168 economies using World Bank Enterprise Survey data.

**Developed by [Kwiz Computing Technologies](https://kwizresearch.com)** | Nairobi, Kenya

---

## Features

| Module | Description |
|--------|-------------|
| **Overview** | Global KPIs, interactive map, regional comparisons |
| **Country Profile** | Deep-dive with radar charts and time series |
| **Benchmark** | Compare up to 10 countries across indicators |
| **Infrastructure** | Power outages, electricity access, productivity impact |
| **Finance** | Credit gaps, collateral, gender disparities |
| **Data Quality** | Full transparency on data issues and filter logic |

### Technical Highlights

- ✅ **Rhino Framework** - Enterprise-grade modular architecture
- ✅ **World Bank API** - Real-time data from Enterprise Surveys API
- ✅ **Comprehensive Testing** - testthat + Cypress e2e tests
- ✅ **Data Quality Documentation** - Full transparency on filtering logic
- ✅ **Responsive Design** - Desktop, tablet, and mobile support

---

## Project Structure

```
.
├── app
│   ├── js
│   │   └── index.js
│   ├── logic
│   │   ├── __init__.R
│   │   └── wbes_data.R        # World Bank API integration
│   ├── static
│   │   └── favicon.ico
│   ├── styles
│   │   └── main.scss          # Kwiz Research theme
│   ├── view
│   │   ├── __init__.R
│   │   ├── overview.R
│   │   ├── country_profile.R
│   │   ├── benchmark.R
│   │   ├── infrastructure.R
│   │   ├── finance_access.R
│   │   ├── data_quality.R
│   │   └── about.R
│   └── main.R
├── tests
│   ├── cypress
│   │   └── e2e
│   │       └── app.cy.js
│   ├── testthat
│   │   └── test-main.R
│   └── cypress.json
├── data                        # Place microdata here (optional)
├── app.R
├── WBESDashboard.Rproj
├── dependencies.R
├── renv.lock
└── rhino.yml
```

---

## Quick Start

### Prerequisites

```r
install.packages(c("rhino", "shiny", "bslib", "plotly", "leaflet", "DT",
                   "dplyr", "tidyr", "httr", "jsonlite", "haven", 
                   "waiter", "logger", "box"))
```

### Run Locally

```r
# Option 1: Using Rhino
rhino::app()

# Option 2: Direct Shiny
shiny::runApp()
```

### With renv (Recommended)

```r
renv::restore()
rhino::app()
```

---

## Data Sources

### 1. World Bank API (Default)

The dashboard fetches aggregate indicators from the World Bank API (Source ID 13: Enterprise Surveys). No registration required.

```r
# Fetched indicators include:
# IC.FRM.OUTG.ZS - Power outages obstacle
# IC.FRM.FINA.ZS - Access to finance obstacle
# IC.FRM.CORR.ZS - Corruption obstacle
# ... and 12+ more
```

### 2. Microdata (Optional)

For firm-level analysis, download microdata from [enterprisesurveys.org](https://www.enterprisesurveys.org/en/survey-datasets):

1. Register (free) at the Enterprise Surveys portal
2. Download `.dta` files for your countries of interest
3. Place files in the `data/` directory
4. Restart the application

---

## Testing

```r
# Unit tests
testthat::test_dir("tests/testthat")

# With Rhino
rhino::test_r()

# E2E tests (requires Cypress)
rhino::test_e2e()

# Lint code
rhino::lint_r()
```

---

## Deployment

### shinyapps.io

```r
rsconnect::deployApp(
  appFiles = c("app.R", "app/", "dependencies.R", "rhino.yml", "renv.lock"),
  appName = "wbes-dashboard"
)
```

### Docker

```dockerfile
FROM rocker/shiny-verse:4.3.0

RUN R -e "install.packages(c('rhino', 'bslib', 'plotly', 'leaflet', 'DT', 'httr', 'jsonlite', 'haven', 'waiter', 'logger', 'box'))"

COPY . /srv/shiny-server/wbes-dashboard

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
```

---

## Theme Customization

Edit `app/styles/main.scss`:

```scss
$primary-teal: #1B6B5F;      // Main brand color
$secondary-coral: #F49B7A;    // Accent color
```

---

## License & Citation

### Data

```
World Bank Group. Enterprise Surveys.
https://www.enterprisesurveys.org
```

### Dashboard

```
Kwiz Computing Technologies (2025). 
Business Environment Benchmarking Dashboard.
https://kwizresearch.com
```

---

## Contact

**Kwiz Computing Technologies**

- 🌐 [kwizresearch.com](https://kwizresearch.com)
- 📧 info@kwizresearch.com
- 💼 [LinkedIn](https://linkedin.com/in/jean-victor-kwizera)

---

*Built with ❤️ in Nairobi, Kenya*
