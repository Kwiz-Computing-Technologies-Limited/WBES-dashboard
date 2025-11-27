# tests/testthat/test-main.R
# Unit tests for WBES Dashboard

box::use(
  testthat[describe, it, expect_true, expect_false, expect_equal, 
           expect_type, expect_named, expect_s3_class, expect_gte, expect_lte]
)

# Set box path for testing
options(box.path = file.path(getwd(), "app"))

box::use(
  app/logic/wbes_data[load_sample_data, generate_quality_metadata, WBES_INDICATORS]
)

describe("load_sample_data()", {
  
  sample_data <- load_sample_data()
  
  it("returns a list with expected components", {
    expect_type(sample_data, "list")
    expect_true("raw" %in% names(sample_data))
    expect_true("latest" %in% names(sample_data))
    expect_true("countries" %in% names(sample_data))
    expect_true("metadata" %in% names(sample_data))
    expect_true("quality" %in% names(sample_data))
  })
  
  it("contains panel data with required columns", {
    panel <- sample_data$raw
    expect_s3_class(panel, "data.frame")
    expect_true("country" %in% names(panel))
    expect_true("country_code" %in% names(panel))
    expect_true("region" %in% names(panel))
    expect_true("year" %in% names(panel))
  })
  
  it("has valid country coverage", {
    expect_gte(length(sample_data$countries), 20)
    expect_true("Kenya" %in% sample_data$countries)
  })
  
  it("has valid WBES indicator columns", {
    panel <- sample_data$raw
    expect_true("IC.FRM.OUTG.ZS" %in% names(panel))
    expect_true("IC.FRM.FINA.ZS" %in% names(panel))
    expect_true("IC.FRM.CORR.ZS" %in% names(panel))
  })
  
  it("has valid indicator ranges (0-100%)", {
    panel <- sample_data$raw
    expect_true(all(panel$IC.FRM.OUTG.ZS >= 0 & panel$IC.FRM.OUTG.ZS <= 100, na.rm = TRUE))
    expect_true(all(panel$IC.FRM.FINA.ZS >= 0 & panel$IC.FRM.FINA.ZS <= 100, na.rm = TRUE))
  })
  
  it("includes metadata with source", {
    meta <- sample_data$metadata
    expect_type(meta, "list")
    expect_true("source" %in% names(meta))
    expect_true(grepl("World Bank", meta$source))
  })
  
})

describe("generate_quality_metadata()", {
  
  quality <- generate_quality_metadata()
  
  it("returns list with issues and filters", {
    expect_type(quality, "list")
    expect_true("issues" %in% names(quality))
    expect_true("filters" %in% names(quality))
  })
  
  it("documents at least 5 data quality issues", {
    expect_gte(length(quality$issues), 5)
  })
  
  it("each issue has required fields", {
    required <- c("id", "category", "severity", "indicator", "description", "filter_applied", "r_code")
    
    for (issue in quality$issues) {
      for (field in required) {
        expect_true(field %in% names(issue), 
                    info = paste("Missing", field, "in", issue$id))
      }
    }
  })
  
  it("severity levels are valid", {
    valid <- c("Low", "Medium", "High")
    for (issue in quality$issues) {
      expect_true(issue$severity %in% valid)
    }
  })
  
})

describe("WBES_INDICATORS", {
  
  it("is a list of indicator codes", {
    expect_type(WBES_INDICATORS, "list")
    expect_gte(length(WBES_INDICATORS), 10)
  })
  
  it("contains key infrastructure indicators", {
    expect_true("infrastructure_obstacle" %in% names(WBES_INDICATORS))
    expect_true("electricity_obstacle" %in% names(WBES_INDICATORS))
    expect_true("power_outages" %in% names(WBES_INDICATORS))
  })
  
  it("contains key finance indicators", {
    expect_true("finance_obstacle" %in% names(WBES_INDICATORS))
    expect_true("bank_account" %in% names(WBES_INDICATORS))
  })
  
  it("indicator codes follow World Bank format", {
    codes <- unlist(WBES_INDICATORS)
    # World Bank indicator codes typically contain "IC.FRM"
    expect_true(all(grepl("IC\\.FRM", codes)))
  })
  
})
