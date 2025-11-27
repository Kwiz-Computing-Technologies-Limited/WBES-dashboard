# app/logic/__init__.R
# Logic module exports

box::use(
  ./wbes_data[
    load_wbes_data,
    load_sample_data,
    fetch_es_data,
    get_es_indicators,
    generate_quality_metadata,
    WBES_INDICATORS
  ]
)
