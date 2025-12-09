# Phase 0: Comprehensive Dashboard Enhancement Plan

## Overview
This document outlines the complete Phase 0 enhancement plan, incorporating World Bank integration, UX improvements, statistical enhancements, and panel data analytics. The work is organized into logical sub-batches for incremental delivery.

---

## BATCH 0A: Foundation & Core Fixes (Priority: CRITICAL)
**Estimated Time: 8-10 hours**

### 0A.1 World Bank Income Integration
- [ ] Add wbstats to DESCRIPTION dependencies (15 min)
- [ ] Create WB integration directory structure (15 min)
- [ ] Implement country code mapping logic (1 hour)
- [ ] Fetch and cache income classifications from World Bank (1 hour)
- [ ] Enrich WBES data with income levels (1.5 hours)
- [ ] Make income filter reactive across all modules (2 hours)
- [ ] Test income filter functionality (1 hour)

### 0A.2 Custom Sectors Feature
- [ ] Create custom sectors UI in sidebar (similar to custom regions) (1 hour)
- [ ] Implement custom sectors reactive value and CRUD operations (1.5 hours)
- [ ] Update sector filter to include custom sectors (45 min)
- [ ] Make all modules reactive to custom sectors (1 hour)
- [ ] Test custom sectors across all tabs (45 min)

### 0A.3 Survey Year Filter Fix
- [ ] Audit current year filter implementation (30 min)
- [ ] Fix year filter reactivity across all modules (1.5 hours)
- [ ] Test year filter changes (30 min)

### 0A.4 Business Environment Radar Consistency
- [ ] Investigate radar chart differences across profiles (45 min)
- [ ] Standardize radar chart calculation logic (1.5 hours)
- [ ] Verify radar charts show same data when no filters applied (30 min)

**Deliverable**: Launch app for Batch 0A review

---

## BATCH 0B: Filter UX & Reactivity (Priority: HIGH)
**Estimated Time: 10-12 hours**

### 0B.1 Dynamic Sector Filter in Sidebar
- [ ] Implement tab navigation observer to detect current tab (1 hour)
- [ ] Show/hide sector filter based on tab context (1 hour)
- [ ] Remove sector filter from main panels where appropriate (1 hour)
- [ ] Set default preselection (3-5 sectors) (30 min)
- [ ] Test dynamic filter behavior across all tabs (1 hour)

### 0B.2 Remove Duplicate Filters from Main Panels
- [ ] Remove duplicate filters from Access to Finance tab (30 min)
- [ ] Remove duplicate filters from Corruption & Governance tab (30 min)
- [ ] Remove duplicate filters from Workforce & Gender Inclusion tab (30 min)
- [ ] Remove duplicate filters from Business Performance & Trade tab (30 min)
- [ ] Remove duplicate filters from Crime & Security tab (30 min)
- [ ] Test each tab after filter removal (1 hour)

### 0B.3 Make Sort By Filter Auto-Reactive
- [ ] Update Cross-Country Benchmarking sort filter (30 min)
- [ ] Update Cross-Sector Benchmarking sort filter (30 min)
- [ ] Update Cross-Regional Benchmarking sort filter (30 min)
- [ ] Update Cross-Size Benchmarking sort filter (30 min)
- [ ] Remove "Compare" button dependencies (30 min)
- [ ] Test auto-reactivity (30 min)

### 0B.4 Chart-Specific Filters
- [ ] Audit all non-global filters across modules (1 hour)
- [ ] Move single-chart filters into card bodies (2 hours)
- [ ] Update reactivity logic for relocated filters (1 hour)
- [ ] Test filter placement and reactivity (1 hour)

### 0B.5 Benchmark Tab Global Filter Reactivity
- [ ] Make Cross-Sector tab reactive to all filters except Sector (1 hour)
- [ ] Make Cross-Country tab reactive to all filters except Country (1 hour)
- [ ] Make Cross-Regional tab reactive to all filters except Region (1 hour)
- [ ] Make Cross-Size tab reactive to all filters except Size (1 hour)
- [ ] Test geographic coverage charts with global filters (1 hour)
- [ ] Test detailed comparison tables with global filters (1 hour)

**Deliverable**: Launch app for Batch 0B review

---

## BATCH 0C: Statistical Enhancements (Priority: HIGH)
**Estimated Time: 12-15 hours**

### 0C.1 Chart Captioning & Metadata System
- [ ] Create chart ID generation utility (deterministic hash) (1.5 hours)
- [ ] Create caption template function with data source, creator, ID (1 hour)
- [ ] Update all plotly charts to include standardized captions (4 hours)
- [ ] Update all reactable tables to include standardized captions (2 hours)
- [ ] Test caption display across all tabs (1 hour)

### 0C.2 Chart Download Functionality
- [ ] Create universal download button component (1 hour)
- [ ] Add download handlers to all plotly charts (PNG/SVG) (2 hours)
- [ ] Add download handlers to all reactable tables (CSV/Excel) (1.5 hours)
- [ ] Test download functionality across browsers (1 hour)

### 0C.3 Scatter Plot Regression Statistics
- [ ] Create regression line utility function (45 min)
- [ ] Add trend lines to all scatter plots (2 hours)
- [ ] Display R², p-value, equation on scatter plots (1.5 hours)
- [ ] Test regression statistics accuracy (45 min)

### 0C.4 Group Comparison Statistical Tests
- [ ] Create t-test utility function (45 min)
- [ ] Create ANOVA utility function with post-hoc tests (1.5 hours)
- [ ] Add statistical annotations to bar charts (2 hours)
- [ ] Add statistical annotations to grouped bar charts (2 hours)
- [ ] Add statistical annotations to pie charts where relevant (1 hour)
- [ ] Display comprehensive ANOVA results (all pairwise comparisons) (1.5 hours)
- [ ] Test statistical test accuracy (1 hour)

**Deliverable**: Launch app for Batch 0C review

---

## BATCH 0D: Interactive Maps (Priority: HIGH)
**Estimated Time: 15-18 hours**

### 0D.1 Map Infrastructure
- [ ] Set up leaflet integration and dependencies (1 hour)
- [ ] Create country coordinate mapping data (1 hour)
- [ ] Create reusable map component with filtering (2 hours)
- [ ] Add map theming and styling (1 hour)

### 0D.2 Profile Module Maps
- [ ] Add interactive map to Regional Profile (1.5 hours)
- [ ] Add interactive map to Sector Profile (1.5 hours)
- [ ] Add interactive map to Size Profile (1.5 hours)
- [ ] Add interactive map to Country Profile (1.5 hours)
- [ ] Make maps reactive to all global filters (1 hour)

### 0D.3 Benchmark Module Maps
- [ ] Add map to Cross-Country Benchmarking (1 hour)
- [ ] Add map to Cross-Sector Benchmarking (1 hour)
- [ ] Add map to Cross-Regional Benchmarking (1 hour)
- [ ] Add map to Cross-Size Benchmarking (1 hour)

### 0D.4 Domain Module Maps
- [ ] Add map to Infrastructure tab (45 min)
- [ ] Add map to Finance tab (45 min)
- [ ] Add map to Corruption & Governance tab (45 min)
- [ ] Add map to Workforce tab (45 min)
- [ ] Add map to Performance tab (45 min)
- [ ] Add map to Crime & Security tab (45 min)
- [ ] Add tab-specific info popups and tooltips (2 hours)
- [ ] Test maps across all tabs (2 hours)

**Deliverable**: Launch app for Batch 0D review

---

## BATCH 0E: Stress Testing & Scenario Analysis (Priority: MEDIUM)
**Estimated Time: 18-22 hours**

### 0E.1 Stress Testing Framework
- [ ] Design stress testing UI component (2 hours)
- [ ] Create scenario parameter sliders/inputs (2 hours)
- [ ] Implement scenario calculation engine (3 hours)
- [ ] Create scenario comparison visualizations (2 hours)

### 0E.2 Domain-Specific Stress Tests
- [ ] Infrastructure stress tests (power outage impact) (2 hours)
- [ ] Finance stress tests (credit availability scenarios) (2 hours)
- [ ] Corruption stress tests (bribery rate impact) (2 hours)
- [ ] Workforce stress tests (labor cost scenarios) (2 hours)
- [ ] Performance stress tests (productivity shocks) (2 hours)

### 0E.3 What-If Analysis Tools
- [ ] Create counterfactual toggle interface (2 hours)
- [ ] Implement multi-variable scenario builder (3 hours)
- [ ] Add uncertainty ranges to projections (2 hours)
- [ ] Create scenario export/save functionality (1.5 hours)
- [ ] Test stress testing across modules (2 hours)

**Deliverable**: Launch app for Batch 0E review

---

## BATCH 0F: Panel Data Analytics - Longitudinal Views (Priority: HIGH)
**Estimated Time: 20-25 hours**

### 0F.1 Data Infrastructure
- [ ] Create panel data structure with 3-year rolling windows (3 hours)
- [ ] Implement survey wave harmonization logic (3 hours)
- [ ] Create versioned data lineage tracking system (2 hours)
- [ ] Add immutable release ID system (1.5 hours)

### 0F.2 Trend Lenses per Entity
- [ ] Create trend visualization component (2 hours)
- [ ] Implement country trend views with confidence bands (2 hours)
- [ ] Implement sector trend views with confidence bands (2 hours)
- [ ] Implement firm-size trend views with confidence bands (2 hours)
- [ ] Add structural break detection algorithm (3 hours)
- [ ] Add break point flagging in visualizations (1.5 hours)

### 0F.3 Within-Entity Change Trackers
- [ ] Create year-over-year delta calculations (2 hours)
- [ ] Implement rolling change score metrics (2 hours)
- [ ] Create acceleration/deceleration indicators (2 hours)
- [ ] Build change tracker dashboard tab (2 hours)
- [ ] Add change heatmaps (1.5 hours)

### 0F.4 Synthetic Cohort Reconstruction
- [ ] Implement 3-year window alignment algorithm (3 hours)
- [ ] Create coverage metadata displays (1.5 hours)
- [ ] Add comparability scoring system (2 hours)
- [ ] Display comparability badges on charts (1 hour)
- [ ] Test synthetic cohort accuracy (2 hours)

**Deliverable**: Launch app for Batch 0F review

---

## BATCH 0G: Panel Data Analytics - Comparative Analytics (Priority: HIGH)
**Estimated Time: 18-22 hours**

### 0G.1 Cohort Benchmarking
- [ ] Create peer group selection UI (2 hours)
- [ ] Implement fixed-effects style adjustments (3 hours)
- [ ] Separate structural vs. time-varying components (2 hours)
- [ ] Create cohort comparison visualizations (2 hours)
- [ ] Add income-level cohort comparisons (1.5 hours)
- [ ] Add region cohort comparisons (1.5 hours)
- [ ] Add fragility cohort comparisons (1.5 hours)

### 0G.2 Event/Treatment Analysis (DiD-Style)
- [ ] Create policy shock selection interface (2 hours)
- [ ] Implement difference-in-differences estimation (3 hours)
- [ ] Add pre-trend visualization checks (2 hours)
- [ ] Display treatment effect estimates with CI (1.5 hours)
- [ ] Add power and coverage warnings (1.5 hours)
- [ ] Create event study plots (2 hours)

### 0G.3 Rotating Control Sets
- [ ] Implement robust comparison group selection algorithm (3 hours)
- [ ] Add outlier detection and exclusion logic (2 hours)
- [ ] Create control set sensitivity analysis (2 hours)
- [ ] Display control group composition (1 hour)

**Deliverable**: Launch app for Batch 0G review

---

## BATCH 0H: Panel Data Analytics - Forward Signals & Quality (Priority: MEDIUM)
**Estimated Time: 22-28 hours**

### 0H.1 Early-Warning Scores
- [ ] Build predictive modeling pipeline (4 hours)
- [ ] Implement credit default risk model (3 hours)
- [ ] Implement export drop risk model (3 hours)
- [ ] Add feature contribution displays (SHAP-style) (3 hours)
- [ ] Create risk score dashboards (2 hours)
- [ ] Add model performance metrics (1.5 hours)

### 0H.2 Scenario Playbooks
- [ ] Create counterfactual parameter interface (2 hours)
- [ ] Implement scenario trajectory calculations (3 hours)
- [ ] Add uncertainty range visualizations (2 hours)
- [ ] Create scenario comparison tables (1.5 hours)
- [ ] Add scenario export functionality (1 hour)

### 0H.3 Survey Quality Diagnostics
- [ ] Implement time-consistency checks (2 hours)
- [ ] Create wave-level nonresponse heatmaps (2 hours)
- [ ] Add interviewer/cluster effect detection (2 hours)
- [ ] Display harmonization status per indicator (1.5 hours)
- [ ] Create data quality dashboard tab (2 hours)

### 0H.4 Comparability System
- [ ] Implement comparability scoring algorithm (2 hours)
- [ ] Create comparability badges for indicators (1.5 hours)
- [ ] Add method change warnings (1 hour)
- [ ] Add sample change warnings (1 hour)
- [ ] Display 3-year alignment assumptions (1 hour)

**Deliverable**: Launch app for Batch 0H review

---

## BATCH 0I: UX/Delivery & API (Priority: MEDIUM)
**Estimated Time: 15-20 hours**

### 0I.1 Narrative Exports
- [ ] Create auto-generated brief template system (3 hours)
- [ ] Implement longitudinal finding summarization (3 hours)
- [ ] Add significant change detection and reporting (2 hours)
- [ ] Create PDF export functionality (2 hours)
- [ ] Add Word export functionality (2 hours)

### 0I.2 API Surfaces
- [ ] Design API endpoint structure (2 hours)
- [ ] Implement panel-ready data extract endpoints (3 hours)
- [ ] Create parameterized query system (2 hours)
- [ ] Add fixed peer set endpoints (1.5 hours)
- [ ] Create reproducible notebook templates (2 hours)
- [ ] Add API documentation (2 hours)

### 0I.3 Governance Dashboards
- [ ] Create data freshness monitoring views (2 hours)
- [ ] Add pipeline health monitoring (2 hours)
- [ ] Implement longitudinal coverage tracking (1.5 hours)
- [ ] Create country/indicator coverage matrix (1.5 hours)
- [ ] Add ops alert system (1 hour)

**Deliverable**: Launch app for Batch 0I review

---

## BATCH 0J: Testing, Documentation & Deployment (Priority: CRITICAL)
**Estimated Time: 12-15 hours**

### 0J.1 Testing
- [ ] Write unit tests for WB integration functions (2 hours)
- [ ] Write tests for custom sectors functionality (1.5 hours)
- [ ] Write tests for panel data calculations (2 hours)
- [ ] Write tests for statistical functions (t-test, ANOVA, regression) (2 hours)
- [ ] Write integration tests for stress testing (1.5 hours)
- [ ] Perform end-to-end user acceptance testing (3 hours)

### 0J.2 Documentation
- [ ] Create user guide for custom regions/sectors (1.5 hours)
- [ ] Document stress testing features (1.5 hours)
- [ ] Document panel data analytics features (2 hours)
- [ ] Create API documentation (1.5 hours)
- [ ] Update README with new features (1 hour)

### 0J.3 Performance & Security
- [ ] Implement logging and metrics (2 hours)
- [ ] Add caching for expensive computations (2 hours)
- [ ] Security audit for microdata handling (2 hours)
- [ ] Performance optimization (2 hours)

### 0J.4 Deployment
- [ ] Create migration notes (1 hour)
- [ ] Create roll-out plan (1 hour)
- [ ] Final deployment to production (1 hour)

**Deliverable**: Production-ready Phase 0 deployment

---

## Summary Timeline

| Batch | Focus Area | Estimated Hours | Priority |
|-------|------------|----------------|----------|
| 0A | Foundation & Core Fixes | 8-10 | CRITICAL |
| 0B | Filter UX & Reactivity | 10-12 | HIGH |
| 0C | Statistical Enhancements | 12-15 | HIGH |
| 0D | Interactive Maps | 15-18 | HIGH |
| 0E | Stress Testing | 18-22 | MEDIUM |
| 0F | Panel Data - Longitudinal | 20-25 | HIGH |
| 0G | Panel Data - Comparative | 18-22 | HIGH |
| 0H | Panel Data - Forward Signals | 22-28 | MEDIUM |
| 0I | UX/Delivery & API | 15-20 | MEDIUM |
| 0J | Testing & Deployment | 12-15 | CRITICAL |
| **TOTAL** | | **150-187 hours** | |

## Recommended Work Schedule

**Week 1-2**: Batches 0A + 0B (Foundation, Core Fixes, Filter UX)
**Week 3-4**: Batches 0C + 0D (Statistics, Maps)
**Week 5-6**: Batch 0E (Stress Testing)
**Week 7-9**: Batches 0F + 0G (Panel Data Longitudinal & Comparative)
**Week 10-11**: Batch 0H (Panel Data Forward Signals & Quality)
**Week 12**: Batch 0I (UX/Delivery & API)
**Week 13-14**: Batch 0J (Testing, Documentation, Deployment)

**Total Duration**: Approximately 3-3.5 months at full-time commitment

## Implementation Approach

1. **Incremental Delivery**: Complete each batch fully before moving to next
2. **User Review Checkpoints**: Launch app after each batch for joint audit
3. **Backward Compatibility**: Preserve existing functionality throughout
4. **Feature Flags**: Use feature flags for experimental features (especially stress testing)
5. **Version Control**: Tag each batch completion in git
6. **Documentation**: Update docs continuously, not at the end

## Success Metrics

- All filters working reactively across all tabs
- Custom regions AND custom sectors fully functional
- All charts have: captions, downloads, statistical tests (where appropriate)
- Interactive maps on every relevant tab
- Stress testing functional for at least 5 key domains
- Panel data analytics operational with trend views and cohort benchmarking
- API endpoints serving panel-ready data extracts
- Zero regression in existing functionality
- User adoption of new custom sector/region features >70%
- Dashboard load time <3 seconds with cached data

## Post-Phase 0 Vision

After Phase 0 completion, the dashboard will be:
- **Comprehensive**: WBES + WB Databank integration
- **Customizable**: Custom regions, sectors, scenarios
- **Statistical**: Rigorous tests, regressions, significance
- **Longitudinal**: Panel data analytics with trend analysis
- **Predictive**: Early-warning scores and scenario playbooks
- **Production-Ready**: API, exports, governance dashboards
- **Decision-Grade**: From exploratory tool to decision support system

This transforms the dashboard into THE definitive platform for business environment analysis and decision support.
