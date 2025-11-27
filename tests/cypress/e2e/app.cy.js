// tests/cypress/e2e/app.cy.js
// End-to-end tests for WBES Dashboard

describe('WBES Dashboard', () => {
  
  beforeEach(() => {
    cy.visit('/');
    // Wait for Shiny to initialize
    cy.get('.navbar', { timeout: 30000 }).should('be.visible');
  });

  describe('Navigation', () => {
    
    it('should display the main navigation bar', () => {
      cy.get('.navbar').should('be.visible');
      cy.get('.navbar-brand').should('contain', 'WBES Dashboard');
    });

    it('should have all navigation tabs', () => {
      cy.get('.nav-link').should('have.length.at.least', 6);
      cy.contains('.nav-link', 'Overview').should('exist');
      cy.contains('.nav-link', 'Country Profile').should('exist');
      cy.contains('.nav-link', 'Benchmark').should('exist');
      cy.contains('.nav-link', 'Infrastructure').should('exist');
      cy.contains('.nav-link', 'Finance').should('exist');
      cy.contains('.nav-link', 'Data Quality').should('exist');
    });

    it('should navigate between tabs', () => {
      cy.contains('.nav-link', 'Country Profile').click();
      cy.url().should('include', 'country');
      
      cy.contains('.nav-link', 'Infrastructure').click();
      cy.contains('Infrastructure Constraints').should('be.visible');
    });

  });

  describe('Overview Page', () => {
    
    it('should display KPI cards', () => {
      cy.get('.card').should('have.length.at.least', 4);
      cy.contains('Countries').should('be.visible');
      cy.contains('Firms Surveyed').should('be.visible');
    });

    it('should display the world map', () => {
      cy.get('.leaflet-container').should('be.visible');
    });

    it('should display the obstacles chart', () => {
      cy.get('.plotly').should('exist');
    });

    it('should have working filters', () => {
      cy.get('select').first().should('be.visible');
      cy.get('select').first().select(1);
    });

  });

  describe('Country Profile Page', () => {
    
    beforeEach(() => {
      cy.contains('.nav-link', 'Country Profile').click();
    });

    it('should display country selector', () => {
      cy.get('select').should('be.visible');
    });

    it('should display radar chart after country selection', () => {
      cy.get('select').first().select('Kenya');
      cy.get('.plotly').should('exist');
    });

    it('should display key metrics', () => {
      cy.get('.list-group').should('be.visible');
    });

  });

  describe('Benchmark Page', () => {
    
    beforeEach(() => {
      cy.contains('.nav-link', 'Benchmark').click();
    });

    it('should allow multi-country selection', () => {
      cy.get('.selectize-input').should('be.visible');
    });

    it('should display comparison chart', () => {
      cy.get('.plotly').should('exist');
    });

    it('should display data table', () => {
      cy.get('.dataTables_wrapper').should('exist');
    });

  });

  describe('Data Quality Page', () => {
    
    beforeEach(() => {
      cy.contains('.nav-link', 'Data Quality').click();
    });

    it('should display documentation sections', () => {
      cy.contains('Data Quality & Methodology').should('be.visible');
      cy.contains('Source Transparency').should('be.visible');
    });

    it('should display documented issues', () => {
      cy.get('.alert').should('have.length.at.least', 1);
    });

    it('should have filter logic tabs', () => {
      cy.contains('Infrastructure').should('be.visible');
      cy.contains('Finance').should('be.visible');
      cy.contains('Corruption').should('be.visible');
    });

    it('should display R code snippets', () => {
      cy.get('pre code').should('exist');
    });

    it('should have download buttons', () => {
      cy.contains('button', 'Issues Log').should('be.visible');
      cy.contains('button', 'Filter Logic').should('be.visible');
    });

  });

  describe('Responsive Design', () => {
    
    it('should work on tablet viewport', () => {
      cy.viewport(768, 1024);
      cy.get('.navbar').should('be.visible');
      cy.get('.card').should('be.visible');
    });

    it('should work on mobile viewport', () => {
      cy.viewport(375, 667);
      cy.get('.navbar').should('be.visible');
    });

  });

  describe('Accessibility', () => {
    
    it('should have proper heading hierarchy', () => {
      cy.get('h2').should('exist');
    });

    it('should have alt text on images', () => {
      cy.get('img').each(($img) => {
        cy.wrap($img).should('have.attr', 'alt').or('have.attr', 'src');
      });
    });

  });

});
