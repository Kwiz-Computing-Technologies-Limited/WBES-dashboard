// app/js/index.js
// WBES Dashboard JavaScript

// Device detection and UI routing
// Threshold: 768px is a common breakpoint for mobile/tablet distinction
const MOBILE_BREAKPOINT = 768;

// Detect if device should use mobile UI
function isMobileDevice() {
  // Check screen width
  const isNarrowScreen = window.innerWidth <= MOBILE_BREAKPOINT;

  // Check for touch capability as secondary signal
  const isTouchDevice = 'ontouchstart' in window ||
    navigator.maxTouchPoints > 0 ||
    navigator.msMaxTouchPoints > 0;

  // Check user agent for mobile devices (fallback)
  const mobileUserAgent = /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent);

  // Primary decision is based on screen width, with touch as a tie-breaker
  return isNarrowScreen || (isTouchDevice && mobileUserAgent);
}

// Get device info for Shiny
function getDeviceInfo() {
  return {
    screenWidth: window.innerWidth,
    screenHeight: window.innerHeight,
    isMobile: isMobileDevice(),
    isTouchDevice: 'ontouchstart' in window || navigator.maxTouchPoints > 0,
    userAgent: navigator.userAgent,
    pixelRatio: window.devicePixelRatio || 1
  };
}

// Send device info to Shiny on connection
$(document).on('shiny:connected', function() {
  Shiny.setInputValue('device_info', getDeviceInfo(), {priority: 'event'});
});

// Update on resize (with debounce)
let resizeTimeout;
$(window).on('resize', function() {
  clearTimeout(resizeTimeout);
  resizeTimeout = setTimeout(function() {
    const newDeviceInfo = getDeviceInfo();
    Shiny.setInputValue('device_info', newDeviceInfo, {priority: 'event'});

    // Check if we need to switch UI
    const currentUI = $('body').data('ui-mode');
    const shouldBeMobile = newDeviceInfo.isMobile;

    if (currentUI === 'desktop' && shouldBeMobile) {
      Shiny.setInputValue('request_ui_switch', 'mobile', {priority: 'event'});
    } else if (currentUI === 'mobile' && !shouldBeMobile) {
      Shiny.setInputValue('request_ui_switch', 'desktop', {priority: 'event'});
    }
  }, 250);
});

// Custom Shiny handlers
$(document).ready(function() {
  // Initialize tooltips
  $('[data-toggle="tooltip"]').tooltip();

  // Smooth scroll for anchor links
  $('a[href^="#"]').on('click', function(e) {
    e.preventDefault();
    var target = $(this.getAttribute('href'));
    if (target.length) {
      $('html, body').animate({
        scrollTop: target.offset().top - 70
      }, 500);
    }
  });
});

// Export functionality
Shiny.addCustomMessageHandler('downloadData', function(message) {
  var blob = new Blob([message.data], { type: message.type });
  var link = document.createElement('a');
  link.href = window.URL.createObjectURL(blob);
  link.download = message.filename;
  link.click();
});

// Notification handler
Shiny.addCustomMessageHandler('showNotification', function(message) {
  var notification = $('<div class="alert alert-' + message.type + ' alert-dismissible fade show" role="alert">' +
    message.text +
    '<button type="button" class="btn-close" data-bs-dismiss="alert" aria-label="Close"></button>' +
    '</div>');
  
  $('#notification-area').append(notification);
  
  setTimeout(function() {
    notification.alert('close');
  }, 5000);
});
