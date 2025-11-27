// app/js/index.js
// WBES Dashboard JavaScript

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
