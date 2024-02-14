// Greying-out function
shinyjs.loading = function() {
  // Show the loading screen when the app starts
  $('.wrapper').append('<div class="greyed-out">\
    <div class="loader"></div></div>');
};

// Loading panel/spinner remove function
shinyjs.finishedLoading = function() {
  $('.greyed-out').remove();
};