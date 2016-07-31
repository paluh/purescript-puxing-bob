/* global exports */
"use strict";

// module Pux.Routing.Bob

exports.push = function(url) {
  return function() {
    window.history.pushState({}, document.title, url);
  }
};
