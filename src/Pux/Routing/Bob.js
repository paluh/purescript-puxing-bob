/* global exports */
"use strict";

// module Pux.Routing.Bob

exports.push = function(url) {
  window.history.pushState({}, document.title, url);
};
