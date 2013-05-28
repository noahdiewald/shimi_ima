var casper = require('casper').create();

casper.start('http://127.0.0.1:8000/projects', function() {
  'use strict';
  casper.test.assertHttpStatus(200, 'login accepted');
});

casper.setHttpAuth('tester', 'tester');

casper.run(function() {
  'use strict';
  this.test.renderResults(true);
});
