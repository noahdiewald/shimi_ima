var casper = require('casper').create();

casper.start('http://127.0.0.1:8000/projects', function() {
    this.test.assertHttpStatus(401, 'no valid login');
});

casper.run(function() {
    this.test.renderResults(true);
});
