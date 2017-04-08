var page = require('webpage').create();
var system = require('system');
//  t, address;

if (system.args.length === 1) {
  console.log('Usage: utest.js <some URL>');
  phantom.exit();
}

var address = system.args[1];
page.settings.resourceTimeout = 10;
page.open(address, function(status) {
  if (status !== 'success') {
    console.log('FAILED to load ' + address);
  } else {
    console.log('Loading ' + system.args[1]);
  }
  var content = page.evaluate(function() {
      var body = document.getElementsByTagName('body')[0];
      return body.innerText;
  });
  console.log(content);
  phantom.exit();
});
