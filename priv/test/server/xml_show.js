var should = require('chai').should();
var xml = require('lib/xml');
var simple = require('simple_doc').simple_doc;
var crypto = require('crypto');

var root = function () {
  'use strict';

  return ['<doc>', '</doc>'];
};

describe('Converting JSON to XML', function () {
  'use strict';

  it('should properly convert a simple document', function () {
    var xmlString = xml.to_xml(JSON.stringify(simple), {
      root: root
    });
    var md5 = crypto.createHash('md5').update(xmlString).digest('hex');

    md5.should.equal('14e322740e470c0edcf20dc103ef397b');
  });
});
