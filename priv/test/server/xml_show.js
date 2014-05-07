var should = require('chai').should();
var xml_show = require('lib/xml_show');
var simple = require('simple_doc').simple_doc;
var crypto = require('crypto');

describe('Converting JSON to XML', function () {
  'use strict';

  it('should properly convert a simple document', function () {
    var xml = xml_show.to_xml(JSON.stringify(simple));
    var md5 = crypto.createHash('md5').update(xml).digest('hex');

    md5.should.equal('14e322740e470c0edcf20dc103ef397b');
  });
});
