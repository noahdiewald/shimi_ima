var should = require('chai').should();
var xml = require('lib/xml');

var root = function (options) {
  'use strict';

  return ['<row>', '</row>'];
};

describe('Converting JSON to XML', function () {
  'use strict';

  it('should properly convert a simple row', function () {
    var original = '{"id":"0fa67152efaca5482aa13c55af26fa91","key":"Config Refactor","value":"project-0fa67152efaca5482aa13c55af26fa91"}';
    var transformed = '<row><id type="string">0fa67152efaca5482aa13c55af26fa91</id><key type="string">Config Refactor</key><value type="string">project-0fa67152efaca5482aa13c55af26fa91</value></row>';

    xml.to_xml(original, {
      root: root
    }).should.equal(transformed);
  });

  it('should properly convert a row with a complex key', function () {
    var original = '{"id":"0fa67152efaca5482aa13c55af26fa91","key":["Config Refactor",{"a":67}],"value":"project-0fa67152efaca5482aa13c55af26fa91"}';
    var transformed = '<row><id type="string">0fa67152efaca5482aa13c55af26fa91</id><key type="array"><item index="0" type="string">Config Refactor</item><item index="1" type="object"><a type="number">67</a></item></key><value type="string">project-0fa67152efaca5482aa13c55af26fa91</value></row>';

    xml.to_xml(original, {
      root: root
    }).should.equal(transformed);
  });

  it('should properly convert a row with a document', function () {
    var original = '{"id":"660f254b0e476024ff61e66ae4017b9b","key":"Production Env 01","value":"project-660f254b0e476024ff61e66ae4017b9b","doc":{"_id":"660f254b0e476024ff61e66ae4017b9b","_rev":"1-44b6b02839e225a32ed4fab437c6c65f","name":"Production Env 01","description":"A production-like environment"}}';
    var transformed = '<row><id type="string">660f254b0e476024ff61e66ae4017b9b</id><key type="string">Production Env 01</key><value type="string">project-660f254b0e476024ff61e66ae4017b9b</value><doc type="object"><_id type="string">660f254b0e476024ff61e66ae4017b9b</_id><_rev type="string">1-44b6b02839e225a32ed4fab437c6c65f</_rev><name type="string">Production Env 01</name><description type="string">A production-like environment</description></doc></row>';

    xml.to_xml(original, {
      root: root
    }).should.equal(transformed);
  });
});
