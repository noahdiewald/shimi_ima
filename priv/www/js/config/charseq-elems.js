/*
 * Copyright 2011 University of Wisconsin Madison Board of Regents.
 * 
 * This file is part of dictionary_maker.
 * 
 * dictionary_maker is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at
 * your option) any later version.
 * 
 * dictionary_maker is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with dictionary_maker. If not, see <http://www.gnu.org/licenses/>.
 * 
 * Author Noah Diewald <noah@diewald.me>
 * 
 * About this file
 * 
 * A charaseq is a collection of information used in definining properies
 * of a script, including some phonological information and information
 * used for collation of items written in the script.
*/

var charseqElems = function() {
  var cElems = {};
  
  cElems.attrs = ["description", "characters", "name", "sort_ignore", "locale", "tailoring", "vowels", "consonants", "ietf_tag", "iso639_tag", "charseq", "rev"];
  
  cElems.get = function(values) {
    var cObj = {};
    
    cObj.attrs = cElems.attrs;
    
    cObj.copyValues = function(source) {
      Object.keys(source).forEach(function(field) {
        cObj[field].val(source[field]);
      });
      return cObj;
    };
    
    cObj.getCharseqInputVals = function() {
      var valObj = {
        "category": "charseq",
        "description": cObj.description.val(),
        "characters": fObj.characters.val().parseQuoted(),
        "name": fObj.name.val(),
        "sort_ignore": fObj.sort_ignore.val().parseQuoted(),
        "locale": fObj.locale.val(),
        "tailoring": fObj.tailoring.val(),
        "vowels": fObj.vowels.val().parseQuoted(),
        "consonants": fObj.consonants.val().parseQuoted(),
        "ietf_tag": fObj.ietf_tag.val().parseQuoted(),
        "iso639_tag": fObj.iso639_tag.val().parseQuoted(),
        "_id": cObj.charseq.val(),
        "_rev": cObj.rev.val()
      }
      return valObj;
    };
    
    cObj.clear = function() {
      clearValues($('#charseq-dialog .input')).removeClass('ui-state-error');
      return cObj;
    };
                   
    cObj.attrs.forEach(function(item) {
      cObj[item] = $('#charseq-' + item + '-input');
    });
    
    cObj.copyValues(values);
      
    return cObj;
  };
  
  return cElems;
};