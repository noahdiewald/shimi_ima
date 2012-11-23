shimi.utils = function() {
  var mod = {};
  
  // safer(ish) string to number. The difference is that in this app
  // I am using '' if the string isn't a valid number.
  mod.stringToNumber = function(string) {
    if (typeof string === 'string' && 
        !isNaN(string) && 
        string !== '') {
      return string * 1;
    } else {
      return '';
    }
  };
  
  // A predicate function to detect blankness
  mod.isBlank = function(value) {
    return (((/^\s*$/).test(value)) || 
      (value === null) || 
      (value === undefined) ||
      (typeof value === 'number' && isNaN(value)) ||
      (Object.prototype.toString.call(value) === '[object Array]' && value.length === 0));
  };

  mod.validID = function(id) {
    return !!id.match(/^[a-f0-9]{32}$/);
  };
  
  return mod;
};
