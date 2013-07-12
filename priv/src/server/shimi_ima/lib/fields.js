var identity = function (value)
{
  'use strict';

  return value;
};

var fromFieldset = function (fieldset, mapfun)
{
  'use strict';

  var map = function (fields)
  {
    return fields.map(function (x)
    {
      return mapfun(x, fieldset);
    });
  };

  if (fieldset.multiple)
  {
    return fieldset.multifields.reduce(function (acc, multifield)
    {
      return Array.concat(acc, map(multifield.fields));
    }, []);
  }
  else
  {
    return map(fieldset.fields);
  }
};

var fromFieldsetsMapFold = function (fieldsets, mapfun, foldfun, init)
{
  'use strict';

  if (init === undefined)
  {
    init = [];
  }
  return fieldsets.reduce(function (acc, fieldset)
  {
    return foldfun(acc, fromFieldset(fieldset, mapfun), fieldset);
  }, init);
};

var fromFieldsets = function (fieldsets)
{
  'use strict';

  return fromFieldsetsMapFold(fieldsets, identity, function (acc, fields)
  {
    return Array.concat(acc, fields);
  });
};

var fromFieldsetsFold = function (fieldsets, foldfun, init)
{
  'use strict';

  return fromFieldsetsMapFold(fieldsets, identity, foldfun, init);
};

var fromFieldsetsMap = function (fieldsets, mapfun)
{
  'use strict';

  return fromFieldsetsMapFold(fieldsets, mapfun, function (acc, fields)
  {
    return Array.concat(acc, fields);
  });
};

exports.fromFieldsetsMapFold = fromFieldsetsMapFold;
exports.fromFieldsets = fromFieldsets;
exports.fromFieldsetsFold = fromFieldsetsFold;
exports.fromFieldsetsMap = fromFieldsetsMap;
