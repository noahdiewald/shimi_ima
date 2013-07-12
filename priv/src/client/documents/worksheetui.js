shimi.worksheetui = (function ()
{
  'use strict';

  var mod = {};
  var setsui = shimi.setsui;

  var worksheetsSet = function ()
  {
    return $('#document-worksheets-set-input');
  };
  var worksheetsArea = function ()
  {
    return $('#worksheet-area');
  };
  var worksheetName = function ()
  {
    return shimi.documents.identifier() + '_worksheet-template';
  };

  mod.selectAllRows = function (select)
  {
    if (select)
    {
      $('#worksheet-table tbody tr').addClass('selected-row');
      $('#worksheet-table tbody tr input').prop('checked', true);
    }
    else
    {
      $('#worksheet-table tbody tr').removeClass('selected-row');
      $('#worksheet-table tbody tr input:checked').prop('checked', false);
    }

    return mod;
  };

  // Set the proper class for a selected row and unset the 'select all'
  mod.rowSelection = function (row, select)
  {
    if (select)
    {
      $('#' + row).addClass('selected-row');
      $('#select-all-worksheet-rows').prop('checked', false);
    }
    else
    {
      $('#' + row).removeClass('selected-row');
      $('#select-all-worksheet-rows').prop('checked', false);
    }

    return mod;
  };

  mod.columnSelection = function (column, select)
  {
    if (select)
    {
      $('.field-column.' + column).addClass('selected-column');
    }
    else
    {
      $('.field-column.' + column).removeClass('selected-column');
    }

    return mod;
  };

  mod.showHandles = function ()
  {
    $('#worksheet-table .handle-column.fieldset').show();

    return mod;
  };

  mod.hideHandles = function ()
  {
    $('#worksheet-table .handle-column.fieldset').hide();

    return mod;
  };

  mod.showFieldset = function (fsid)
  {
    $('#worksheet-table .handle-column.field.' + fsid).show();

    return mod;
  };

  mod.hideFieldset = function (fsid)
  {
    $('#worksheet-table .handle-column.field.' + fsid).hide();

    return mod;
  };

  mod.showField = function (fid)
  {
    $('.field-column.' + fid).show();

    return mod;
  };

  mod.hideField = function (fid)
  {
    $('.field-column.' + fid).hide();

    return mod;
  };

  mod.buildTemplate = function ()
  {
    var doctypeInfo = shimi.documents.info();
    var metaTemp = '{{=<% %>=}}\n' + templates['worksheet'].render(doctypeInfo);
    shimi.globals[worksheetName()] = Hogan.compile(metaTemp);

    return mod;
  };

  mod.fillWorksheet = function ()
  {
    var setName = worksheetsSet().val();
    var url = 'worksheets';
    var complete = function (_ignore, req)
    {
      var data = JSON.parse(req.responseText);
      var ws = shimi.globals[worksheetName()].render(data);
      worksheetsArea().html(ws);
    };

    if (!setName.isBlank())
    {
      var thisSet = setsui.getSet(setName)[1];

      if (thisSet.lenght <= 250)
      {
        var setIds = thisSet.map(function (x)
        {
          return x[1];
        });

        shimi.form.send(url, setIds, 'POST', complete);
      }
      else
      {
        shimi.flash('Could not load worksheet', 'the current set size is limited to 250 items.').error();
      }
    }

    return mod;
  };

  return mod;
})();
