// # Change Event Handling
//
// *Implicit depends:* DOM, JQuery, JQueryUI
//
// Like [`click-dispatch.js`](./click-dispatch.html) I would like
// to centralize the keystroke events. This is a start and a bit of
// an experiment. It uses the JQuery `on()` function, which was not
// available when I first began programming this application. It also
// uses the JQuery hotkeys plugin, which I'd like to remove at some point.

// ## Variable Definitions

var hotkeys = require('./jquery.hotkeys.js');
var sender = require('./sender.js');
var ipreviewui = require('./index_tool/ipreviewui.js');
var indexui = require('./documents/indexui.js');
var changeui = require('./documents/changeui.js');
var editui = require('./documents/editui.js');
var viewui = require('./documents/viewui.js');
var searchui = require('./documents/searchui.js');

// # Exported Functions

// All this does is register a bunch of event handlers.
var keystrokes = function ()
{
  'use strict';

  [ipreviewui, indexui, changeui].forEach(function (mod)
  {
    var keyupHandler = function (e)
    {
      var getIndexTimer;
      window.clearTimeout(getIndexTimer);
      getIndexTimer = setTimeout(function ()
      {
        if (e.which !== 8 && e.which !== 46)
        {
          mod.get();
        }
      }, 500);
    };

    document.addEventListener('keyup', function(e)
    {
      if (e.target.id === mod.prefix() + '-filter')
      {
        keyupHandler(e);
      }
      else if (e.target.id === mod.prefix() + '-limit')
      {
        keyupHandler(e);
      }
    });
  });

  $(document).on('keydown', '#document-worksheets-form', function (e)
  {
    if (e.which === 13)
    {
      sender('worksheet-form-submit');
      return false;
    }
    return true;
  });

  $(document).on('keydown', '#document-sets-form', function (e)
  {
    if (e.which === 13)
    {
      sender('sets-form-submit');
      return false;
    }
    return true;
  });

  $('#new-set-form').on('keydown', function (e)
  {
    if (e.which === 13)
    {
      sender('new-set-form-submit');
      return false;
    }
    return true;
  });

  $(document).bind('keydown', 'Alt+n', function (e)
  {
    var t = function ()
    {
      return $('#edit-tabs');
    };
    var totaltabs = t().find('li').length;
    var selected = t().tabs('option', 'active');

    if (selected < totaltabs - 1)
    {
      t().tabs('option', 'active', selected + 1);
      sender('lost-focus');
    }
    else
    {
      t().tabs('option', 'active', 0);
      sender('lost-focus');
    }

    return false;
  });

  $(document).bind('keydown', 'Alt+c', function (e)
  {
    var active = $(document.activeElement).attr('id');
    sender('initiated-command', active);
    return true;
  });

  $(document).bind('keydown', 'Alt+p', function (e)
  {
    var t = function ()
    {
      return $('#edit-tabs');
    };
    var totaltabs = t().find('li').length;
    var selected = t().tabs('option', 'active');

    if (selected !== 0)
    {
      t().tabs('option', 'active', selected - 1);
      sender('lost-focus');
    }
    else
    {
      t().tabs('option', 'active', totaltabs - 1);
      sender('lost-focus');
    }

    return false;
  });


  $(document).on('keydown', '#edit-command-input', function (e)
  {
    if (e.which === 13)
    {
      var command = $('#edit-command-input').val();
      sender('submitted-command', command);
    }
    return true;
  });

  $(document).on('keydown', '#edit-document-form input', function (e)
  {
    if (e.which === 13)
    {
      if ($('#save-document-button').css('display') === 'none')
      {
        editui.create();
      }
      else
      {
        editui.save();
      }
    }
    return true;
  });

  $(document).on('keydown', '#edit-document-form textarea', 'Alt+x', function (e)
  {
    editui.toggleTextarea($(e.target));
    return false;
  });

  $(document).on('keypress', '#view-jump-id', function (e)
  {
    if (e.which === 13)
    {
      var docid = $('#view-jump-id').val();
      viewui.get(docid);
      return false;
    }
    return true;
  });

  $(document).on('keydown', '#document-search-term', function (e)
  {
    if (e.which === 13)
    {
      searchui.getSearch();
      return false;
    }
    return true;
  });

  return true;
};

exports.keystrokes = keystrokes;
