var shimi = {};

shimi.utils = require('./utils.js');
shimi.csv = require('./csv.js');
shimi.sets = require('./sets.js');
shimi.flash = require('./flash.js');
shimi.store = require('./store.js');
shimi.path = require('./path.js');
require('./jquery.hotkeys.js');
require('./jquery-ui-input-state.js');
require('./click-dispatch.js');
require('./dblclick-dispatch.js');
require('./keystrokes.js');
require('./changes.js');
require('./gen-dispatch.js');
shimi.pager = require('./pager.js');
shimi.panelToggle = require('./panel-toggle.js');
shimi.form = require('./form.js');
shimi.sess = require('./sess.js');
shimi.config = require('./config/config.js');
shimi.documents = require('./documents/documents.js');
shimi.fm = require('./file_manager/fm.js');
shimi.ilistingui = require('./index_tool/ilistingui.js');
shimi.projectui = require('./projects/projectui.js');

// A place to temporarily store global objects
shimi.globals = {};

// functions added to String
String.prototype.isBlank = function ()
{
  'use strict';

  return ((/^\s*$/).test(this) && !(/\S/).test(this) && (this !== null));
};

String.prototype.trim = function ()
{
  'use strict';

  return this.replace(/^\s+/, '').replace(/\s+$/, '');
};

// functions added to Array
Array.prototype.trimAll = function ()
{
  'use strict';

  return this.map(function (i)
  {
    return i.trim();
  }).filter(function (i)
  {
    return !i.match(/^$/);
  });
};

$(function ()
{
  'use strict';

  $('body').click(function (e)
  {
    shimi.clickDispatch(e);
  });
  $('body').dblclick(function (e)
  {
    shimi.dblclickDispatch(e);
  });

  $('.notification').hide();

  $('#loading').hide();

  $(document).ajaxStart(function ()
  {
    $('#loading').show();
  }).ajaxStop(function ()
  {
    $('#loading').hide();
  });

  shimi.form.initDateFields();

  // Config
  if ($('#configuration').length > 0)
  {
    shimi.initTabs();
    $('.simple-tabs').tabs();
  }

  // Documents
  if ($('#all-document-container').length > 0)
  {
    shimi.documents.init();
  }

  // File Manager
  if ($('#file-upload').length > 0)
  {
    shimi.fm.init();
  }

  // Index Tool
  if ($('#all-index-container').length > 0)
  {
    shimi.ilistingui.init();
  }

  // Project
  if ($('#projects-container').length > 0)
  {
    shimi.projectui.init();
  }
});
