$(function ()
{
  'use strict';

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
