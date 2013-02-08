$(function () {
  $('.notification').hide();

  $('#loading').hide().ajaxStart(function () {
    $(this).show();
  }).ajaxStop(function () {
    $(this).hide();
  });

  shimi.form.initDateFields();

  // Config
  if ($('#configuration').length > 0) {
    shimi.initTabs();
    $('.simple-tabs').tabs();
  }

  // Documents
  if ($('#all-document-container').length > 0) {
    shimi.documents.init();
  }

  // File Manager
  if ($('#file-upload').length > 0) {
    shimi.fm.refreshListings();

    $('#file-upload-target').load(function () {
      var encoded = $('#file-upload-target').contents().find('body pre').html();
      var obj = function () {
        if (encoded && encoded.length > 0) {
          return JSON.parse(encoded);
        } else {
          return {
            message: false
          };
        }
      };

      if (obj() && obj().message && obj().status === "error") {
        shimi.flash("Error", obj().message).error();
        shimi.fm.refreshListings();
      } else if (obj().message) {
        shimi.flash("Success", obj().message).highlight();
        shimi.fm.refreshListings();
      }
    });
  }

  // Index Tool
  if ($('#all-index-container').length > 0) {
    shimi.ilistingui.init();
  }

  // Project
  if ($('#projects-container').length > 0) {
    shimi.projectui.init();
  }
});