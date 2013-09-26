var Hogan = require('hogan.js');
var t = {
  'changelog-element' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");if(_.s(_.f("doc",c,p,1),c,p,0,8,777,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("<tr class=\"change-header\" id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("  <th");_.b("\n" + i);if(_.s(_.f("firstrow",c,p,1),c,p,0,73,188,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      id=\"first-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-element\"");_.b("\n" + i);_.b("      data-first-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-first-key=\"");_.b(_.v(_.f("encoded_key",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    ");});c.pop();}_.b(">");_.b("\n" + i);_.b("    <a");_.b("\n" + i);_.b("      href=\"#");_.b(_.v(_.f("document_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      class=\"view-document-link\">");_.b("\n" + i);_.b("      ");if(_.s(_.f("head_values",c,p,1),c,p,0,298,305,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.v(_.d(".",c,p,0)));});c.pop();}_.b("\n" + i);_.b("    </a>");_.b("\n" + i);_.b("  </th>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("change_type",c,p,0)));_.b("\n" + i);_.b("  </td>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("user",c,p,0)));_.b("\n" + i);_.b("  </td>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("timestamp",c,p,0)));_.b("\n" + i);_.b("  </td>");_.b("\n" + i);_.b("</tr>");_.b("\n" + i);if(_.s(_.f("changes",c,p,1),c,p,0,459,764,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <tr class=\"change-change\">");_.b("\n" + i);_.b("    <th>");_.b("\n" + i);_.b("      ");_.b(_.v(_.f("fieldsetLabel",c,p,0)));_.b(": ");_.b(_.v(_.f("fieldLabel",c,p,0)));_.b("\n" + i);_.b("    </th>");_.b("\n" + i);_.b("    <td colspan=3>");_.b("\n" + i);if(!_.s(_.f("originalValue",c,p,1),c,p,1,0,0,"")){_.b("      <b>Ø</b>");_.b("\n");};_.b("      ");_.b(_.v(_.f("originalValue",c,p,0)));_.b("\n" + i);_.b("      →");_.b("\n" + i);if(!_.s(_.f("newValue",c,p,1),c,p,1,0,0,"")){_.b("      <b>Ø</b>");_.b("\n");};_.b("      ");_.b(_.v(_.f("newValue",c,p,0)));_.b("\n" + i);_.b("    </td>");_.b("\n" + i);_.b("  </tr>");_.b("\n");});c.pop();}});c.pop();}return _.fl();;}),
  'charseqs-element' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<tr id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("  <th");_.b("\n" + i);if(_.s(_.f("firstrow",c,p,1),c,p,0,42,157,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      id=\"first-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-element\"");_.b("\n" + i);_.b("      data-first-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-first-key=\"");_.b(_.v(_.f("encoded_key",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    ");});c.pop();}_.b(">");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("key",c,p,0)));_.b("\n" + i);_.b("  </th>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("value",c,p,0)));_.b("\n" + i);_.b("   </td>");_.b("\n" + i);_.b("</tr>");_.b("\n");return _.fl();;}),
  'config-maintenance' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<div id=\"maintenance\">");_.b("\n" + i);_.b("  <h3>Upgrade Project</h3>");_.b("\n" + i);_.b("\n" + i);_.b("  <p>");_.b("\n" + i);_.b("    Clicking the button below will initiate an upgrade of the project");_.b("\n" + i);_.b("    core design document to the latest version available on your");_.b("\n" + i);_.b("    system.");_.b("\n" + i);_.b("  </p>");_.b("\n" + i);_.b("  <p>");_.b("\n" + i);_.b("    Be aware that this may cause significant slowness on your system");_.b("\n" + i);_.b("    while view indexes are rebuilt.");_.b("\n" + i);_.b("  </p>");_.b("\n" + i);_.b("\n" + i);_.b("  <a id=\"maintenance-upgrade-button\" class=\"maintenance-upgrade-button link-button\">Upgrade</a>");_.b("\n" + i);_.b("</div>");_.b("\n");return _.fl();;}),
  'doctypes-element' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<tr id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("  <th");_.b("\n" + i);if(_.s(_.f("firstrow",c,p,1),c,p,0,42,157,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      id=\"first-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-element\"");_.b("\n" + i);_.b("      data-first-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-first-key=\"");_.b(_.v(_.f("encoded_key",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    ");});c.pop();}_.b(">");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("key",c,p,0)));_.b("\n" + i);_.b("  </th>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    ");_.b(_.v(_.f("value",c,p,0)));_.b("\n" + i);_.b("   </td>");_.b("\n" + i);_.b("</tr>");_.b("\n");return _.fl();;}),
  'document-view-field' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<li ");_.b("\n" + i);_.b("  class=\"field-view ");_.b("\n" + i);_.b("    ");if(_.s(_.f("changed",c,p,1),c,p,0,42,49,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("changed");});c.pop();}_.b("\"");_.b("\n" + i);_.b("  data-field-field=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);if(_.s(_.f("instance",c,p,1),c,p,0,108,150,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  data-field-instance=\"");_.b(_.v(_.f("instance",c,p,0)));_.b("\"");_.b("\n");});c.pop();}_.b("  data-field-value=\"");_.b(_.v(_.f("json_value",c,p,0)));_.b("\">");_.b("\n" + i);_.b("  <b>");_.b(_.v(_.f("label",c,p,0)));_.b("</b>");if(_.s(_.f("changed",c,p,1),c,p,0,235,343,"{{ }}")){_.rs(c,p,function(c,p,_){if(!_.s(_.f("newfield",c,p,1),c,p,1,0,0,"")){_.b("<span class=\"small-control view-field-change\" title=\"");_.b(_.v(_.f("originalValue",c,p,0)));_.b("\">→</span>");};});c.pop();}_.b(":");_.b("\n" + i);if(_.s(_.f("is_textarea",c,p,1),c,p,0,375,426,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <span class=\"retain-white\">");_.b(_.v(_.f("value",c,p,0)));_.b("</span>");_.b("\n");});c.pop();}if(!_.s(_.f("is_textarea",c,p,1),c,p,1,0,0,"")){_.b("    ");_.b(_.v(_.f("value",c,p,0)));_.b("\n");};_.b("</li>");_.b("\n");return _.fl();;}),
  'document-view-tree' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");if(_.s(_.f("previous_revision",c,p,1),c,p,0,22,76,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <div id=\"revision-message\">Previous Revision</div>");_.b("\n");});c.pop();}_.b("\n" + i);if(_.s(_.f("deleted_",c,p,1),c,p,0,113,163,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <div id=\"deleted-message\"><b>Deleted</b></div>");_.b("\n");});c.pop();}_.b("\n" + i);_.b("<ul>");_.b("\n" + i);if(_.s(_.f("fieldsets",c,p,1),c,p,0,197,975,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <li");_.b("\n" + i);_.b("    class=\"fieldset-view");_.b("\n" + i);_.b("      ");if(_.s(_.f("collapse",c,p,1),c,p,0,248,257,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("collapsed");});c.pop();}_.b("\n" + i);_.b("      ");if(_.s(_.f("altered",c,p,1),c,p,0,289,296,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("changed");});c.pop();}_.b("\"");_.b("\n" + i);_.b("    data-fieldset-fieldset=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("    data-group-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("    <b>");_.b(_.v(_.f("label",c,p,0)));_.b("</b>");if(_.s(_.f("addition",c,p,1),c,p,0,414,468,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("<span title=\"fieldset added\" class=\"addition\">+</span>");});c.pop();}if(_.s(_.f("removal",c,p,1),c,p,0,493,548,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("<span title=\"fieldset removed\" class=\"removal\">−</span>");});c.pop();}_.b(":");_.b("\n" + i);if(_.s(_.f("multiple",c,p,1),c,p,0,579,818,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <ol>");_.b("\n" + i);if(_.s(_.f("multifields",c,p,1),c,p,0,613,785,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        <li>");_.b("\n" + i);_.b("          <ul class=\"multifield\">");_.b("\n" + i);if(_.s(_.f("fields",c,p,1),c,p,0,684,737,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("document-view-field",c,p,"              "));});c.pop();}_.b("          </ul>");_.b("\n" + i);_.b("        </li>");_.b("\n");});c.pop();}_.b("      </ol>");_.b("\n");});c.pop();}if(!_.s(_.f("multiple",c,p,1),c,p,1,0,0,"")){_.b("      <ul>");_.b("\n" + i);if(_.s(_.f("fields",c,p,1),c,p,0,880,925,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("document-view-field",c,p,"          "));});c.pop();}_.b("      </ul>");_.b("\n");};_.b("  </li>");_.b("\n");});c.pop();}_.b("</ul>");_.b("\n" + i);_.b("\n" + i);_.b("<div class=\"timestamps\">");_.b("\n" + i);_.b("  <dl>");_.b("\n" + i);_.b("    <dt>Created At</dt><dd class=\"timestamp\">");_.b(_.v(_.f("created_at_",c,p,0)));_.b("</dd>");_.b("\n" + i);_.b("    <dt>Created By</dt><dd>");_.b(_.v(_.f("created_by_",c,p,0)));_.b("</dd>");_.b("\n" + i);_.b("    <dt>Updated At</dt><dd class=\"timestamp\">");_.b(_.v(_.f("updated_at_",c,p,0)));_.b("</dd>");_.b("\n" + i);_.b("    <dt>Updated By</dt><dd>");_.b(_.v(_.f("updated_by_",c,p,0)));_.b("</dd>");_.b("\n" + i);_.b("    <dt>ID</dt><dd>");_.b(_.v(_.f("_id",c,p,0)));_.b("</dd>");_.b("\n" + i);_.b("  </dl>");_.b("\n" + i);_.b("</div>");_.b("\n");return _.fl();;}),
  'document-view' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");if(_.s(_.f("doctype_info",c,p,1),c,p,0,17,197,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <h2 class=\"header\">");_.b(_.v(_.f("_id",c,p,0)));_.b(" View</h2>");_.b("\n" + i);_.b("\n" + i);_.b("  <form id=\"view-jump\">");_.b("\n" + i);_.b("    <label for=\"view-jump-id\">Id</label>");_.b("\n" + i);_.b("    <input type=\"text\" id=\"view-jump-id\" name=\"view-jump-id\">");_.b("\n" + i);_.b("  </form>");_.b("\n");});c.pop();}_.b("\n" + i);_.b("<div id=\"document-view-info\"");_.b("\n" + i);_.b("     data-document-deleted=\"");_.b(_.v(_.f("deleted_",c,p,0)));_.b("\"");_.b("\n" + i);_.b("     data-document-document=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("     data-document-rev=\"");_.b(_.v(_.f("_rev",c,p,0)));_.b("\"></div>");_.b("\n" + i);_.b("\n" + i);_.b("<a id=\"document-restore-button\"");_.b("\n" + i);_.b("   data-group-id=\"document-view-info\"");_.b("\n" + i);_.b("   class=\"link-button hidden\">Restore</a>");_.b("\n" + i);_.b("\n" + i);_.b("<a id=\"document-edit-button\"");_.b("\n" + i);_.b("   data-group-id=\"document-view-info\"");_.b("\n" + i);_.b("   class=\"link-button\">Edit</a>");_.b("\n" + i);_.b("\n" + i);_.b("<a id=\"document-delete-button\"");_.b("\n" + i);_.b("   data-group-id=\"document-view-info\"");_.b("\n" + i);_.b("   class=\"link-button\">Delete</a>");_.b("\n" + i);_.b("\n" + i);_.b("<nav id=\"history\">");_.b("\n" + i);if(_.s(_.f("revs_info",c,p,1),c,p,0,726,1001,"{{ }}")){_.rs(c,p,function(c,p,_){if(_.s(_.f("status",c,p,1),c,p,0,742,985,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      <a href=\"#");_.b(_.v(_.f("_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("         class=\"revision-link\"");_.b("\n" + i);_.b("         data-group-id=\"document-view-info\"");_.b("\n" + i);if(_.s(_.f("first",c,p,1),c,p,0,864,910,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("         id=\"current-revision-link\"");_.b("\n");});c.pop();}_.b("         data-document-oldrev=\"");_.b(_.v(_.f("rev",c,p,0)));_.b("\">");_.b(_.v(_.f("count",c,p,0)));_.b("</a>");_.b("\n");});c.pop();}});c.pop();}_.b("</nav>");_.b("\n" + i);_.b("\n" + i);_.b("<div id=\"document-view-tree\">");_.b("\n" + i);_.b(_.rp("document-view-tree",c,p,"  "));_.b("</div>");_.b("\n");return _.fl();;}),
  'index-element' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<tr class=\"change-header\" id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("  <th");_.b("\n" + i);if(_.s(_.f("firstrow",c,p,1),c,p,0,64,179,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      id=\"first-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-element\"");_.b("\n" + i);_.b("      data-first-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-first-key=\"");_.b(_.v(_.f("encoded_key",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    ");});c.pop();}_.b(">");_.b("\n" + i);_.b("    <ul class=\"head-elements\">");_.b("\n" + i);if(_.s(_.f("display_key",c,p,1),c,p,0,247,362,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        <li>");_.b("\n" + i);_.b("          <a href=\"#");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("            class=\"view-document-link\">");_.b(_.v(_.d(".",c,p,0)));_.b("</a>");_.b("\n" + i);_.b("        </li>");_.b("\n");});c.pop();}_.b("    </ul>");_.b("\n" + i);_.b("  </th>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    <ul class=\"reversal-elements\">");_.b("\n" + i);if(_.s(_.f("value",c,p,1),c,p,0,455,487,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        <li>");_.b(_.v(_.d(".",c,p,0)));_.b("</li>");_.b("\n");});c.pop();}_.b("    </ul>");_.b("\n" + i);_.b("  </td>");_.b("\n" + i);_.b("</tr>");_.b("\n");return _.fl();;}),
  'index-listing' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<table>");_.b("\n" + i);_.b("  <thead>");_.b("\n" + i);_.b("    <th>Name</th>");_.b("\n" + i);_.b("    <th>Doctype</th>");_.b("\n" + i);_.b("  </thead>");_.b("\n" + i);_.b("  <tbody>");_.b("\n" + i);if(_.s(_.f("rows",c,p,1),c,p,0,91,210,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <tr>");_.b("\n" + i);_.b("      <th><a href=\"#\" data-index-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b(_.v(_.d("key.1",c,p,0)));_.b("</a></th> ");_.b("\n" + i);_.b("      <td>");_.b(_.v(_.d("key.0",c,p,0)));_.b("</td>");_.b("\n" + i);_.b("    </tr>");_.b("\n");});c.pop();}_.b("  </tbody>");_.b("\n" + i);_.b("</table>");return _.fl();;}),
  'index-options' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<option></option>");_.b("\n" + i);if(_.s(_.f("rows",c,p,1),c,p,0,27,74,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("<option value=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b(_.v(_.d("key.1",c,p,0)));_.b("</option>");_.b("\n");});c.pop();}return _.fl();;}),
  'paged-listing' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<nav class=\"pager\">");_.b("\n" + i);_.b("<a");_.b("\n" + i);_.b("  href=\"#\" ");_.b("\n" + i);_.b("  title=\"Previous Page\"");_.b("\n" + i);_.b("  id=\"previous-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-page\"");_.b("\n" + i);_.b("  class=\"pager-button link-button\"");_.b("\n" + i);_.b(">Prev</a> ");_.b("\n" + i);_.b("<a");_.b("\n" + i);_.b("  href=\"#\"");_.b("\n" + i);_.b("  title=\"Next Page\"");_.b("\n" + i);_.b("  class=\"pager-button link-button\"");_.b("\n" + i);_.b("  id=\"next-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-page\"");_.b("\n" + i);if(_.s(_.f("lastpage",c,p,1),c,p,0,322,351,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    data-last-page=\"true\"");_.b("\n");});c.pop();}if(_.s(_.f("lastrow",c,p,1),c,p,0,379,448,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    data-startkey=\"");_.b(_.v(_.f("encoded_key",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    data-startid=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n");});c.pop();}_.b(">Next</a>");_.b("\n" + i);_.b("</nav>");_.b("\n" + i);_.b("<div class=\"total-rows-info\">");_.b("\n" + i);_.b("  <b>Total</b>: ");_.b(_.v(_.f("total_rows",c,p,0)));_.b("\n" + i);_.b("</div>");_.b("\n" + i);_.b("<table>");_.b("\n" + i);if(_.s(_.f("rows",c,p,1),c,p,0,567,595,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("listed-element",c,p,"    "));});c.pop();}_.b("</table>");_.b("\n");return _.fl();;}),
  'preview-element' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<tr class=\"change-header\" id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("  <th");_.b("\n" + i);if(_.s(_.f("firstrow",c,p,1),c,p,0,64,179,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("      id=\"first-");_.b(_.v(_.f("prefix",c,p,0)));_.b("-element\"");_.b("\n" + i);_.b("      data-first-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      data-first-key=\"");_.b(_.v(_.f("encoded_key",c,p,0)));_.b("\"");_.b("\n" + i);_.b("    ");});c.pop();}_.b(">");_.b("\n" + i);_.b("    <ul class=\"head-elements\">");_.b("\n" + i);if(_.s(_.f("display_key",c,p,1),c,p,0,247,279,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        <li>");_.b(_.v(_.d(".",c,p,0)));_.b("</li>");_.b("\n");});c.pop();}_.b("    </ul>");_.b("\n" + i);_.b("  </th>");_.b("\n" + i);_.b("  <td>");_.b("\n" + i);_.b("    <ul class=\"reversal-elements\">");_.b("\n" + i);_.b("      ");_.b(_.v(_.f("value",c,p,0)));_.b("\n" + i);_.b("    </ul>");_.b("\n" + i);_.b("  </td>");_.b("\n" + i);_.b("</tr>");_.b("\n");return _.fl();;}),
  'search-field-item' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<a class='search-field-item' ");_.b("\n" + i);_.b("  title='click to remove' ");_.b("\n" + i);_.b("  data-field-field='");_.b(_.v(_.f("field",c,p,0)));_.b("' ");_.b("\n" + i);_.b("  href='#'>");_.b(_.v(_.f("fieldLabel",c,p,0)));_.b("</a>");_.b("\n");return _.fl();;}),
  'set-listing' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<div class=\"total-rows-info\">");_.b("\n" + i);_.b("  <b>Total</b>: <span id=\"total-set-rows\">");_.b(_.v(_.f("total",c,p,0)));_.b("</span>");_.b("\n" + i);_.b("</div>");_.b("\n" + i);_.b("<div id=\"save-set-results\">");_.b("\n" + i);_.b("  <a href=\"#\">(Save Selected)</a>");_.b("\n" + i);_.b("</div>");_.b("\n" + i);_.b("<table id=\"set-elements\">");_.b("\n" + i);_.b("  <thead>");_.b("\n" + i);_.b("    <tr>");_.b("\n" + i);_.b("      <td>");_.b("\n" + i);_.b("        <input type=\"checkbox\" id=\"select-all-set-elements\" title=\"Click to select or deselect all elements\" />");_.b("\n" + i);_.b("      </td>");_.b("\n" + i);_.b("      <th>");_.b("\n" + i);_.b("        Elements");_.b("\n" + i);_.b("      </th>");_.b("\n" + i);_.b("    </tr>");_.b("\n" + i);_.b("  </thead>");_.b("\n" + i);_.b("  <tbody>");_.b("\n" + i);if(_.s(_.f("elements",c,p,1),c,p,0,435,674,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <tr>");_.b("\n" + i);_.b("      <td>");_.b("\n" + i);_.b("        <input type=\"checkbox\" class=\"set-element-selection\" title=\"Click to select element\" />");_.b("\n" + i);_.b("      </td>");_.b("\n" + i);_.b("      <td>");_.b("\n" + i);_.b("        <a class=\"view-document-link\" href=\"#");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b(_.v(_.f("context",c,p,0)));_.b("</a>");_.b("\n" + i);_.b("      </td>");_.b("\n" + i);_.b("    </tr>");_.b("\n");});c.pop();}_.b("  </tbody>");_.b("\n" + i);_.b("</table>");_.b("\n");return _.fl();;}),
  'set-options' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<option></option>");_.b("\n" + i);if(_.s(_.f("names",c,p,1),c,p,0,28,66,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("<option value=\"");_.b(_.v(_.d(".",c,p,0)));_.b("\">");_.b(_.v(_.d(".",c,p,0)));_.b("</option>");_.b("\n");});c.pop();}return _.fl();;}),
  'simple-to-form-field' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<li>");_.b("\n" + i);_.b("  <label for=\"");_.b(_.v(_.f("key",c,p,0)));_.b("\">");_.b(_.v(_.f("key",c,p,0)));_.b("</label>");_.b("\n" + i);if(!_.s(_.f("text",c,p,1),c,p,1,0,0,"")){_.b("  <input type=\"");if(_.s(_.f("string",c,p,1),c,p,0,86,90,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("text");});c.pop();}if(_.s(_.f("number",c,p,1),c,p,0,112,118,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("number");});c.pop();}_.b("\" name=\"");_.b(_.v(_.f("key",c,p,0)));_.b("\" value=\"");_.b(_.v(_.f("val",c,p,0)));_.b("\"/>");_.b("\n");};if(_.s(_.f("text",c,p,1),c,p,0,191,246,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <textarea name=\"");_.b(_.v(_.f("key",c,p,0)));_.b("\">");_.b(_.v(_.f("val",c,p,0)));_.b("</textarea>");_.b("\n");});c.pop();}_.b("</li>");_.b("\n");return _.fl();;}),
  'simple-to-form' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<form>");_.b("\n" + i);_.b("  <ul>");_.b("\n" + i);if(_.s(_.f("fields",c,p,1),c,p,0,29,67,"{{ }}")){_.rs(c,p,function(c,p,_){_.b(_.rp("simple-to-form-field",c,p,"      "));});c.pop();}_.b("  </ul>");_.b("\n" + i);_.b("</form>");_.b("\n");return _.fl();;}),
  'worksheet' : new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<table id=\"worksheet-table\">");_.b("\n" + i);_.b("  <thead>");_.b("\n" + i);_.b("    <tr class=\"header-row\">");_.b("\n" + i);_.b("      <td id=\"select-all-worksheet-rows-cell\"");_.b("\n" + i);_.b("        class=\"select-column\">");_.b("\n" + i);_.b("        <input ");_.b("\n" + i);_.b("          id=\"select-all-worksheet-rows\"");_.b("\n" + i);_.b("          type=\"checkbox\"");_.b("\n" + i);_.b("          title=\"Click to select all rows\">");_.b("\n" + i);_.b("      </td>");_.b("\n" + i);if(_.s(_.f("fieldsets",c,p,1),c,p,0,303,1494,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        <th ");_.b("\n" + i);_.b("          class=\"worksheet-handle-header fieldset handle-column ");_.b(_.v(_.f("_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("          title=\"");_.b(_.v(_.f("label",c,p,0)));_.b("\">");_.b("\n" + i);_.b("          <div>");_.b("\n" + i);_.b("            <span>");_.b("\n" + i);_.b("              <a class=\"fieldset-handle\" ");_.b("\n" + i);_.b("                data-field-fieldset=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("                href=\"#\">");_.b(_.v(_.f("label",c,p,0)));_.b("</a></span></div>");_.b("\n" + i);_.b("        </th>");_.b("\n" + i);if(_.s(_.f("fields",c,p,1),c,p,0,634,1476,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          <th");_.b("\n" + i);_.b("            class=\"");if(_.s(_.f("multiple",c,p,1),c,p,0,681,689,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("multiple");});c.pop();}_.b(" ");if(_.s(_.f("collapse",c,p,1),c,p,0,716,724,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("collapse");});c.pop();}_.b(" ");_.b(_.v(_.f("fieldset",c,p,0)));_.b(" ");_.b(_.v(_.f("_id",c,p,0)));_.b(" worksheet-handle-header field handle-column\"");_.b("\n" + i);_.b("            title=\"");_.b(_.v(_.f("label",c,p,0)));_.b("\">");_.b("\n" + i);_.b("            <div>");_.b("\n" + i);_.b("              <span>");_.b("\n" + i);_.b("                <a class=\"field-handle\" ");_.b("\n" + i);_.b("                  data-field-field=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("                  href=\"#\">");_.b(_.v(_.f("label",c,p,0)));_.b("</a></span></div>");_.b("\n" + i);_.b("          </th>");_.b("\n" + i);_.b("          <th");_.b("\n" + i);_.b("            class=\"");_.b(_.v(_.f("fieldset",c,p,0)));_.b(" ");_.b(_.v(_.f("_id",c,p,0)));_.b(" field-column\"");_.b("\n" + i);_.b("            title=\"");_.b(_.v(_.f("label",c,p,0)));_.b("\">");_.b("\n" + i);_.b("            <a class=\"field-header\" ");_.b("\n" + i);_.b("              data-field-field=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("              href=\"#\">");_.b(_.v(_.f("label",c,p,0)));_.b("</a>");_.b("\n" + i);_.b("            <input ");_.b("\n" + i);_.b("              class=\"select-worksheet-column\"");_.b("\n" + i);_.b("              data-field-field=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\" ");_.b("\n" + i);_.b("              type=\"checkbox\"");_.b("\n" + i);_.b("              title=\"Click to select column\">");_.b("\n" + i);_.b("          </td>");_.b("\n");});c.pop();}});c.pop();}_.b("    </tr>");_.b("\n" + i);_.b("  </thead>");_.b("\n" + i);_.b("  <tbody>");_.b("\n" + i);_.b("    <%#rows%>");_.b("\n" + i);_.b("      <tr id=\"worksheet-row-<% _id %>\"");_.b("\n" + i);_.b("        class=\"body-row\">");_.b("\n" + i);_.b("        <td class=\"select-column\">");_.b("\n" + i);_.b("          <input ");_.b("\n" + i);_.b("            class=\"select-worksheet-row\"");_.b("\n" + i);_.b("            data-row=\"worksheet-row-<% _id %>\"");_.b("\n" + i);_.b("            type=\"checkbox\"");_.b("\n" + i);_.b("            title=\"Click to select row\">");_.b("\n" + i);_.b("        </td>");_.b("\n" + i);if(_.s(_.f("fieldsets",c,p,1),c,p,0,1865,2890,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          <td class=\"");_.b(_.v(_.f("_id",c,p,0)));_.b(" fieldset handle-column\"></td>");_.b("\n" + i);if(_.s(_.f("fields",c,p,1),c,p,0,1948,2870,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("            <td class=\"");if(_.s(_.f("multiple",c,p,1),c,p,0,1985,1993,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("multiple");});c.pop();}_.b(" ");if(_.s(_.f("collapse",c,p,1),c,p,0,2020,2028,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("collapse");});c.pop();}_.b(" ");_.b(_.v(_.f("_id",c,p,0)));_.b(" ");_.b(_.v(_.f("fieldset",c,p,0)));_.b(" field handle-column\"></td>");_.b("\n" + i);_.b("            <td");_.b("\n" + i);_.b("              class=\"");if(_.s(_.f("multiple",c,p,1),c,p,0,2144,2152,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("multiple");});c.pop();}_.b(" ");if(_.s(_.f("collapse",c,p,1),c,p,0,2179,2187,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("collapse");});c.pop();}_.b(" ");_.b(_.v(_.f("fieldset",c,p,0)));_.b(" ");_.b(_.v(_.f("_id",c,p,0)));_.b(" field-column\"");_.b("\n" + i);_.b("              data-field-fieldset=\"");_.b(_.v(_.f("fieldset",c,p,0)));_.b("\"");_.b("\n" + i);_.b("              data-field-field=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("              <%#");_.b(_.v(_.f("_id",c,p,0)));_.b("%>");_.b("\n" + i);_.b("                <%#multiple%>");_.b("\n" + i);_.b("                <ol>");_.b("\n" + i);_.b("                  <%#items%>");_.b("\n" + i);_.b("                    <li");_.b("\n" + i);_.b("                      data-field-fieldset_instance=\"<% fieldset_instance %>\"");_.b("\n" + i);_.b("                      data-field-field_instance=\"<% field_instance %>\"><% value %></li>");_.b("\n" + i);_.b("                  <%/items%>");_.b("\n" + i);_.b("                </ol>");_.b("\n" + i);_.b("                <%/multiple%>");_.b("\n" + i);_.b("                <%#single%>");_.b("\n" + i);_.b("                  <span><% value %></span>");_.b("\n" + i);_.b("                <%/single%>");_.b("\n" + i);_.b("              <%/");_.b(_.v(_.f("_id",c,p,0)));_.b("%>");_.b("\n" + i);_.b("            </td>");_.b("\n");});c.pop();}});c.pop();}_.b("      </tr>");_.b("\n" + i);_.b("    <%/rows%>");_.b("\n" + i);_.b("  </tbody>");_.b("\n" + i);_.b("</table>");_.b("\n");return _.fl();;})
},
r = function(n) {
  var tn = t[n];
  return function(c, p, i) {
    return tn.render(c, p || t, i);
  }
};
module.exports = {
  templates : t,
  'changelog-element' : r('changelog-element'),
  'charseqs-element' : r('charseqs-element'),
  'config-maintenance' : r('config-maintenance'),
  'doctypes-element' : r('doctypes-element'),
  'document-view-field' : r('document-view-field'),
  'document-view-tree' : r('document-view-tree'),
  'document-view' : r('document-view'),
  'index-element' : r('index-element'),
  'index-listing' : r('index-listing'),
  'index-options' : r('index-options'),
  'paged-listing' : r('paged-listing'),
  'preview-element' : r('preview-element'),
  'search-field-item' : r('search-field-item'),
  'set-listing' : r('set-listing'),
  'set-options' : r('set-options'),
  'simple-to-form-field' : r('simple-to-form-field'),
  'simple-to-form' : r('simple-to-form'),
  'worksheet' : r('worksheet')
};