var templates = {};
templates["set-options"] = new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<option></option>");_.b("\n" + i);if(_.s(_.f("names",c,p,1),c,p,0,28,66,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("<option value=\"");_.b(_.v(_.d(".",c,p,0)));_.b("\">");_.b(_.v(_.d(".",c,p,0)));_.b("</option>");_.b("\n");});c.pop();}return _.fl();;});
templates["set-listing"] = new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<div class=\"total-rows-info\">");_.b("\n" + i);_.b("  <b>Total</b>: <span id=\"total-set-rows\">");_.b(_.v(_.f("total",c,p,0)));_.b("</span>");_.b("\n" + i);_.b("</div>");_.b("\n" + i);_.b("<div id=\"save-set-results\">");_.b("\n" + i);_.b("  <a href=\"#\">(Save Selected)</a>");_.b("\n" + i);_.b("</div>");_.b("\n" + i);_.b("<table id=\"set-elements\">");_.b("\n" + i);_.b("  <thead>");_.b("\n" + i);_.b("    <tr>");_.b("\n" + i);_.b("      <td>");_.b("\n" + i);_.b("        <input type=\"checkbox\" id=\"select-all-set-elements\" title=\"Click to select or deselect all elements\" />");_.b("\n" + i);_.b("      </td>");_.b("\n" + i);_.b("      <th>");_.b("\n" + i);_.b("        Elements");_.b("\n" + i);_.b("      </th>");_.b("\n" + i);_.b("    </tr>");_.b("\n" + i);_.b("  </thead>");_.b("\n" + i);_.b("  <tbody>");_.b("\n" + i);if(_.s(_.f("elements",c,p,1),c,p,0,435,674,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <tr>");_.b("\n" + i);_.b("      <td>");_.b("\n" + i);_.b("        <input type=\"checkbox\" class=\"set-element-selection\" title=\"Click to select element\" />");_.b("\n" + i);_.b("      </td>");_.b("\n" + i);_.b("      <td>");_.b("\n" + i);_.b("        <a class=\"view-document-link\" href=\"#");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b(_.v(_.f("context",c,p,0)));_.b("</a>");_.b("\n" + i);_.b("      </td>");_.b("\n" + i);_.b("    </tr>");_.b("\n");});c.pop();}_.b("  </tbody>");_.b("\n" + i);_.b("</table>");_.b("\n");return _.fl();;});
templates["search-field-item"] = new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<a class='search-field-item' ");_.b("\n" + i);_.b("  title='click to remove' ");_.b("\n" + i);_.b("  data-field-field='");_.b(_.v(_.f("field",c,p,0)));_.b("' ");_.b("\n" + i);_.b("  href='#'>");_.b(_.v(_.f("fieldLabel",c,p,0)));_.b("</a>");_.b("\n");return _.fl();;});
templates["worksheet"] = new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<table class=\"worksheet-table\">");_.b("\n" + i);_.b("  <colgroup span=\"1\" id=\"worksheet-check-boxes-column\">");_.b("\n" + i);_.b("  <colgroup span=\"1\" id=\"worksheet-keys-column\">");_.b("\n" + i);if(_.s(_.f("fieldsets",c,p,1),c,p,0,153,647,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("    <colgroup span=\"1\"");_.b("\n" + i);_.b("      id=\"worksheet-handle-column-");_.b(_.v(_.f("_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      class=\"");if(_.s(_.f("multiple",c,p,1),c,p,0,248,256,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("multiple");});c.pop();}_.b(" ");if(_.s(_.f("collapse",c,p,1),c,p,0,283,291,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("collapse");});c.pop();}_.b("\">");_.b("\n" + i);_.b("    <colgroup ");_.b("\n" + i);_.b("      id=\"worksheet-column-group-");_.b(_.v(_.f("_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("      class=\"");if(_.s(_.f("multiple",c,p,1),c,p,0,392,400,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("multiple");});c.pop();}_.b(" ");if(_.s(_.f("collapse",c,p,1),c,p,0,427,435,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("collapse");});c.pop();}_.b("\">");_.b("\n" + i);if(_.s(_.f("fields",c,p,1),c,p,0,468,617,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        <col");_.b("\n" + i);_.b("          id=\"worksheet-column-");_.b(_.v(_.f("_id",c,p,0)));_.b("\"");_.b("\n" + i);_.b("          data-field-fieldset=\"");_.b(_.v(_.f("fieldset",c,p,0)));_.b("\"");_.b("\n" + i);_.b("          data-field-field=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\">");_.b("\n");});c.pop();}_.b("    </colgroup>");_.b("\n");});c.pop();}_.b("  <thead>");_.b("\n" + i);_.b("    <tr>");_.b("\n" + i);_.b("      <td>");_.b("\n" + i);_.b("        <input ");_.b("\n" + i);_.b("          id=\"select-all-worksheet-rows\"");_.b("\n" + i);_.b("          type=\"checkbox\"");_.b("\n" + i);_.b("          title=\"Click to select all rows\">");_.b("\n" + i);_.b("      </td>");_.b("\n" + i);_.b("      <th>Key</th>");_.b("\n" + i);if(_.s(_.f("fieldsets",c,p,1),c,p,0,870,1231,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("        <th ");_.b("\n" + i);_.b("          class=\"worksheet-handle-header\"");_.b("\n" + i);_.b("          data-field-fieldset=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("          ");_.b(_.v(_.f("label",c,p,0)));_.b("\n" + i);_.b("        </th>");_.b("\n" + i);if(_.s(_.f("fields",c,p,1),c,p,0,1024,1213,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          <th");_.b("\n" + i);_.b("            class=\"worksheet-header\"");_.b("\n" + i);_.b("            data-field-fieldset=\"");_.b(_.v(_.f("fieldset",c,p,0)));_.b("\"");_.b("\n" + i);_.b("            data-field-field=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("            ");_.b(_.v(_.f("_id",c,p,0)));_.b("\n" + i);_.b("          </th>");_.b("\n");});c.pop();}});c.pop();}_.b("    </tr>");_.b("\n" + i);_.b("  </thead>");_.b("\n" + i);_.b("  <tbody>");_.b("\n" + i);_.b("    <%#rows%>");_.b("\n" + i);_.b("      <tr id=\"worksheet-row-<% _id %>\">");_.b("\n" + i);_.b("        <td>");_.b("\n" + i);_.b("          <input ");_.b("\n" + i);_.b("            class=\"select-worksheet-row\"");_.b("\n" + i);_.b("            type=\"checkbox\"");_.b("\n" + i);_.b("            title=\"Click to select rows\">");_.b("\n" + i);_.b("        </td>");_.b("\n" + i);_.b("        <td>");_.b("\n" + i);_.b("          <%#head%>");_.b("\n" + i);_.b("            <span class=\"head-id\"><%.%></span>");_.b("\n" + i);_.b("          <%/head%>");_.b("\n" + i);_.b("        </td>");_.b("\n" + i);if(_.s(_.f("fieldsets",c,p,1),c,p,0,1623,2375,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("          <td class=\"empty-handle\" data-field-fieldset=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\">...</td>");_.b("\n" + i);if(_.s(_.f("fields",c,p,1),c,p,0,1721,2355,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("            <td");_.b("\n" + i);_.b("              data-field-fieldset=\"");_.b(_.v(_.f("fieldset",c,p,0)));_.b("\"");_.b("\n" + i);_.b("              data-field-field=\"");_.b(_.v(_.f("_id",c,p,0)));_.b("\">");_.b("\n" + i);_.b("              <%#");_.b(_.v(_.f("_id",c,p,0)));_.b("%>");_.b("\n" + i);_.b("                <%#multiple%>");_.b("\n" + i);_.b("                <ol>");_.b("\n" + i);_.b("                  <%#items%>");_.b("\n" + i);_.b("                    <li");_.b("\n" + i);_.b("                      data-field-fieldset_instance=\"<% fieldset_instance %>\"");_.b("\n" + i);_.b("                      data-field-field_instance=\"<% field_instance %>\"><% value %></li>");_.b("\n" + i);_.b("                  <%/items%>");_.b("\n" + i);_.b("                </ol>");_.b("\n" + i);_.b("                <%/multiple%>");_.b("\n" + i);_.b("                <%#single%>");_.b("\n" + i);_.b("                  <% value %>");_.b("\n" + i);_.b("                <%/single%>");_.b("\n" + i);_.b("              <%/");_.b(_.v(_.f("_id",c,p,0)));_.b("%>");_.b("\n" + i);_.b("            </td>");_.b("\n");});c.pop();}});c.pop();}_.b("      </tr>");_.b("\n" + i);_.b("    <%/rows%>");_.b("\n" + i);_.b("  </tbody>");_.b("\n" + i);_.b("</table>");_.b("\n");return _.fl();;});
templates["index-listing"] = new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<ul>");_.b("\n" + i);if(_.s(_.f("rows",c,p,1),c,p,0,16,122,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("  <li><a href=\"#\" data-index-id=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b(_.v(_.d("key.1",c,p,0)));_.b("<span class=\"quiet\">(");_.b(_.v(_.d("key.0",c,p,0)));_.b(")</span></a></li>");_.b("\n");});c.pop();}_.b("</ul>");_.b("\n");return _.fl();;});
templates["index-options"] = new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<option></option>");_.b("\n" + i);if(_.s(_.f("rows",c,p,1),c,p,0,27,74,"{{ }}")){_.rs(c,p,function(c,p,_){_.b("<option value=\"");_.b(_.v(_.f("id",c,p,0)));_.b("\">");_.b(_.v(_.d("key.1",c,p,0)));_.b("</option>");_.b("\n");});c.pop();}return _.fl();;});
