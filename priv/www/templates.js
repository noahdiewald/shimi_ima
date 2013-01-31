var templates = {};
templates["search-field-item"] = new Hogan.Template(function(c,p,i){var _=this;_.b(i=i||"");_.b("<a class='search-field-item' ");_.b("\n" + i);_.b("  title='click to remove' ");_.b("\n" + i);_.b("  data-field-field='");_.b(_.v(_.f("field",c,p,0)));_.b("' ");_.b("\n" + i);_.b("  href='#'>");_.b(_.v(_.f("fieldLabel",c,p,0)));_.b("</a>");_.b("\n");return _.fl();;});
