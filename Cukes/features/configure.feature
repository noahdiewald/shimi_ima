@webbrowser
Feature: Configuring Project

Scenario: Creating a document type
  Given I have created a project
  When I click the project Configure button
  And click the Add Document Type button
  And I fill in Popsicle in the name editor input
  And I click the editor create button
  Then there is a Popsicle document type

Scenario: Deleting a document type
  Given I created the NoGood document type
  When I open the NoGood document type in the editor
  And I click the editor delete button
  Then the document type NoGood has been deleted

Scenario: Updating a document type
  Given I created the ExtraCorn document type
  When I open the ExtraCorn document type in the editor
  And I fill in SampleCorn in the name editor input
  And I click the editor save button
  Then there is a SampleCorn document type

Scenario Outline: Creating fieldsets
  Given the Popsicle document type is in the editor
  When I click the top level element fieldsets
  And I click the editor AddChildObject button
  And I click the editor save button
  And I double click the top level element fieldsets
  And I double click the last fieldset
  And I give <name> as the fieldset name value
  And I give <label> as the fieldset label value
  And I give <collapse> as the fieldset collapse value
  And I give <multiple> as the fieldset multiple value
  And I give <order> as the fieldset order value
  And I click the editor save button
  Then the <name> fieldset exists

  Examples:
  | name   | label  | collapse | multiple | order |
  | movies | Movies | true     | true     |   100 |
  | basic  | Basic  | false    | false    |    10 |
  | lie    | Lie    | true     | false    |    50 |
  | oops   | Delete | true     | true     |   666 |

# Scenario Outline: Creating fields
#   Given the <fieldset> fieldset is selected in the Popscicle document type
#   When I click the <fieldset> Add New Field button
#   And I give <name> as the field name
#   And I give <label> as the field label
#   And I give <type> as the field type
#   And I give <minval> as the minimum value
#   And I give <maxval> as the maximum value
#   And I give <regex> as the regex value
#   And I give <default> as the default value
#   And I give <allowed> as the allowed values
#   And I give <head> as the document head value
#   And I give <reversal> as the document reversal value
#   And I give <required> as the required field value
#   And I give <order> as the field order value
#   And I click the Field Save button
#   Then the <name> field exists

#   Examples:
#   | fieldset | name        | label             | type     | minval | maxval | regex  | default | allowed       | head  | reversal | required | order |
#   | movies   | movie_name  | Movie Name        | text     | skip   | skip   | skip   | skip    | skip          | false | false    | false    |     9 |
#   | movies   | date        | Release Date      | date     | skip   | skip   | skip   | skip    | skip          | false | false    | false    |    27 |
#   | movies   | stars       | Star Rating       | integer  | 1      | 5      | skip   | skip    | skip          | false | false    | false    |    45 |
#   | movies   | description | Movie Description | textarea | skip   | skip   | skip   | skip    | skip          | false | false    | false    |    99 |
#   | basic    | flavor      | Popscicle Flavor  | text     | skip   | skip   | berry$ | skip    | skip          | true  | false    | true     |     5 |
#   | basic    | sticks      | Double or Single? | select   | skip   | skip   | skip   | single  | single,double | false | false    | false    |    34 |
#   | lie      | lie_field   | The Lie           | text     | skip   | skip   | skip   | skip    | skip          | false | true     | false    |    78 |
#   | oops     | not_here    | Not Here          | text     | skip   | skip   | skip   | skip    | skip          | false | true     | false    |    78 |

# Scenario: Deleting a field
#   Given the oops fieldset is selected in the Popscicle document type
#   When I click the not_here field delete button
#   Then the not_here field is deleted

# Scenario: Deleting a fieldset
#   Given the oops fieldset is selected in the Popscicle document type
#   When I click the oops fieldset delete button
#   Then the oops fieldset is deleted

# Scenario Outline: Editing fields
#   Given there is no open dialog
#   Given the <fieldset> fieldset is selected in the Popscicle document type
#   When I click the <name> Edit Field button
#   And I give <name> as the field name
#   And I give <label> as the field label
#   And I give <type> as the field type
#   And I give <minval> as the minimum value
#   And I give <maxval> as the maximum value
#   And I give <regex> as the regex value
#   And I give <default> as the default value
#   And I give <allowed> as the allowed values
#   And I give <head> as the document head value
#   And I give <reversal> as the document reversal value
#   And I give <required> as the required field value
#   And I give <order> as the field order value
#   And I click the Field Save button
#   Then the <name> field has <changed_value> for the <changed_form_field> value

#   Examples:
#   | fieldset | name        | label             | type     | minval     | maxval | regex  | default | allowed | head  | reversal | required | order | changed_value | changed_form_field |
#   | movies   | movie_name  | Movie Name        | text     | skip       | skip   | skip   | skip    | skip    | false | false    | false    |     8 | 8             | order              |
#   | movies   | date        | Release Date      | date     | 1900-01-01 | skip   | skip   | skip    | skip    | false | false    | false    |    27 | 1900-01-01    | min                |
#   | movies   | stars       | Star Rating       | integer  | 1          | 10     | skip   | skip    | skip    | false | false    | false    |    45 | 10            | max                |
#   | movies   | description | Movie Description | textarea | skip       | skip   | skip   | skip    | skip    | false | true     | false    |    99 | true          | reversal           |
#   | basic    | flavor      | Flavor            | text     | skip       | skip   | berry$ | skip    | skip    | true  | false    | true     |     5 | Flavor        | label              |
#   | lie      | lie_field   | The Lie           | textarea | skip       | skip   | skip   | skip    | skip    | false | true     | false    |    78 | textarea      | subcategory        |

# Scenario Outline: Editted fields UI presents proper information
#   Given there is no open dialog
#   Given the <fieldset> fieldset is selected in the Popscicle document type
#   When I click the <name> Edit Field button
#   Then the <name> field has <current_value> for the <form_field> value in a <input> displayed

#   Examples:
#   | fieldset | name        | current_value | form_field  | input      |
#   | movies   | movie_name  | 8             | order       | text_field |
#   | movies   | date        | 1900-01-01    | min         | text_field |
#   | movies   | stars       | 10            | max         | text_field |
#   | movies   | description | true          | reversal    | checkbox   |
#   | basic    | flavor      | true          | required    | checkbox   |
#   | lie      | lie_field   | textarea      | subcategory | select     |
