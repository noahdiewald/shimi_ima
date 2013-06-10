@firefox
Feature: Configuring Project

Scenario: Creating a document type
  Given I have created a project
  When I click the project Configure button
  And click the Add Document Type button
  And I fill in the name Popsicle in the new document type form
  And I click the document type dialog Save button
  Then there is a new Popsicle document type

Scenario: Deleting a document type
  Given I created the NoGood document type
  When I click the Delete NoGood Document Type button
  Then the document type NoGood has been deleted

Scenario Outline: Creating fieldsets
  Given the Popsicle document type is selected
  When I click the Popsicle add fieldset button
  And I give <name> as the fieldset name
  And I give <label> as the fieldset label
  And I give <collapse> as the fieldset collapse value
  And I give <multiple> as the fieldset multiple value
  And I give <order> as the fieldset order
  And I click the Fieldset Save button
  Then the <label> fieldset exists

  Examples:
  | name   | label                            | collapse | multiple | order |
  | movies | This Popscicle's Favorite Movies | true     | true     | 100   |
  | basic  | Basic Info                       | false    | false    | 10    |
  | lie    | Lie Told by Popscicle            | true     | false    | 50    |
  | oops   | Better Delete                    | true     | true     | 666   |

Scenario Outline: Creating fields
  Given the <fieldset> fieldset is selected in the Popscicle document type
  When I click the <fieldset> Add New Field button
  And I give <name> as the field name
  And I give <label> as the field label
  And I give <type> as the field type
  And I give <minval> as the minimum value
  And I give <maxval> as the maximum value
  And I give <regex> as the regex value
  And I give <default> as the default value
  And I give <allowed> as the allowed values
  And I give <head> as the document head value
  And I give <reversal> as the document reversal value
  And I give <required> as the required field value
  And I give <order> as the field order value
  And I click the Field Save button
  Then the <name> field exists

  Examples:
  | fieldset | name        | label             | type     | minval | maxval | regex  | default | allowed       | head  | reversal | required | order |
  | movies   | movie_name  | Movie Name        | text     | skip   | skip   | skip   |skip     | skip          | false | false    | false    | 9     |
  | movies   | date        | Release Date      | date     | skip   | skip   | skip   | skip    |skip           | false | false    | false    | 27    |
  | movies   | stars       | Star Rating       | integer  | 1      | 5      | skip   |skip     | skip          | false | false    | false    | 45    |
  | movies   | description | Movie Description | textarea | skip   | skip   | skip   | skip    | skip          | false | false    | false    | 99    |
  | basic    | flavor      | Popscicle Flavor  | text     | skip   | skip   | berry$ | skip    | skip          | true  | false    | true     | 5     |
  | basic    | sticks      | Double or Single? | select   | skip   | skip   | skip   | single  | single,double | false | false    | false    | 34    |
  | lie      | lie         | The Lie           | text     | skip   | skip   | skip   | skip    | skip          | false | true     | false    | 78    |
  | oops     | lie         | Not Here          | text     | skip   | skip   | skip   | skip    | skip          | false | true     | false    | 78    |
