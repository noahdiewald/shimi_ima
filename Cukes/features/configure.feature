@webbrowser
Feature: Configuring Project

Scenario: Creating a document type
  Given a project is ready
  When I click the project Configure button
  And I click the Add Document Type button
  And I fill in Popsicle in the name editor input
  And I click the editor create button
  Then there is a Popsicle document type

Scenario: Deleting a document type
  Given a doctype exists
  And I am on the configuration page
  When I open the Popsicle document type in the editor
  And I click the editor delete button
  Then the document type Popsicle has been deleted
  And the editor area is blank

Scenario: Updating a document type
  Given a doctype exists
  And I am on the configuration page
  When I open the Popsicle document type in the editor
  And I fill in SampleCorn in the name editor input
  And I click the editor save button
  Then there is a SampleCorn document type

Scenario Outline: Creating fieldsets
  Given for <run> a doctype exists
  And I am on the configuration page
  And the Popsicle document type is in the editor
  When I click the top level element fieldsets
  And I click the editor AddChildObject button
  And I click the editor save button
  And I show the top level element fieldsets
  And I show the last fieldset
  And I give <name> as the fieldset name value
  And I give <label> as the fieldset label value
  And I give <collapse> as the fieldset collapse value
  And I give <multiple> as the fieldset multiple value
  And I give <order> as the fieldset order value
  And I click the editor save button
  Then the <name> fieldset exists
  And the fieldset has an id

  Examples:
  | name   | label  | collapse | multiple | order | run |
  | movies | Movies | true     | true     |   100 |   1 |
  | basic  | Basic  | false    | false    |    10 |   2 |
  | lie    | Lie    | true     | false    |    50 |   3 |
  | oops   | Delete | true     | true     |   666 |   4 |

Scenario: Deleting a fieldset
  Given a doctype with fieldsets exists
  And I am on the configuration page
  And the Popsicle fieldset named oops is selected
  When I click the editor RemoveElement button
  And I click the editor save button
  Then the oops fieldset is deleted

Scenario Outline: Creating fields
  Given for <order> a doctype with fieldsets exists
  And I am on the configuration page
  And the Popsicle fieldset named <fieldset> is selected
  When I click the fieldset <fieldset> element fields
  And I click the editor AddChildObject button
  And I click the editor save button
  And I show the fieldset <fieldset> last field
  And I give <name> for <fieldset> field name value
  And I give <label> for <fieldset> field label value
  And I give <type> for <fieldset> field subcategory value
  And I give <head> for <fieldset> field head value
  And I give <reversal> for <fieldset> field reversal value
  And I give <order> for <fieldset> field order value
  And I click the editor save button
  Then the <fieldset> <name> field exists
  And the <fieldset> field has an id

  Examples:
  | fieldset | name        | label       | type     | head  | reversal | order |
  | movies   | moviename   | Name        | text     | false | false    |     1 |
  | movies   | date        | Date        | date     | false | false    |    27 |
  | movies   | stars       | Rating      | integer  | false | false    |    45 |
  | movies   | description | Description | textarea | false | false    |    99 |
  | basic    | flavor      | Flavor      | text     | true  | false    |     5 |
  | basic    | sticks      | Double?     | boolean  | false | false    |    34 |
  | lie      | liefield    | Lie         | text     | false | true     |    78 |

Scenario: Nameless document type
  Given a project exists
  And I am on the configuration page
  When I click the Add Document Type button
  And I fill in a19e3acc63248869400eb8b7632c0144 in the _id editor input
  And I click the editor create button
  Then there is a a19e3acc63248869400eb8b7632c0144 document type

Scenario: Editor clears after document type creation
  Given a project exists
  And I am on the configuration page
  When I click the Add Document Type button
  And I fill in Dummy in the name editor input
  And I click the editor create button
  Then the editor area is blank

Scenario: Editor provides suitable error when updating non-existing
  Given a project exists
  And I am on the configuration page
  When I click the Add Document Type button
  And I fill in NotReady in the name editor input
  And I click the editor save button
  Then there is an error "Likely attempt to update before properly creating."

Scenario: Editor provides suitable error when deleting non-existing
  Given a project exists
  And I am on the configuration page
  When I click the Add Document Type button
  And I fill in NotReady in the name editor input
  And I click the editor delete button
  Then there is an error "The document was not found on the server."
