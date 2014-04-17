@webbrowser
Feature: Editing Documents

Scenario: Choosing a document type
  Given a doctype with fields exists
  And I navigate to the projects page
  When I click the test project link
  Then I am taken to the document type listing
  And there is a link to the Popsicle document page
  When I click the Popsicle link
  Then I am taken to the Popsicle document page

Scenario: Creating a document
  Given a doctype with fields exists
  And I am at the document page
  When I click the Basic link
  And I input "horn" in the Basic Flavor field
  And I click the Lie link
  And I input "dog's are nice" in the Lie Lie field
  And I click the Movies link
  And I click the Add link
  And I input "harvey" in Movies Name field 1
  Then the Movies Name field 1 has the correct identifier
  When I click the "Create as New" link
  Then a document with head "horn" will exist in the index
  And a document with Basic Flavor "horn" will be displayed
  And in the editor Basic Flavor will be blank
  And in the editor Lie Lie will be blank
  And in the editor Movies will have no children

Scenario: Selecting and viewing a document
  Given a doctype with two documents exists
  And I am at the document page

Scenario: Updating a document
  Given a doctype with two documents exists
  And I am at the document page

Scenario: Deleting a document
  Given a doctype with two documents exists
  And I am at the document page

Scenario: Expanding and contracting text boxes
  Given a doctype with fields exists
  And I am at the document page
  When I click the Movies link
  And I click the Add link
  And I click the Movies Description expander 1
  Then the Movies Description field 1 is expanded
  When I click the Movies Description expander 1
  Then the Movies Description field 1 is not expanded
  When I focus on Movies Description field 1
  And perform the key sequence alt x
  Then the Movies Description field 1 is expanded
  When I focus on Movies Description field 1
  And perform the key sequence alt x
  Then the Movies Description field 1 is not expanded

Scenario: Using the date picker
  Given a doctype with fields exists
  And I am at the document page
  When I click the Movies link
  And I click the Add link
  And I focus on Movies Date field 1
  Then the date picker is present
  When I click on the last day of the first week
  Then there is a date in the Movies Date field 1
