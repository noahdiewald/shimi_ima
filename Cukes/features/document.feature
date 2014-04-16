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
  And I click the "Create as New" link
  Then a document with head "horn" will exist in the index
  And a document with Basic Flavor "horn" will be displayed
  And in the editor Basic Flavor will be blank
  And in the editor Lie Lie will be blank
  And in the editor Movies will have no children

Scenario: Selecting and viewing a document
  Given a doctype with fields exists
  And I am at the document page

Scenario: Updating a document
  Given a doctype with fields exists
  And I am at the document page

Scenario: Deleting a document
  Given a doctype with fields exists
  And I am at the document page
