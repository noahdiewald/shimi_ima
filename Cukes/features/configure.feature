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
  When I press the Delete Document Type button
  Then the document type NoGood has been deleted
