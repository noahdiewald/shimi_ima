@webbrowser
Feature: Indexing Documents

Scenario: Creating document index
  Given a doctype with fields exists
  And I am at the indexes page
  When I click the New link
  Then the "New Index" dialog is visible
  When I enter whatever for the index name
  And I select Popsicle for the index doctype
  Then the index fieldset select list is populated
  When I select Basic for the index fieldset
  Then the index field select list is populated
  When I select Flavor for the index field
  And click the Create button
  Then whatever exists in the indexes listing
