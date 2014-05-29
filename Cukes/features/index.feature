@webbrowser
Feature: Indexing Documents

Scenario: New dialog change events
  Given a doctype with fields exists
  And I am at the indexes page
  When I click the New link
  Then the "New Index" dialog is visible
  And the index doctype select list is enabled 
  And the index fieldset select list is disabled 
  And the index field select list is disabled 
  When I enter whatever for the index name 
  When I select Popsicle for the index doctype
  Then the index doctype select list is enabled 
  And the index fieldset select list is enabled 
  And the index field select list is disabled 
  When I select Basic for the index fieldset
  Then the index doctype select list is enabled 
  And the index fieldset select list is enabled 
  And the index field select list is enabled
  When I select blank for the index doctype
  Then the index doctype select list is enabled 
  And the index fieldset select list is disabled
  And the index fieldset is blank
  And the index field select list is disabled 
  And the index field is blank

Scenario: Creating document index
  Given a doctype with fields exists
  And I am at the indexes page
  When I click the New link
  Then the "New Index" dialog is visible
  When I enter whatever for the index name 
  When I select Popsicle for the index doctype
  And I select Basic for the index fieldset
  And I select Flavor for the index field
  And I click the "Create" button
  Then whatever exists in the indexes listing
