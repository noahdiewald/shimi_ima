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
  Then the data is loaded in the index pane
  And document 31bb7974cd97a09997da637e4445a142 is listed in the index pane
  When I click the view document link for 31bb7974cd97a09997da637e4445a142
  Then document 31bb7974cd97a09997da637e4445a142 is displayed
  And has value '"burn berry"' for field id 464aea16f338422cb5b1dab79d80c34a

Scenario: Updating a document
  Given the 31bb7974cd97a09997da637e4445a142 document is in the edit pane
  When I click save
  Then the updated date is not blank

Scenario: Deleting a document
  Given the 31bb7974cd97a09997da637e4445a142 document is in the view pane
  When I click the Delete link
  And I click ok
  Then document 31bb7974cd97a09997da637e4445a142 is not listed in the index pane
  And the Restore link is visible

Scenario: Restoring a document immmediately after deleting
  Given the 31bb7974cd97a09997da637e4445a142 document is in the view pane
  When I click the Delete link
  And I click ok
  Then document 31bb7974cd97a09997da637e4445a142 is not listed in the index pane
  When I click the Restore link
  And I click ok
  Then document 31bb7974cd97a09997da637e4445a142 is listed in the index pane
  And the Delete link is visible

Scenario: Restoring a previously deleted document
  Given the deleted document is in the view pane
  When I click the Restore link
  And I click ok
  Then document 0e834d0dd8b47706bd4aa0d74ce3f8ab is listed in the index pane

Scenario: Expanding and contracting text boxes
  Given the 31bb7974cd97a09997da637e4445a142 document is in the edit pane
  When I click the Movies Description expander 1
  Then the Movies Description field 1 is expanded
  When I click the Movies Description expander 1
  Then the Movies Description field 1 is not expanded
  When I focus on Movies Description field 1
  And perform the key sequence alt x
  Then the Movies Description field 1 is expanded
  When I focus on Movies Description field 1
  And perform the key sequence alt x
  Then the Movies Description field 1 is not expanded

Scenario: Expanding and contracting text boxes for newly added fieldsets
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

Scenario: Multiple fieldset field identifiers
  Given the 0e834d0dd8b47706bd4aa0d74ce3f8ab document is in the view pane
  When I click the Edit link
  Then the input with id 13fa990addce00feacc1dfb623315490-89c94bf96b79418295b2cf05c9b97688 has the value "Quadrillion Dollar Baby"
  And the input with id 13fa990addce00feacc1dfb623315490-835af33919734fb0ab5af5abc681186e has the value "What the film is this?"

Scenario: Help dialogs
  Given the 31bb7974cd97a09997da637e4445a142 document is in the edit pane
  When I click the first help icon
  Then the help dialog text is "Just enter a name!!!!"

Scenario: Updating a document twice in a row
  Given the 31bb7974cd97a09997da637e4445a142 document is in the edit pane
  When I click save
  Then the updated date is not blank
  When I click save a second time
  Then the updated date is different
