@webbrowser
Feature: Manipulating Projects

Scenario: Creating a project.
  Given the test database does not exist
  And I navigate to the projects page
  When I click the New Projects button
  And I fill in the project name
  And I click the "Add project" button
  Then there is a new project

Scenario: Deleting a project
  Given the test database exists
  And I navigate to the projects page
  When I click the delete button
  Then the target project was deleted

Scenario: Validation Error
  Given the test database exists
  And I navigate to the projects page
  When I click the New Projects button
  And I click the "Add project" button
  Then the validation text will warn me of invalid project name input
  When I click the "Cancel" button
  And I click the New Projects button
  Then the validation text for project name is gone
