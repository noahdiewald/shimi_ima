@webbrowser
Feature: Manipulating Projects

Scenario: Creating a project.
  Given I navigate to the projects page
  When I click the New Projects button
  And I fill in the project name
  And I click the add project button
  Then there is a new project

Scenario: Deleting a project
  Given I navigate to the projects page
  When I click the delete button
  Then the target project was deleted

Scenario: Validation Error
  Given I navigate to the projects page
  When I click the New Projects button
  And I fail to fill in the project name
  And I click the add project button
  Then the validation text will warn me of invalid project name input
  When I click the cancel button
  And I click the New Projects button
  Then the validation text for project name is gone
