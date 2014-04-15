@webbrowser
Feature: Editing Documents

Scenario: Choosing a document type
  Given a doctype with fields exists
  And I navigate to the projects page
  When I click the project link
  Then I am taken to the document type listing
  And there is a link to the Popsicle document page
  When I click the Popsicle link
  Then I am taken to the Popsicle document page
