@webbrowser
Feature: Searching Documents

Scenario: Simple general search
  Given a doctype with two documents exists
  And I am at the document page
  When I click the search pane menu item
  Then the search panel is visible
  When I enter "burn" as the search term
  Then the loading message is displayed
  And the 31bb7974cd97a09997da637e4445a142 is listed under Basic Flavor
