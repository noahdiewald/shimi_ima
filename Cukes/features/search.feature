@webbrowser
Feature: Searching Documents

Scenario: Simple general search
  Given a doctype with two documents exists
  And I am at the document page
  When I click the search panel menu item
  Then the search panel is visible
  When I enter "burn" as the search term
  And I execute the search
  Then the loading message is displayed
  And there are 1 results for Basic Flavor
  When I click the search header for Basic Flavor
  Then the 31bb7974cd97a09997da637e4445a142 document is listed under Basic Flavor
