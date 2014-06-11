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

Scenario: Narrow results to single field
  Given I have searched for "burn"
  When I double click the search result header for Basic Flavor
  Then the search form will be restricted to only Basic Flavor
  When I enter "n" as the search term
  And I execute the search
  Then there are 2 results for Basic Flavor
  And there are search results for 1 field

Scenario: Narrow results to two fields
  Given I have searched for "n"
  When I double click the search result header for Basic Flavor
  And I double click the search result header for Movies Name
  Then the search form will be restricted to Basic Flavor
  And the search form will be restricted to Movies Name
  And I enter "n" as the search term
  And I execute the search
  Then there are 2 results for Basic Flavor
  And there are 2 results for Movies Name
  And there are search results for 2 fields

Scenario: Broaden the results
  Given I have searched for "n"
  When I double click the search result header for Basic Flavor
  And I double click the search result header for Movies Name
  Then the search form will be restricted to Basic Flavor
  And the search form will be restricted to Movies Name
  When I click the Movies Name search field item
  Then the search form will be restricted to only Basic Flavor
  And I enter "n" as the search term
  And I execute the search
  Then there are 2 results for Basic Flavor
  And there are search results for 1 field

Scenario: Reset to search all fields
  Given I have searched for "n"
  When I double click the search result header for Basic Flavor
  Then the search form will be restricted to Basic Flavor
  When I click the "Search All Fields" link
  Then the search form will be unrestricted
  When I enter "n" as the search term
  And I execute the search
  Then there are search results for 4 fields

Scenario: Exclude a field
  Given I have searched for "burn"
  When I double click the search result header for Basic Flavor
  And I click the document search exclude checkbox
  And I enter "n" as the search term
  And I execute the search
  Then there are 2 results for Movies Name
  And there are 2 results for Lie Lie
  And there are 2 results for Movies Description
  And there are search results for 3 fields

Scenario: Exclude results from two fields
  Given I have searched for "n"
  When I double click the search result header for Basic Flavor
  And I double click the search result header for Movies Name
  And I click the document search exclude checkbox
  And I enter "n" as the search term
  And I execute the search
  Then there are 2 results for Lie Lie
  And there are 2 results for Movies Description
  And there are search results for 2 fields
