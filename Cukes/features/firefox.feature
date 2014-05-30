@firefox
Feature: Bugs and Behavior Specific to Firefox

# This JQuery UI element is only needed in Firefox but there is a bug
# in JQuery UI that is interacting badly with Firefox when used from
# webdriver. For this reason, these tests are failing but the calendar
# widget is working.
Scenario: Using the date picker
  Given a doctype with fields exists
  And I am at the document page
  When I click the Movies link
  And I click the Add link
  And I focus on Movies Date field 1
  Then the date picker is visible
  When I click on the last day of the first week
  Then there is a date in the Movies Date field 1
