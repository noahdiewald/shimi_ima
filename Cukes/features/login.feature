# Note, this requires that the database is set up correctly to
# authenticate the tester user.
Feature: Loging In

Scenario Outline: Attempting to access the application.
  Given I am <userName> with <password>
  When I request the projects page
  Then I am provided the <response> code

  Examples:
  | userName | password | response |
  | badUser  | badPass  |      401 |
  | tester   | tester   |      200 |

