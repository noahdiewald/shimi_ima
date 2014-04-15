Given /^I navigate to the projects page$/ do
  @browser.goto "#{@baseURL}/projects"
end

Given /^the test database does not exist$/ do
  delete_project
end

Given /^the test database exists$/ do
  step "the test database does not exist"
  create_project
end

# TODO: combine with link clicking step in shared
When /^I click the New Projects button$/ do
  @browser.link(:id, 'create-project').click
end

When /^I fill in the project name$/ do
  @browser.text_field(:id, 'project-name').set @projectName
end

When /^I click the add project button$/ do
  @browser.span(:text, 'Add project').click
end

When /^I click the cancel button$/ do
  @browser.span(:text, 'Cancel').click
end
  
When /^I click the delete button$/ do
  @browser.div(:id => 'loading').wait_while_present
  @browser.link(:id => @projectId).click
  @browser.alert.ok
end

Then /^there is a new project$/ do
  @browser.div(:id => 'loading').wait_while_present
  @browser.link(:text, @projectName).should be_exists
end

Then /^the target project was deleted$/ do
  @browser.div(:id => 'loading').wait_while_present
  @browser.link(:text, @projectName).should_not be_exists
end

Then /^the validation text for project name is gone$/ do
  @browser.p(:class => 'validate-tips').text.should match /All fields are required./
end

Then /^the validation text will warn me of invalid project name input$/ do
  @browser.span(:class => 'validation-error-message').text.should match /Length of project name must be between 1 and 50./
end

