Given /^I navigate to the projects page$/ do
  @browser.goto "#{@baseURL}/projects"
end

When /^I click the New Projects button$/ do
  newProjectButton = @browser.link(:id, 'create-project')
  newProjectButton.click
end

When /^I fill in the project name$/ do
  projectNameField = @browser.text_field(:id, 'project-name')
  projectNameField.set @projectName
end

When /^I fail to fill in the project name$/ do
  true
end

When /^I click the add project button$/ do
  projectAddButton = @browser.span(:text, 'Add project')
  projectAddButton.click
end

When /^I click the cancel button$/ do
  cancelButton = @browser.span(:text, 'Cancel')
  cancelButton.click
end
  
When /^I click the delete button$/ do
  lastProjectName = @redis.get('last_project_name')
  targ = {:text => lastProjectName}
  @browser.link(targ).wait_until_present(5)
  projectId = @browser.link(targ).href.split("/")[-2].split("-").last
  deleteButton = @browser.link(:id => projectId)
  deleteButton.click
  @browser.alert.ok
end

Then /^there is a new project$/ do
  projectName = @redis.get('current_project_name')
  @browser.link(:text => projectName).wait_until_present(5)
  true
end

Then /^the target project was deleted$/ do
  lastProjectName = @redis.get('last_project_name')
  targ = {:text => lastProjectName}
  @browser.link(targ).wait_while_present(5)
  @browser.link(:class => 'project-configure-button').wait_until_present(5)
  @browser.link(targ).should_not be_exists
end

Then /^the validation text for project name is gone$/ do
  @browser.p(:class => 'validate-tips').text.should match /All fields are required./
end

Then /^the validation text will warn me of invalid project name input$/ do
  @browser.p(:class => 'validate-tips').text.should match /Length of project name must be between 1 and 50./
end

