Given(/^I have created a project$/) do
  step "I navigate to the projects page"
  step "I click the New Projects button"
  step "I fill in the project name"
  step "I click the add project button"
  projectName = @redis.get('current_project_name')
  targ = {:text => projectName}
  @browser.link(targ).wait_until_present(5)
  projectId = @browser.link(targ).href.split("/")[-2].split("-").last
  @redis.set('current_project_id', projectId)
end

Given(/^I created the (\w+) document type$/) do | name |
  pending # express the regexp above with the code you wish you had
end

When(/^I click the project Configure button$/) do
  projectId = @redis.get('current_project_id')
  configureButton = @browser.link(:href => "/projects/project-#{projectId}/config")
  configureButton.click
end

When(/^click the Add Document Type button$/) do
  @browser.div(:id => 'loading', :visible => true).wait_while_present
  @browser.link(:id => 'doctype-add-button').click
end

When(/^I fill in the name (\w+) in the new document type form$/) do | name |
  @browser.div(:id => 'loading', :visible => true).wait_while_present
  doctypeNameField = @browser.text_field(:id => 'doctype-doctype-input', :visible => true)
  doctypeNameField.set name
end

When(/^I click the document type dialog Save button$/) do
  @browser.button(:xpath => '/html/body/div[6]/div[11]/div/button[1]').click
end

When(/^I press the Delete Document Type button$/) do
  pending # express the regexp above with the code you wish you had
end

Then(/^the document type (\w+) has been deleted$/) do | name |
  pending # express the regexp above with the code you wish you had
end

Then(/^there is a new (\w+) document type$/) do | name |
  @browser.link(:href => "config/doctypes/#name").wait_until_present(5)
  true
end
