Given(/^I have created a project$/) do
  step "I navigate to the projects page"
  step "I click the New Projects button"
  step "I fill in the project name"
  step "I click the add project button"
  projectName = @redis.get('current_project_name')
  targ = {:text => projectName}
  @browser.link(targ).wait_until_present(5)
  projectId = @browser.link(targ).href.split("/")[-3].split("-").last
  @redis.set('current_project_id', projectId)
end

Given(/^I created the (\w+) document type$/) do | name |
  projectId = @redis.get('current_project_id')
  @browser.goto("#{@baseURL}/projects/project-#{projectId}/config")
  step("click the Add Document Type button")
  step("I fill in #{name} in the name editor input")
  step("I click the editor create button")
  @browser.div(:id => 'loading').wait_while_present
end

Given(/^the (\w+) document type is in the editor$/) do | name |
  projectId = @redis.get('current_project_id')
  @browser.goto("#{@baseURL}/projects/project-#{projectId}/config")
  step("I open the #{name} document type in the editor")
end

When(/^I click the project Configure button$/) do
  projectId = @redis.get('current_project_id')
  configureButton = @browser.link(:href => "/projects/project-#{projectId}/config")
  configureButton.click
end

When(/^click the Add Document Type button$/) do
  button = @browser.link(:id => 'doctypes-add-button')
  button.wait_until_present
  button.click
end

When(/^I click the top level element (\w+)$/) do | title |
  @browser.div(:id => 'loading').wait_while_present
  @browser.element(:css, "#edit-form > form > ul > li > span[title=#{title}]").click
end

When(/^I double click the top level element (\w+)$/) do | title |
  @browser.div(:id => 'loading').wait_while_present
  @browser.element(:css, "#edit-form > form > ul > li > span[title=#{title}]").double_click
end

When(/^I double click the last fieldset$/) do
  @browser.div(:id => 'loading').wait_while_present
  @browser.element(:css, "#edit-form > form > ul > li > span[title='fieldsets'] + ol > li:last-child").double_click
end

When(/^I fill in (\w+) in the (\w+) editor input$/) do | data, input |
  inputField = @browser.text_field(:name => input)
  inputField.set data
end

When(/^I give (.+) as the fieldset (\w+) value$/) do | data, input |
  inputField = @browser.text_field(:css, "#edit-form > form > ul > li > span[title='fieldsets'] + ol > li:last-child > ul > li > [name='#{input}']")
  inputField.set data
end

When(/^I open the (\w+) document type in the editor$/) do | name |
  @browser.div(:id => 'loading').wait_while_present
  @browser.link(:text => name).click
  @browser.div(:id => 'loading').wait_while_present
end

When(/^I click the editor (\w+) button$/) do | bttn |
  @browser.div(:id => 'loading').wait_while_present
  @browser.link(:id => "config-#{bttn.to_dash}-button").click
  if bttn == "delete"
    @browser.alert.ok
  end
  @browser.div(:id => 'loading').wait_while_present
end

Then(/^the document type (\w+) has been deleted$/) do | name |
  @browser.div(:id => 'loading').wait_while_present
  @browser.link(:text => name).should_not be_exists
end

Then(/^there is a (\w+) document type$/) do | name |
  @browser.div(:id => 'loading').wait_while_present
  @browser.link(:text => name).should be_exists
end

Then(/^the (\w+) (\w+) exists$/) do | name, elem_type |
  @browser.div(:id => 'loading').wait_while_present
  @browser.text_field(:css, "#edit-form ol[title='#{elem_type}s'] > li:last-child [name='name']").value.should == name
end
