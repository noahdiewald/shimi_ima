Given /^a project is ready$/ do
  step "the test database exists"
  step "I navigate to the projects page"
end

Given /^a project exists$/ do
  step "the test database exists"
end

Given /^a doctype exists$/ do
  step "the test database exists"
  post_fixture "Popsicle_bare.json"
  post_fixture "Popsicle_design.json"
end

Given /^a doctype with (\w+) exists$/ do | fix |
  step "the test database exists"
  post_fixture "Popsicle_#{fix}.json"
  post_fixture "Popsicle_design.json"
end

Given /^I am at the document page$/ do
  @browser.goto(@popsicleURL)
end

Given /^a doctype with two documents exists$/ do
  step "a doctype with fields exists"
  post_fixture "Popsicle_two_docs.json", true
end

Given /^the ([a-f0-9]{32}) document is in the view pane$/ do | identifier |
  step "a doctype with two documents exists"
  @browser.goto(@popsicleURL + '#' + identifier)
  @browser.a(:text => 'Edit').wait_until_present
end

Given /^the ([a-f0-9]{32}) document is in the edit pane$/ do | identifier |
  step "the #{identifier} document is in the view pane"
  step "I click the Edit link"
  @browser.a(:id => 'save-document-button').wait_until_present
end

When /^I click the (\w+) panel menu item$/ do | panel |
  @browser.li(:data_panel => "document-#{panel}").click
end
 
When /^I click the (\w+) link$/ do | text |
  step "I click the \"#{text}\" link"
end

When /^I click the "(.*?)" link$/ do | text |
  @browser.div(:id => 'loading').wait_while_present
  @browser.link(:text, text).click
end

When /^I click the "(.*?)" button$/ do | name |
  @browser.span(:text, name).click
end

Then /^the (\w+) panel is visible$/ do | panel |
  @browser.div(:id => "document-#{panel}").should be_visible
end

Then /^the loading message is displayed$/ do
  @browser.div(:id => "loading").should be_visible
end
