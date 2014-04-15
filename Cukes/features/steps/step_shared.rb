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

When /^I click the (\w+) link$/ do | text |
  step "I click the \"#{text}\" link"
end

When /^I click the "(.*?)" link$/ do | text |
  @browser.div(:id => 'loading').wait_while_present
  @browser.link(:text, text).click
end
