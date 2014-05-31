Given(/^I created the (\w+) document type$/) do | name |
  step "the test database exists"
  step "I click the Add Document Type button"
  step "I fill in #{name} in the name editor input"
  step "I click the editor create button"
end

Given /^I am on the configuration page$/ do
  @browser.goto(@projectConfig)
end

# This is only supposed to happen the first time
Given /^for (\d+) a doctype exists$/ do | r |
  step "a doctype exists" if r == "1"
end

# This is only supposed to happen the first time
Given /^for (\d+) a doctype with (\w+) exists$/ do | r, fix |
  step "a doctype with #{fix} exists" if r == "1"
end

Given /^the (\w+) document type is in the editor$/ do | name |
  @browser.goto(@projectConfig)
  step "I open the #{name} document type in the editor"
end

Given /^the (\w+) fieldset named (\w+) is selected$/ do | doctype, name |
  step "the #{doctype} document type is in the editor"
  step "I show the fieldset named #{name}"
end

# TODO: combine with link clicking step in shared
When(/^I click the project Configure button$/) do
  @browser.div(:id => 'loading').wait_while_present
  @browser.link(:href => "/projects/project-#{@projectId}/config").click
end

# TODO: combine with link clicking step in shared
When(/^I click the Add Document Type button$/) do
  button = @browser.link(:id => 'doctypes-add-button')
  button.wait_until_present
  button.click
end

When(/^I click the top level element (\w+)$/) do | title |
  @browser.div(:id => 'loading').wait_while_present
  @browser.element(:css, "#edit-form > form > ul > li > span[title=#{title}]").click
end

When(/^I click the fieldset (\w+) element (\w+)$/) do | name, title |
  @browser.div(:id => 'loading').wait_while_present
  step("I show the fieldset named #{name}")
  @browser.element(:css, "#edit-form > form > ul > li > ol[title='fieldsets'] > li > ul > li > [value='#{name}']").parent.parent.element(:css, "span[title='#{title}']").click
end

When(/^I show the top level element (\w+)$/) do | title |
  @browser.div(:id => 'loading').wait_while_present
  target = @browser.element(:css, "#edit-form > form > ul > li > span[title=#{title}]")
  if @browser.element(:css, "li##{target.parent.id.escape_first_digit} > .hidden").exists?
    target.double_click
  end
end

When(/^I show the fieldset (\w+) element (\w+)$/) do | name, title |
  @browser.div(:id => 'loading').wait_while_present
  step("I show the fieldset named #{name}")
  target = @browser.element(:css, "#edit-form > form > ul > li > ol[title='fieldsets'] > li > ul > li > [value='#{name}']").parent.parent.element(:css, "span[title=#{title}]")
  if @browser.element(:css, "li##{target.parent.id.escape_first_digit} > .hidden").exists?
    target.double_click
  end
end

When(/^I show the last fieldset$/) do
  @browser.div(:id => 'loading').wait_while_present
  target = @browser.element(:css, "#edit-form > form > ul > li > span[title='fieldsets'] + ol > li:last-child")
  if @browser.element(:css, "li##{target.id.escape_first_digit} > .hidden").exists?
    target.double_click
  end
end

When(/^I show the fieldset (\w+) last field$/) do | name |
  @browser.div(:id => 'loading').wait_while_present
  step("I show the fieldset #{name} element fields")
  target = @browser.element(:css, "#edit-form > form > ul > li > ol[title='fieldsets'] > li > ul > li > [value='#{name}']").parent.parent.element(:css, "ol[title='fields'] > li:last-child")
  if @browser.element(:css, "li##{target.id.escape_first_digit} > .hidden").exists?
    target.double_click
  end
end

When(/^I show the fieldset named (\w+)$/) do | name |
  step("I show the top level element fieldsets")
  target = @browser.element(:css, "#edit-form > form > ul > li > ol[title='fieldsets'] > li > ul > li > [value='#{name}']").parent.parent.parent
  if @browser.element(:css, "li##{target.id.escape_first_digit} > .hidden").exists?
    target.double_click
  end
end

When(/^I fill in (\w+) in the (\w+) editor input$/) do | data, input |
  inputField = @browser.text_field(:name => input)
  inputField.wait_until_present
  inputField.set data
end

When(/^I give (.+) as the fieldset (\w+) value$/) do | data, input |
  inputField = @browser.text_field(:css, "#edit-form > form > ul > li > span[title='fieldsets'] + ol > li:last-child > ul > li > [name='#{input}']")
  inputField.set data
end

When(/^I give (.+) for (\w+) field (\w+) value$/) do | data, name, input |
  inputField = @browser.element(:css, "#edit-form > form > ul > li > ol[title='fieldsets'] > li > ul > li > [value='#{name}']").parent.parent.text_field(:css, "ol[title='fields'] > li:last-child > ul > li > [name='#{input}']")
  inputField.set data
end

When(/^I open the (\w+) document type in the editor$/) do | name |
  @browser.div(:id => 'doctypes-listing').wait_until_present
  @browser.div(:id => 'loading').wait_while_present
  # TODO: combine with link clicking step in shared
  @browser.link(:text => name).click
  @browser.div(:id => 'loading').wait_while_present
end

When(/^I click the editor (\w+) button$/) do | bttn |
  @browser.div(:id => 'loading').wait_while_present
  @browser.link(:id => "config-#{bttn.to_dash}-button").click
  if bttn == "delete"
    sleep 0.25
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

Then(/^the (\w+) fieldset exists$/) do | name |
  @browser.div(:id => 'loading').wait_while_present
  @browser.text_field(:css, "#edit-form ol[title='fieldsets'] > li:last-child [name='name']").value.should == name
end

Then(/^the fieldset has an id$/) do
  @browser.div(:id => 'loading').wait_while_present
  @browser.text_field(:css, "#edit-form ol[title='fieldsets'] > li:last-child [name='_id']").value.should =~ /^[a-z0-9]{32}$/
end

Then(/^the (\w+) (\w+) field exists$/) do | fieldset, name |
  @browser.div(:id => 'loading').wait_while_present
  @browser.element(:css, "#edit-form [value='#{fieldset}']").parent.parent.text_field(:css, "ol[title='fields'] > li:last-child [name='name']").value.should == name
end

Then(/^the (\w+) field has an id$/) do | fieldset |
  @browser.div(:id => 'loading').wait_while_present
  @browser.element(:css, "#edit-form [value='#{fieldset}']").parent.parent.text_field(:css, "ol[title='fields'] > li:last-child [name='_id']").value.should =~ /^[a-z0-9]{32}$/
end

Then(/^the (\w+) fieldset is deleted$/) do | name |
  @browser.div(:id => 'loading').wait_while_present
  @browser.text_field(:css, "#edit-form ol[title='fieldsets'] > li:last-child [value='#{name}']").should_not be_exists
end

Then(/^the editor area is blank$/) do
  @browser.div(:id => 'loading').wait_while_present
  @browser.div(:id => 'edit-form').form.ul.text.should == ''
end

Then(/^there is an error "(.*?)"$/) do | msg |
  @browser.div(:id => 'loading').wait_while_present
  @browser.div(:class => 'ui-state-error').span(:class => 'notification-message').text.should =~ /#{msg}/
end
