Given /^the (\w+) (\w+) index exists$/ do | x, y |
  step "a doctype with fields exists"
  post_fixture("#{x}_#{y}.json")
  post_fixture("#{x}_#{y}_design.json")
end

Given /^I am at the indexes page$/ do
  @browser.goto(@projectIndexes)
end

When /^I enter (\w+) for the (\w+) (\w+)$/ do | val, form, name |
  @browser.text_field(:id => "#{form}-#{name}-input").set val
end

When /^I select (\w+) for the (\w+) (\w+)$/ do | val, form, name |
  val = '' if val == 'blank'
  @browser.select(:id => "#{form}-#{name}-input").select val
end

When /^I select (\w+) from the indexes listing$/ do | name |
  @browser.div(:id => 'loading').wait_while_present
  @browser.a(:text => name).click
end

Then /^the input (\w+) (\w+) is blank$/ do | form, name |
  @browser.select(:id => "#{form}-#{name}-input").value == ''
end

Then /^the "(.*?)" dialog is visible$/ do | title_text |
  title = @browser.span(:id => 'ui-id-1')
  title.should be_visible
  title.text.should == title_text
end

Then /^(\w+) exists in the indexes listing$/ do | name |
  @browser.div(:id => 'loading').wait_while_present
  @browser.a(:text => name).should be_exists
end

Then /^the (\w+) (\w+) select list is (enabled|disabled)$/ do | form, name, state |
  selection_state = @browser.select(:id => "#{form}-#{name}-input").attribute_value('disabled')
  if state == 'enabled'
    selection_state.should be_nil
  else
    selection_state.should == 'true'
  end
end

Then /^the text "(.*?)" will be displayed$/ do | text |
  @browser.div(:id => 'loading').wait_while_present
  @browser.div(:id => 'index-editing-data').p.text.should =~ /#{text}/
end
