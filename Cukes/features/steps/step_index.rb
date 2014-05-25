Given /^I am at the indexes page$/ do
  @browser.goto(@projectIndexes)
end

When /^I enter (\w+) for the (\w+) (\w+)$/ do | val, form, name |
  @browser.text_field(:id => "#{form}-#{name}-input").set val
end

When /^I select (\w+) for the (\w+) (\w+)$/ do | val, form, name |
  @browser.select(:id => "#{form}-#{name}-input").select val
end

Then /^the (\w+) (\w+) select list is populated$/ do | form, name |
  @browser.div(:id => 'loading').wait_while_present
  @browser.select(:id => "#{form}-#{name}-input").option.should be_exists
end

Then /^the "(.*?)" dialog is visible$/ do | title_text |
  title = @browser.span(:id => 'ui-id-1')
  title.should be_visible
  title.text.should == title_text
end
