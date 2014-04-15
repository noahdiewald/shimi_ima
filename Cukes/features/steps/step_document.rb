Given /^I am at the document page$/ do
  @browser.goto(@popsicleURL)
end

When /^I click the project link$/ do
  step "I click the #{@projectName} link"
end

When /^I input "(.*?)" in the (\w+) (\w+) field$/ do | data, fieldset, field |
  fid = @popsicleFields[fieldset + ':' + field]
  @browser.text_field(:id => fid).set data
end

When /^I input "(.*?)" in the (\w+) (\w+) field (\d+)$/ do | data, fieldset, field, inst |
  fid = @popsicleFields[fieldset + ':' + field]
  @browser.text_fields(:'data-field-field' => fid)[inst - 1].set data
end

Then /^I am taken to the document type listing$/ do
  @browser.div(:id => 'loading').wait_while_present
  @browser.h1.text.should match /All Document Types/
end

Then /^there is a link to the (\w+) document page$/ do | doctype |
  @browser.div(:id => 'loading').wait_while_present
  @browser.link(:text, doctype).should be_exists
end

Then /^I am taken to the (\w+) document page$/ do | doctype |
  @browser.div(:id => 'loading').wait_while_present
  @browser.h1.text.should match /#{@projectName}: #{doctype}/
end
