Given /^I am at the document page$/ do
  @browser.goto(@popsicleURL)
end

When /^I click the test project link$/ do
  step "I click the #{@projectName} link"
end

When /^I input "(.*?)" in the (\w+) (\w+) field$/ do | data, fieldset, field |
  @browser.div(:id => 'loading').wait_while_present
  fid = @popsicleFields[fieldset + ':' + field]
  @browser.text_field(:id => fid).set data
end

When /^I input "(.*?)" in (\w+) (\w+) field (\d+)$/ do | data, fieldset, field, inst |
  @browser.div(:id => 'loading').wait_while_present
  fid = @popsicleFields[fieldset + ':' + field]
  @browser.text_fields(:data_field_field => fid)[inst.to_i - 1].set data
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

Then /^a document with head "(.*?)" will exist in the index$/ do | head |
  @browser.link(:class => 'view-document-link').wait_until_present
  @browser.link(:class => 'view-document-link').text.should match /#{head}/
end

Then /^a document with (\w+) (\w+) "(.*?)" will be displayed$/ do | fieldset, field, value |
  @browser.div(:id => 'loading').wait_while_present
  fid = @popsicleFields[fieldset + ':' + field]
   @browser.li(:class => 'field-view', :data_field_value => value.to_json, :data_field_field => fid).should be_exists
end

Then /^in the editor (\w+) (\w+) will be blank$/ do | fieldset, field |
  @browser.div(:id => 'loading').wait_while_present
  fid = @popsicleFields[fieldset + ':' + field]
  @browser.text_field(:id => fid).value.should be_empty
end

Then /^in the editor (\w+) will have no children$/ do | fieldset |
  @browser.div(:id => 'loading').wait_while_present
  fsid = @popsicleFieldsets[fieldset]
  @browser.div(:id => "container-#{fsid}").elements.to_a.should be_empty
end

Then /^the (\w+) (\w+) field (\d+) has the correct identifier$/ do | fieldset, field, inst |
  @browser.div(:id => 'loading').wait_while_present
  fid = @popsicleFields[fieldset + ':' + field]
  @browser.text_fields(:data_field_field => fid)[inst.to_i - 1].id.should match /#{fid}-[0-9a-f]{32}/
end
