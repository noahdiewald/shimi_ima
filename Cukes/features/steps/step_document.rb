Given /^I am at the document page$/ do
  @browser.goto(@popsicleURL)
end

Given /^a doctype with two documents exists$/ do
  step "a doctype with fields exists"
  post_fixture "Popsicle_two_docs.json", true
end

When /^I click the test project link$/ do
  step "I click the #{@projectName} link"
end

When /^I input "(.*?)" in the (\w+) (\w+) field$/ do | data, fieldset, field |
  @browser.div(:id => 'loading').wait_while_present
  fid = @popsicleFields[fieldset + ':' + field]
  @browser.text_field(:id => fid).set data
end

When /^I input "(.*?)" in (\w+) (\w+) field (\d+)$/ do | data, fieldset, field, index |
  @browser.div(:id => 'loading').wait_while_present
  multi_field(fieldset, field, index).set data
end

When /^I focus on (\w+) (\w+) field (\d+)$/ do | fieldset, field, index |
  @browser.div(:id => 'loading').wait_while_present
  multi_field(fieldset, field, index).focus
end

When /^I click the (\w+) (\w+) expander (\d+)$/ do | fieldset, field, index |
  @browser.div(:id => 'loading').wait_while_present
  gid = multi_field(fieldset, field, index).id
  @browser.span(:data_group_id => gid).click
end

When(/^perform the key sequence (alt|control) (\w+)$/) do | mod, key |
  @browser.div(:id => 'loading').wait_while_present
  @browser.element(:css, ':focus').send_keys [mod.to_sym, key]
end

When(/^I click on the last day of the first week$/) do
  @browser.div(:id => 'loading').wait_while_present
  @browser.element(:css, '#ui-datepicker-div > .ui-datepicker-calendar > tbody > tr:first-child > td:last-child > a').click
end

When /^I click the view document link for ([a-f0-9]{32})$/ do | docid |
  @browser.div(:id => 'loading').wait_while_present
  @browser.a(:href => "##{docid}").click
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

Then /^the (\w+) (\w+) field (\d+) has the correct identifier$/ do | fieldset, field, index |
  @browser.div(:id => 'loading').wait_while_present
  fid = @popsicleFields[fieldset + ':' + field]
  multi_field(fieldset, field, index).id.should match /#{fid}-[0-9a-f]{32}/
end

Then /^the (\w+) (\w+) field (\d+) is expanded$/ do | fieldset, field, index |
  @browser.div(:id => 'loading').wait_while_present
  multi_field(fieldset, field, index).attribute_value("class").should match /expanded/
end

Then /^the (\w+) (\w+) field (\d+) is not expanded$/ do | fieldset, field, index |
  @browser.div(:id => 'loading').wait_while_present
  multi_field(fieldset, field, index).attribute_value("class").should_not match /expanded/
end

Then /^there is a date in the (\w+) (\w+) field (\d+)$/ do | fieldset, field, index |
  @browser.div(:id => 'loading').wait_while_present
  multi_field(fieldset, field, index).value.should match /^\d{4}-\d{2}-\d{2}/
end

Then /^the date picker is present$/ do
  @browser.div(:id => 'loading').wait_while_present
  @browser.div(:id, 'ui-datepicker-div').should be_present
end

Then /^document ([a-f0-9]{32}) is displayed$/ do | docid |
  @browser.div(:id => 'loading').wait_while_present
  @browser.div(:id => 'document-view-info').attribute_value('data-document-document').should == docid
end

Then /^has value '(.*?)' for field id ([a-f0-9]{32})$/ do | value, fieldid |
  @browser.div(:id => 'loading').wait_while_present
  @browser.element(:css, "li.field-view[data-field-field='#{fieldid}']").attribute_value('data-field-value').should == value
end

Then /^the data is loaded in the index pane$/ do
  @browser.div(:id => 'loading').wait_while_present
  @browser.div(:id => 'index-listing').should be_exists
end

Then /^document ([a-f0-9]{32}) is listed in the index pane$/ do | docid |
  @browser.a(:href => "##{docid}").wait_until_present
  @browser.a(:href => "##{docid}").should be_exists
end
