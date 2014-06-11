Given /^the search pane is open$/ do
  step "a doctype with two documents exists"
  step "I am at the document page"
  step "I click the search panel menu item"
end

Given /^I have searched for "(.*?)"$/ do | term |
  step "the search pane is open"
  step "I enter \"#{term}\" as the search term"
  step "I execute the search"
end

When /^I click the search header for (\w+) (\w+)$/ do | fieldset, field |
  @browser.div(:id => 'loading').wait_while_present
  fid = @popsicleFields[fieldset + ':' + field]
  @browser.h5(:data_field_field => fid).click
end

When /^I enter "(.*?)" as the search term$/ do | term |
  @browser.text_field(:id => 'document-search-term').set term
end

When /^I execute the search$/ do
  @browser.input(:id => 'document-search-term').send_keys [:enter]
end

When /^I double click the search result header for (\w+) (\w+)$/ do | fieldset, field |
  @browser.div(:id => 'loading').wait_while_present
  fid = @popsicleFields[fieldset + ':' + field]
  header = @browser.h5(:data_field_field => fid)
  header.wait_until_present
  header.link.double_click
end

When /^I click the (\w+) (\w+) search field item$/ do | fieldset, field |
  @browser.div(:id => 'loading').wait_while_present
  fid = @popsicleFields[fieldset + ':' + field]
  @browser.link(:class => 'search-field-item', :data_field_field => fid).click
end

When /^I click the (\w+) (\w+) (\w+) checkbox$/ do | x, y, z |
  @browser.checkbox(:id => "#{x}-#{y}-#{z}").set
end

Then /^there are (\d+) results for (\w+) (\w+)$/ do | num, fieldset, field |
  @browser.div(:id => 'loading').wait_while_present
  fid = @popsicleFields[fieldset + ':' + field]
  totals = @browser.h5(:data_field_field => fid)
  totals.wait_until_present
  totals.text.should match(/\(#{num}\)/)
end

Then /^there are search results for (\d+) fields?$/ do | num |
  @browser.div(:id => 'loading').wait_while_present
  listings = @browser.div(:id => 'search-listing')
  listings.wait_until_present
  listings.h5s.length.should == num.to_i
end

Then /^the search form will be restricted to only (\w+) (\w+)$/ do | fieldset, field |
  fid = @popsicleFields[fieldset + ':' + field]
  @browser.span(:id => 'search-field-label').link(:data_field_field => fid).text.should match /#{fieldset}: #{field}/
  @browser.hidden(:id => 'document-search-field').value.should == "[\"#{fid}\"]"
end

Then /^the search form will be restricted to (\w+) (\w+)$/ do | fieldset, field |
  fid = @popsicleFields[fieldset + ':' + field]
  @browser.span(:id => 'search-field-label').link(:data_field_field => fid).text.should match /#{fieldset}: #{field}/
  @browser.hidden(:id => 'document-search-field').value.should match /#{fid}/
end

Then /^the search form will be unrestricted$/ do
  @browser.hidden(:id => 'document-search-field').value.should == ''
end

Then /^the ([a-f0-9]{32}) document is listed under (\w+) (\w+)$/ do | document, fieldset, field |
  fid = @popsicleFields[fieldset + ':' + field]
  result = @browser.element(:css => "#results-for-field-#{fid} > table > tbody > tr > th > a[href='##{document}']")
  result.wait_until_present
  result.should be_visible
end
