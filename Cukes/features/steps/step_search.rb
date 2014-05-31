Given /^the search pane is open$/ do
  step "a doctype with two documents exists"
  step "I am at the document page"
  step "I click the search panel menu item"
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

Then /^there are (\d+) results for (\w+) (\w+)$/ do | num, fieldset, field |
  @browser.div(:id => 'loading').wait_while_present
  fid = @popsicleFields[fieldset + ':' + field]
  totals = @browser.h5(:data_field_field => fid)
  totals.wait_until_present
  totals.text.should match(/\(#{num}\)/)
end

Then /^the ([a-f0-9]{32}) document is listed under (\w+) (\w+)$/ do | document, fieldset, field |
  fid = @popsicleFields[fieldset + ':' + field]
  @browser.element(:css => "#results-for-field-#{fid} > table > tbody > tr > th > a[href='##{document}']").should be_exists
end
