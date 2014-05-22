Given /^the search pane is open$/ do
  step "a doctype with two documents exists"
  step "I am at the document page"
  step "I click the search panel menu item"
end

When /^I click the search header for (\w+) (\w+) do | fieldset, field |
  @browser.div(:id => 'loading').wait_while_present
  fid = @popsicleFields[fieldset + ':' + field]
  @browser.h5(:'data-field-field' => fid).click
end

Then /^there are (\d+) results for (\w+) (\w+)$/ do | num, fieldset, field |
  fid = @popsicleFields[fieldset + ':' + field]  
  @browser.h5(:'data-field-field' => fid).text.should match(/\(#{num}\)/)
end

Then /^the ([a-f0-9]{32}) document is listed under (\w+) (\w+)$/ do | document, fieldset, field |
  fid = @popsicleFields[fieldset + ':' + field]
  @browser.element(:css => "#results-for-field-#{fid} > table > tbody > tr > th > a[href='##{document}']").should be_exists
end
