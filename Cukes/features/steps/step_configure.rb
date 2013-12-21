# Given(/^there is no open dialog$/) do
#   cancelButton = @browser.span(:text => 'Cancel')
#   if cancelButton.exists?
#     cancelButton.click
#   end
# end

Given(/^I have created a project$/) do
  step "I navigate to the projects page"
  step "I click the New Projects button"
  step "I fill in the project name"
  step "I click the add project button"
  projectName = @redis.get('current_project_name')
  targ = {:text => projectName}
  @browser.link(targ).wait_until_present(5)
  projectId = @browser.link(targ).href.split("/")[-2].split("-").last
  @redis.set('current_project_id', projectId)
end

Given(/^I created the (\w+) document type$/) do | name |
  projectId = @redis.get('current_project_id')
  @browser.goto("#{@baseURL}/projects/project-#{projectId}/config")
  step("click the Add Document Type button")
  step("I fill in #{name} in the name editor input")
  step("I click the editor create button")
  @browser.div(:id => 'loading').wait_while_present
end

# Given(/^the (\w+) document type is selected$/) do | name |
#   projectId = @redis.get('current_project_id')
#   @browser.goto("#{@baseURL}/projects/project-#{projectId}/config")
#   @browser.link(:href => "config/doctypes/#{name}")  
#   @browser.div(:id => 'loading').wait_while_present
# end

# Given(/^the (\w+) fieldset is selected in the (\w+) document type$/) do | fieldset, doctype |
#   step("the #{doctype} document type is selected")
#   fieldsetData = JSON.parse(@redis.get(fieldset))
#   @browser.link(:text => fieldsetData['label']).click
#   @browser.div(:id => 'loading').wait_while_present
# end

# When(/^I click the (\w+) fieldset delete button$/) do | field |
#   fieldsetData = JSON.parse(@redis.get(field))
#   @browser.link(:id => "delete-#{fieldsetData['id']}").click
#   @browser.alert.ok
#   @browser.div(:id => 'loading').wait_while_present
# end

# When(/^I click the (\w+) field delete button$/) do | field |
#   fieldData = JSON.parse(@redis.get(field))
#   @browser.link(:id => "delete-field-#{fieldData['id']}-button").click
#   @browser.alert.ok
#   @browser.div(:id => 'loading').wait_while_present
# end

# When(/^I click the (\w+) Edit Field button$/) do | field |
#   fieldData = JSON.parse(@redis.get(field))
#   @browser.link(:id => "edit-field-#{fieldData['id']}-button").click
# end

# When(/^I click the (\w+) Add New Field button$/) do | fieldset |
#   fieldsetData = JSON.parse(@redis.get(fieldset))
#   @browser.link(:id => "add-field-to-#{fieldsetData['id']}").click
# end

# When(/^I give (\w+) as the field name$/) do | value |
#   fieldName = @browser.text_field(:id => 'field-name-input')
#   fieldName.set value
# end

# When(/^I give (.+) as the field label$/) do | value |
#   fieldLabel = @browser.text_field(:id => 'field-label-input')
#   fieldLabel.set value
# end

# When(/^I give (\w+) as the field type$/) do | value |
#   fieldType = @browser.select_list(:id => 'field-subcategory-input').option(:value => value)
#   fieldType.select
# end

# When(/^I give (.+) as the minimum value$/) do | value |
#   fieldMin = @browser.text_field(:id => 'field-min-input')
#   fieldMin.set(value) unless value === "skip"
# end

# When(/^I give (.+) as the maximum value$/) do | value |
#   fieldMax = @browser.text_field(:id => 'field-max-input')
#   fieldMax.set(value) unless value === "skip"
# end

# When(/^I give (.+) as the regex value$/) do | value |
#   fieldRegex = @browser.text_field(:id => 'field-regex-input')
#   fieldRegex.set(value) unless value === "skip"
# end

# When(/^I give (\w+) as the default value$/) do | value |
#   fieldDefault = @browser.text_field(:id => 'field-default-input')
#   fieldDefault.set(value) unless value === "skip"
# end

# When(/^I give (.+) as the allowed values$/) do | value |
#   fieldAllowed = @browser.text_field(:id => 'field-allowed-input')
#   fieldAllowed.set(value) unless value === "skip"
# end

# When(/^I give (true|false) as the document head value$/) do | value |
#   fieldHead = @browser.checkbox(:id => 'field-head-input')
#   fieldHead.set if value === "true"
# end

# When(/^I give (true|false) as the document reversal value$/) do | value |
#   fieldReverse = @browser.checkbox(:id => 'field-reversal-input')
#   fieldReverse.set if value === "true"
# end

# When(/^I give (true|false) as the required field value$/) do | value |
#   fieldRequired = @browser.checkbox(:id => 'field-required-input')
#   fieldRequired.set if value === "true"
# end

# When(/^I give (\d+) as the field order value$/) do | value |
#   fieldOrder = @browser.text_field(:id => 'field-order-input')
#   fieldOrder.set(value)
# end

# When(/^I click the Field Save button$/) do
#   @browser.span(:text => 'Save').click
#   @browser.div(:id => 'loading').wait_while_present
# end

# When(/^I click the (\w+) add fieldset button$/) do | name |
#   @browser.link(:id => "add-fieldset-to-#{name}").click
#   @browser.text_field(:id => 'fieldset-name-input').wait_until_present
# end

# When(/^I give (\w+) as the fieldset name$/) do | value |
#   fieldsetName = @browser.text_field(:id => 'fieldset-name-input')
#   fieldsetName.set value
# end

# When(/^I give (.+) as the fieldset label$/) do | value |
#   fieldsetLabel = @browser.text_field(:id => 'fieldset-label-input')
#   fieldsetLabel.set value
# end

# When(/^I give (true|false) as the fieldset collapse value$/) do | value |
#   fieldsetCollapse = @browser.checkbox(:id => 'fieldset-collapse-input')
#   fieldsetCollapse.set if value === "true"
# end

# When(/^I give (true|false) as the fieldset multiple value$/) do | value |
#   fieldsetMultiple = @browser.checkbox(:id => 'fieldset-multiple-input')
#   fieldsetMultiple.set if value === "true"
# end

# When(/^I give (\d+) as the fieldset order$/) do | value |
#   fieldsetOrder = @browser.text_field(:id => 'fieldset-order-input')
#   fieldsetOrder.set value
# end

# When(/^I click the Fieldset Save button$/) do
#   @browser.span(:text => 'Save').click
#   @browser.div(:id => 'loading').wait_while_present
# end

When(/^I click the project Configure button$/) do
  projectId = @redis.get('current_project_id')
  configureButton = @browser.link(:href => "/projects/project-#{projectId}/config")
  configureButton.click
end

When(/^click the Add Document Type button$/) do
  button = @browser.link(:id => 'doctypes-add-button')
  button.wait_until_present
  button.click
end

When(/^I fill in (\w+) in the (\w+) editor input$/) do | data, input |
  doctypeNameField = @browser.text_field(:name => input)
  doctypeNameField.set data
end

When(/^I open the (\w+) document type in the editor$/) do | name |
  @browser.link(:text => name).click
end

When(/^I click the editor (\w+) button$/) do | bttn |
  @browser.link(:id => "config-#{bttn}-button").click
  if bttn == "delete"
    @browser.alert.ok
  end
  @browser.div(:id => 'loading').wait_while_present
end

# Then(/^the (.+) fieldset exists$/) do | label |
#   fieldsetSectionHeader = @browser.link(:text => label)
#   fieldsetSectionHeader.should be_exists
#   fieldsetId = fieldsetSectionHeader.attribute_value('data-group-id')[13 .. -1]
#   fieldsetDiv = @browser.div(:id => fieldsetId)
#   fieldsetDiv.should be_exists
#   fieldsetData = {
#     id: fieldsetId,
#     label: fieldsetDiv.attribute_value('data-fieldset-label')
#   }.to_json
#   @redis.set(fieldsetDiv.attribute_value('data-fieldset-name'), fieldsetData)
# end

Then(/^the document type (\w+) has been deleted$/) do | name |
  @browser.link(:text => name).should_not be_exists
end

Then(/^there is a (\w+) document type$/) do | name |
  @browser.link(:text => name).wait_until_present
  true
end

# Then(/^the (\w+) field exists$/) do | name |
#   fieldRow = @browser.td(:text => name).parent
#   fieldRow.should be_exists
#   fieldId = fieldRow.attribute_value('data-field-id')
#   fieldData = {
#     id: fieldId,
#     label: fieldRow.attribute_value('data-field-label')
#   }.to_json
#   @redis.set(name, fieldData)
# end

# Then(/^the (\w+) field has (.+) for the (\w+) value$/) do | name, value, field_attr |
#   fieldRow = @browser.td(:text => name).parent
#   fieldRow.should be_exists
#   fieldRow.attribute_value("data-field-#{field_attr}").should == value
#   fieldId = fieldRow.attribute_value('data-field-id')
#   fieldData = {
#     id: fieldId,
#     label: fieldRow.attribute_value('data-field-label')
#   }.to_json
#   @redis.set(name, fieldData)
# end

# Then(/^the (\w+) field has (.+) for the (\w+) value in a (\w+) displayed$/) do | name, value, field_attr, input_type |
#   fieldAttrField = @browser.send(input_type.to_sym, :id => "field-#{field_attr}-input")
#   fieldAttrField.should be_exists
#   if input_type == "checkbox"
#     fieldAttrField.set?.should == (value == "true")
#   else
#     fieldAttrField.value.should == value
#   end
# end

# Then(/^the (\w+) field is deleted$/) do | field |
#   fieldData = JSON.parse(@redis.get(field))
#   @browser.link(:id => "delete-field-#{fieldData['id']}-button").should_not be_exists
# end

# Then(/^the (.+) fieldset is deleted$/) do | label |
#   fieldsetSectionHeader = @browser.link(:text => label)
#   fieldsetSectionHeader.should_not be_exists
# end
