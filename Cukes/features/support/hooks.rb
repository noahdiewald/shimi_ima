Before('@webbrowser, @firefox') do
  @baseURL = "http://tester:tester@127.0.0.1:8000"
  @baseCouchURL = "http://tester:tester@127.0.0.1:5984"
  @baseApp = "#{@baseCouchURL}/shimi_ima"
  @projectName = '__test_NGZGQAJR'
  @projectId = '3774eb0e92817f2615133467018a6602'
  @projectURL = "#{@baseCouchURL}/project-#{@projectId}"
  @projectConfig = "#{@baseURL}/projects/project-#{@projectId}/config"
  @projectIndexes = "#{@baseURL}/projects/project-#{@projectId}/index_tool"
  @popsicleURL = "#{@baseURL}/projects/project-#{@projectId}/doctypes/f5cae2e2f8f54a608f4ad45e8f422110/documents"
  @popsicleFields = json_to_field_lookup("Popsicle_fields.json")
  @popsicleFieldsets = json_to_fieldset_lookup("Popsicle_fields.json")
end

Before('@webbrowser') do
  @browser = Watir::Browser.new :chrome
end

Before('@firefox') do
  @browser = Watir::Browser.new :firefox
end

After('@webbrowser, @firefox') do |scenario|
  @browser.close unless @inspect_window
end
