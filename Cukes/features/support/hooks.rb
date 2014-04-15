Before('@webbrowser') do
  @baseURL = "http://tester:tester@127.0.0.1:8000"
  @baseCouchURL = "http://tester:tester@127.0.0.1:5984"
  @baseApp = "#{@baseCouchURL}/shimi_ima"
  @projectName = '__test_NGZGQAJR'
  @projectId = '3774eb0e92817f2615133467018a6602'
  @projectURL = "#{@baseCouchURL}/project-#{@projectId}"
  @projectConfig = "#{@baseURL}/projects/project-#{@projectId}/config"

  @browser = Watir::Browser.new :chrome
end

After('@webbrowser') do |scenario|
  @browser.close unless @inspect_window
end
