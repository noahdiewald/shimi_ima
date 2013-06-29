Before('@firefox') do
  @baseURL = "http://tester:tester@127.0.0.1:8000"
  @redis = Redis.new
  @browser = Watir::Browser.new :chrome
  @projectName = "__test_#{(0...8).map{(65+rand(26)).chr}.join}"
  @redis.set('current_project_name', @projectName)
end

After('@firefox') do |scenario|
  @browser.close unless @inspect_window
  @redis.set('last_project_name', @projectName)
end
