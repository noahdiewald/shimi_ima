Before('@firefox') do
  @redis = Redis.new
  @browser = Watir::Browser.new :ff
  @projectName = "__test_#{(0...8).map{(65+rand(26)).chr}.join}"
  @redis.set('current_project_name', @projectName)
end

After('@firefox') do |scenario|
  @browser.close unless scenario.failed?
  @redis.set('last_project_name', @projectName)
end
