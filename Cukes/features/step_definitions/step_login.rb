Given /^I am (\w+) with (\w+)$/ do | user, pass |
  @uri = URI("http://#{user}:#{pass}@127.0.0.1:8000/projects")
end

When /^I request the projects page$/ do
  @res = Net::HTTP.get_response(@uri)
end

Then /^I am provided the (\w+) code$/ do | code |
  @res.code == code
end
