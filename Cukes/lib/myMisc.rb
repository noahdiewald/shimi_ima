# Required gems and helper functions are going here so far.

require 'watir-webdriver'
require 'net/http'
require 'json'
require 'redis'

if ENV['HEADLESS']
  require 'headless'
  headless = Headless.new
  headless.start
  at_exit do
    headless.destroy
  end
end

class String
  def to_dash
    gsub(/([A-Z]+)([A-Z][a-z])/,'\1-\2').
    gsub(/([a-z\d])([A-Z])/,'\1-\2').
    downcase
  end

  def escape_first_digit
    gsub(/^(\d)/, '\\\3\1 ')
  end
end

def delete_project
  project_info = get_project

  if project_info
    delete_project_database(project_info['value'])
    delete_project_document(project_info['id'], project_info['doc']['_rev'])
  end
end

def create_project
  create_project_document
  create_project_database
  replicate_project_code
end

def post_fixture(fixture, bulk = false)
  uri = @projectURL
  uri = uri + '/_bulk_docs' if bulk
  post(URI(uri), get_fixture_json(fixture))
end

def json_to_field_lookup(fixture)
  json = JSON.parse(get_fixture_json(fixture))
  m = json['fieldsets'].map do | fs |
    fs['fields'].map { | f |  [fs['label'] + ':' + f['label'], f['_id']] }
  end.flatten
  Hash[*m]
end

def json_to_fieldset_lookup(fixture)
  json = JSON.parse(get_fixture_json(fixture))
  m = json['fieldsets'].map do | fs |
    [fs['label'], fs['_id']]
  end.flatten
  Hash[*m]
end

def get_fixture_json(fixture)
  file = File.open("#{File.dirname(__FILE__)}/fixtures/#{fixture}", "rb")
  file.read
end

def create_project_document
  # The content of the shimi ima application database entry for the project.
  json = '{"_id":"' + @projectId + '","name":"' + @projectName + '","description":""}'
  uri = URI(@baseApp)
  post(uri, json)
end

def create_project_database
  uri = URI(@projectURL)
  put(uri)
end

def replicate_project_code
  # The replication instruction to ensure that the new project is more than just an empty database.
  json = '{"source":"shimi_ima","target":"project-' + @projectId + '","filter":"shimi_ima/upgrade"}'
  uri = URI("#{@baseCouchURL}/_replicate")
  post(uri, json)
end

def get_project
  uri = URI("#{@baseApp}/_design/shimi_ima/_view/all_projects?key=%22#{@projectName}%22&include_docs=true")
  res = Net::HTTP.get(uri)
  jbod = JSON.parse(res)
  
  if jbod['rows'].length == 1
    jbod['rows'][0]
  else
    false
  end
end

def delete_project_document(id, rev)
  delete(URI("#{@baseApp}/#{id}?rev=#{rev}"))
end

def delete_project_database(project)
  delete(URI("#{@baseCouchURL}/#{project}"))
end

def delete(uri)
  req = Net::HTTP::Delete.new(uri)
  req.basic_auth uri.user, uri.password
  request(req)
end

def post(uri, data = nil)
  req = Net::HTTP::Post.new(uri)
  req['Content-Type'] = "application/json"
  req.basic_auth uri.user, uri.password
  request(req, data)
end

def put(uri, data = nil)
  req = Net::HTTP::Put.new(uri)
  req['Content-Type'] = "application/json"
  req.basic_auth uri.user, uri.password
  request(req, data)
end

def request(req, data = nil)
  Net::HTTP.start(req.uri.hostname, req.uri.port) {|http| http.request(req, data) }
end

def multi_field(fieldset, field, index)
  fid = @popsicleFields[fieldset + ':' + field]
  @browser.text_fields(:data_field_field => fid)[index.to_i - 1]
end
