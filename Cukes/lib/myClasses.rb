require 'watir-webdriver'
require 'net/http'
require 'redis'

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