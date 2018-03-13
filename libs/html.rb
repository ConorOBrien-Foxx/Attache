require 'nokogiri'

# test = Nokogiri::HTML()

AtState.function("HTML") { |inst, arg|
    Nokogiri::HTML(arg)
}