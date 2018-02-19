# http module

require 'net/http'

def url_string(str)
    unless /^https?:\/\// === str
        str = "http://" + str
    end
    URI.parse str
end

AtState.function("Curl") { |inst, arg|
    Net::HTTP.get(url_string(arg))
}

AtState.function("CurlJson") { |inst, arg|
    require 'json'
    JSON.parse Net::HTTP.get(url_string(arg))
}