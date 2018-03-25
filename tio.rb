
require_relative 'compress.rb'

require 'zlib'
require 'base64'
FIELD_SEPARATOR = "\xff"

def finalize(state_string)
    compressed = zlib_deflate(state_string)
    encoded = Base64.encode64 compressed
    encoded.tr("+", "@").gsub(/=+/, "")
end

def tio_encode(program)
    state_string = "attache#{FIELD_SEPARATOR * 2}#{program}#{FIELD_SEPARATOR * 2}"
    url = finalize state_string
    # I have no idea why this is a thing
    url.gsub!(/\n/, "")
    "https://tio.run/###{url}"
end