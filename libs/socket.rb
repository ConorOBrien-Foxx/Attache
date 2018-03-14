require 'socket'
require_relative '../TemplAt.rb'

def local_server(inst, input, port=8000)
    server = TCPServer.new port

    # modified from https://stackoverflow.com/a/34912856/4119004
    loop {
        socket = server.accept
        
        content = if AtState.func_like? input
            input[inst]
        else
            input
        end
        
        content = content.to_s
        
        socket.write "HTTP/1.1 200 OK\r\n" +
                     "Content-Type: text/html; charset=utf-8\r\n" +
                     "Content-Length: #{content.bytesize}\r\n"

        socket.write "\r\n"
        socket.write content
        
        socket.close
    }
end

AtState.function("LocalServer") { |inst, input, port=8000|
    local_server inst, input, port
}

AtState.function("ServeTemplat") { |inst, code, port=8000|
    local_server inst, lambda { |inst|
        content = code[inst].to_s if AtState.func_like? code
        templat content
    }, port
}