BOILERPLATES = {
    header: "<h2><code>%<name>s[%<args>s] &rarr; <span class=\"return\">%<return_type>s</span> </code><span class=\"genre\">(%<genre>s)</span></h2>",
    header_op: "<h2><code>%<left>s %<name>s %<right>s &rarr; <span class=\"return\">%<return_type>s</span> </code><span class=\"genre\">(%<genre>s)</span></h2>",
    header_op_unary: "<h2><code>%<name>s %<left>s &rarr; <span class=\"return\">%<return_type>s</span> </code><span class=\"genre\">(%<genre>s)</span></h2>",
    param: "<p><code>%<name>s</code> - %<description>s</p>",
    argument: "<span class=\"type\">%<type>s</span> %<name>s",
    html: "<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF8\">
    <title>%<title>s</title>
    <link rel=\"STYLESHEET\" href=\"styles.css\">
</head>
<body><div id=\"content\"><h1>%<title>s</h1>%<body>s</div></body>
</html>",
    example: "<h3>Example</h3><div class=\"example\"><code>%<code>s</code></div>",
    option: "<p><code>%<name>s</code> &rarr; %<description>s</p>"
}