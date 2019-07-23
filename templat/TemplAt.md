## TemplAt

TemplAt is Attache embedded into HTML, and can be used to serve dynamic Attache content to servers.

### Syntax

Hello, World!

```
<attache>
Print["Hello, World!"]
Exit[]
</attache>
```

Example of dynamically-served Date function:

```<p>The time is:
<attache>
HTMLReplace[el, HTMLCreate["code", document, DateFormat["%Y"]]]
</attache>
</p>
```

Might output:

```
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" "http://www.w3.org/TR/REC-html40/loose.dtd">
<html><body><p>The time is:
<code>2018</code>
</p></body></html>
```
