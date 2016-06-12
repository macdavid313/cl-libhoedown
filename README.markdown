# cl-libhoedown 

Common Lisp Binding for [Hoedown](https://github.com/hoedown/hoedown), a standards compliant, fast, secure markdown processing library in C.

## Installation

You need to first install Hoedown, either clone or download it from this [page](https://github.com/hoedown/hoedown).

```
git clone https://github.com/hoedown/hoedown.git
cd hoedown
make && make install
```

Then just `(ql:quickload :cl-libhoedown)` in your REPL. Note that before it's collected by Quicklisp, you need to put it in your `quicklisp/local-projects` directory.

## Usage

All API symbols are exported from package `cl-libhoedown`(and its nicknames `libhoedown` or `hoedown`).

### render

```common-lisp
HOEDOWN> (render "#h1

This is Head1.

##h2

This is Head2.

###h3

This is Head3. Over")

"<h1>h1</h1>

<p>This is Head1.</p>

<h2>h2</h2>

<p>This is Head2.</p>

<h3>h3</h3>

<p>This is Head3. Over</p>
"
```

Simple to use. If it's a pathname that passed to `render`, it will use `uiop:read-file-string` first and then do the rendering.

```common-lisp
HOEDOWN> (render #P"/tmp/test.md")
"<h1>h1</h1>

<p>This is Head1.</p>

<h2>h2</h2>

<p>This is Head2.</p>

<h3>h3</h3>

<p>This is Head3. Over</p>
"
```

There are a lot of options that can affect the behavior of the **rendering**.

<h3>*hoedown-extensions*</h3>

Hoedown Extensions, which affects the behavior of Markdown syntax such as tables, autolinking, footnotes... The enabled extensions are:

```common-lisp
'(:hoedown-ext-tables :hoedown-ext-fenced-code :hoedown-ext-footnotes
  :hoedown-ext-autolink :hoedown-ext-strikethrough :hoedown-ext-underline
  :hoedown-ext-highlight :hoedown-ext-quote :hoedown-ext-superscript
  :hoedown-ext-math :hoedown-ext-no-intra-emphasis :hoedown-ext-space-headers
  :hoedown-ext-math-explicit :hoedown-ext-disable-indented-code)
```

You can enable multiple extensions by using `add-hoedown-extensions`. For example, if a markdown file `/tmp/test.md` looks like this:

```
#h1

This is Head1. Let's test some `autolink`.

* Google: http://google.com
* Bing: http://bing.com
* My Blog: http://gty.org.in

##h2

This is Head2. Let's test some `strikethrough`.

Trump is the next USA president? ~~This is totally nonsence.~~

###h3

This is Head3. Test some `footnotes` then over.

McCLIM[^1] now has a new website!

May 2016 Quicklisp[^2] dist update now available!

Over.

[^1]: McCLIM, https://common-lisp.net/project/mcclim/posts/New-website.html

[^2]: Quicklisp, https://quicklisp.org
```

```common-lisp
HOEDOWN> (add-hoedown-extensions :hoedown-ext-autolink :hoedown-ext-footnotes :hoedown-ext-strikethrough)
(:HOEDOWN-EXT-STRIKETHROUGH :HOEDOWN-EXT-FOOTNOTES :HOEDOWN-EXT-AUTOLINK)
HOEDOWN> (render #P"/tmp/test.md")
"<h1>h1</h1>

<p>This is Head1. Let&#39;s test some <code>autolink</code>.</p>

<ul>
<li>Google: <a href=\"http://google.com\">http://google.com</a></li>
<li>Bing: <a href=\"http://bing.com\">http://bing.com</a></li>
<li>My Blog: <a href=\"http://gty.org.in\">http://gty.org.in</a></li>
</ul>

<h2>h2</h2>

<p>This is Head2. Let&#39;s test some <code>strikethrough</code>.</p>

<p>Trump is the next USA president? <del>This is totally nonsence.</del></p>

<h3>h3</h3>

<p>This is Head3. Test some <code>footnotes</code> then over.</p>

<p>McCLIM<sup id=\"fnref1\"><a href=\"#fn1\" rel=\"footnote\">1</a></sup> now has a new website!</p>

<p>May 2016 Quicklisp<sup id=\"fnref2\"><a href=\"#fn2\" rel=\"footnote\">2</a></sup> dist update now available!</p>

<p>Over.</p>

<div class=\"footnotes\">
<hr>
<ol>

<li id=\"fn1\">
<p>McCLIM, <a href=\"https://common-lisp.net/project/mcclim/posts/New-website.html\">https://common-lisp.net/project/mcclim/posts/New-website.html</a>&nbsp;<a href=\"#fnref1\" rev=\"footnote\">&#8617;</a></p>
</li>

<li id=\"fn2\">
<p>Quicklisp, <a href=\"https://quicklisp.org\">https://quicklisp.org</a>&nbsp;<a href=\"#fnref2\" rev=\"footnote\">&#8617;</a></p>
</li>

</ol>
</div>
"
```

There is also a function `delete-hoedown-extensions`, which I guess you'll barely have to use...

<h3>*hoedown-html-flags*</h3>

Hoedown html flags, which derectly affects the behavior of hoedown html renderers. The enabled flags are:

```common-lisp
'(:hoedown-html-skip-html :hodown-html-escape :hoedown-html-hard-wrap :hoedown-html-use-html)
```

You can enable multiple flags by using `add-hoedown-html-flags`, and there's also a function `delete-hoedown-html-flags`.

<h3>*hoedown-buffer-unit-size*</h3>

Reallocation unit size (0 = read-only buffer) for a hoedown buffer, set to `16` by default.

<h3>*hoedown-html-renderer-nesting-levels*</h3>

Nesting levels for a hoedown html renderer, set to `0` by default.

<h3>*hoedown-document-max-nesting*</h3>

Max nesting for a hoedown document, set to `16` by default.

### hoedown-version

Retrieve Hoedown's version numbers, returning 4 values where the first is a string representing the version and
the 2nd, 3rd and 4th are major, minor and revision number repsectively.

```common-lisp
HOEDOWN> (hoedown-version)

"3.0.7"
3
0
7
```

## Limitations

For now, this binding only supports a subset of features from Hoedown. So the feature work are obviously make the APIs more complete.

### TODO

- [ ] Enable more options for a hoedown html renderer.
- [ ] Enable customizing renderers.
- [ ] Support various callbacks.

## Author

* David Gu (david_guru@gty.org.in)

## Copyright

Copyright (c) 2016 David Gu (david_guru@gty.org.in)
