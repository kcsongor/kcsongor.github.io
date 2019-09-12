---
layout: post
title: "Most underrated vim features: C-a"
excerpt: "In which I present a hidden gem in the vim text editor."
modified: 2019-09-12
tags: [productivity, vim]
comments: true
---

The aim of this series of blog posts is to shed light on some of the
darker corners of the vim text editor that I have encountered over the
years. Each post will focus on one particular feature, and should take
no longer than a couple of minutes to read.

Today, I'd like to talk about the `<C-a>` key sequence (that is,
control+a). It is extremely simple: pressing `<C-a>` searches the
current line (starting at the cursor position) for a number, then
increments it.

For example:
{% highlight text %}
this is a number: 10.
^
{% endhighlight %}
`<C-a>`
{% highlight text %}
this is a number: 11.
                   ^
{% endhighlight %}
where `^` marks the cursor position.

Its inverse is `<C-x>`, which decrements the number.
We can also specify a count, for example `20<C-x>` will result in:

{% highlight text %}
this is a number: -9.
                   ^
{% endhighlight %}

Hexidecimal and binary numbers are supported too.
For example, to convert `192` to hex, we can do

{% highlight text %}
this is a hexadecimal number: 0x0.
^
{% endhighlight %}
`192<C-a>`
{% highlight text %}
this is a hexadecimal number: 0xc0.
                                 ^
{% endhighlight %}
