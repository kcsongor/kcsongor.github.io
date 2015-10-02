---
layout: post
title: Time travel in Haskell for dummies
excerpt: "Some interesting consequences of lazy evaluation."
modified: 2015-10-02
tags: [haskell, tutorial, monad, tardis]
comments: true
---

<section id="table-of-contents" class="toc">
  <header>
    <h3>Overview</h3>
  </header>
<div id="drawer" markdown="1">
*  Auto generated table of contents
{:toc}
</div>
</section><!-- /#table-of-contents -->

### module TimeTravel where

Browsing Hackage the other day, I came across the [Tardis Monad](https://hackage.haskell.org/package/tardis-0.3.0.0/docs/Control-Monad-Tardis.html).
Reading its description, it turns out that the Tardis monad is capable of
sending state back in time. Yep. Back in time.

### How?
No, it’s not the reification of [some hypothetical time-travelling particle](https://en.wikipedia.org/wiki/Tachyon), rather a really clever way of exploiting Haskell’s
laziness. 

In this rather lengthy post, I’ll showcase some interesting consequences of 
lazy evaluation and the way to work ourselves up from simple examples to
’time travelling’ craziness through different levels of abstraction.

### The repMax problem
Imagine you had a list, and you wanted to replace all the elements of 
the list with the largest element, by only passing the list once.

You might say something like "Easier said than done, how do I know the
largest element without having passed the list before?"

Let’s start from the beginning:
First, you ask the future for the largest element of the list, 
(don’t worry, this will make sense in a bit)
let’s call this value `rep` (as in the value we replace stuff with).

Walking through the list, you do two things:
  * replace the current element with rep
  * ’return’ the larger of the current element and the largest element of the remaining list.

When only one element remains, replace it with rep, and return what was there
originally. (this is the base case)

Right, at the moment, we haven’t acquired the skill of seeing the future, so
we just write the rest of the function with that bit left out.

### Heading 3

#### Heading 4

##### Heading 5

###### Heading 6

### Body text

Lorem ipsum dolor sit amet, test link adipiscing elit. **This is strong**. Nullam dignissim convallis est. Quisque aliquam.

*This is emphasized*. Donec faucibus. Nunc iaculis suscipit dui. 53 = 125. Water is H2O. Nam sit amet sem. Aliquam libero nisi, imperdiet at, tincidunt nec, gravida vehicula, nisl. The New York Times (That’s a citation). Underline.Maecenas ornare tortor. Donec sed tellus eget sapien fringilla nonummy. Mauris a ante. Suspendisse quam sem, consequat at, commodo vitae, feugiat in, nunc. Morbi imperdiet augue quis tellus.

HTML and CSS are our tools. Mauris a ante. Suspendisse quam sem, consequat at, commodo vitae, feugiat in, nunc. Morbi imperdiet augue quis tellus. Praesent mattis, massa quis luctus fermentum, turpis mi volutpat justo, eu volutpat enim diam eget metus.

### Blockquotes

> Lorem ipsum dolor sit amet, test link adipiscing elit. Nullam dignissim convallis est. Quisque aliquam.

## List Types

### Ordered Lists

1. Item one
   1. sub item one
   2. sub item two
   3. sub item three
2. Item two

### Unordered Lists

* Item one
* Item two
* Item three

## Tables

| Header1 | Header2 | Header3 |
|:--------|:-------:|--------:|
| cell1   | cell2   | cell3   |
| cell4   | cell5   | cell6   |
|----
| cell1   | cell2   | cell3   |
| cell4   | cell5   | cell6   |
|=====
| Foot1   | Foot2   | Foot3
{: rules="groups"}

## Code Snippets

{% highlight haskell %}
#container {
  float: left;
  margin: 0 -240px 0 0;
  width: 100%;
}
{% endhighlight %}

## Buttons

Make any link standout more when applying the `.btn` class.

{% highlight html %}
<a href="#" class="btn btn-success">Success Button</a>
{% endhighlight %}

<div markdown="0"><a href="#" class="btn">Primary Button</a></div>
<div markdown="0"><a href="#" class="btn btn-success">Success Button</a></div>
<div markdown="0"><a href="#" class="btn btn-warning">Warning Button</a></div>
<div markdown="0"><a href="#" class="btn btn-danger">Danger Button</a></div>
<div markdown="0"><a href="#" class="btn btn-info">Info Button</a></div>

## Notices

**Watch out!** You can also add notices by appending `{: .notice}` to a paragraph.
{: .notice}
