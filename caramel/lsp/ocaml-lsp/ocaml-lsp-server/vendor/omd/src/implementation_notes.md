# Notes on the Implementation and Semantics of omd

## In short

I believe that all features described in
<http://daringfireball.net/projects/markdown/syntax> have now been
implemented. Extensive testing should be done.


Parsing: it mainly relies on the property that two consecutive tokens
produced by the lexer cannot designate the same "thing". For instance,
there can't be [Word "foo"; Word "bar"] because it should be
[Word "foobar"] instead. There can't be [Newlines 4; Newline; Newlines 3] because it should be
[Newlines(10)]  (yes it's 10, not 8).


## More details


### Checklist
 * HTML
   * As in "standard" Markdown, it's a "subset" of HTML with **restrictions** on the syntax that is supported. For instance, one cannot write `< a href...` instead of `<a href...` because a space following the `<` character will make it "not HTML". It's the same kind of restrictions for closing tags.
   * block-level
     * partly done
   * span-level
     * partly done
   * Some work still has to be done but it may already work. All HTML tags are basically in a single category for now. So, in order to have a block-level tag treated as a block-level tag, one should put it after 2 line breaks.

 * paragraphs
   * done
 * automatic escaping for special characters
   * done
 - Email-style quoting  (block-quoting)
   * done
 * Titles
   * hash-style (Atx-style): done
   - Setext-style (with - and =): done
 * Lists
   * unordered: done
   * ordered: done
   * paragraphs in lists: partly done
   * blockquote inside lists: done
   * code inside lists: done
 * Horizontal rules:
   * with stars: done
   * with dashes: done
 * Links
   * inline: done
   * reference: done (preliminary testing done, but needs more testing)
 * Emphasis
   * single, double, triple asterisk: done
   * single, double, triple underscore: done
 * Image insertions: done (but needs more testing)
 * Semi-automatic links: done (but needs more testing)
 * Backslashes: done
   * as with `pandoc`, escaping a line break with a backslash means `<br/>`. For instance, `"plop\\nhello"` is translated to `"<p>plop<br/>hello</p>"` (this has been implemented on 2013.08.15)
 * Code:
   * verbatim: done (but needs more testing)
   * syntax-highlighted code: *todo*


### Flaws in Markdown

Since there are no errors in  Markdown, it means taht everything has a
meaning.  Sometimes, one has to imagine a meaning that is not too much
nonsense.


#### Lists

##### Problem Description
There are several semantics for a "broken" list such as the following one:
```
 * Indentation 1, Element 1
 * Indentation 1, Element 2
   * Indentation 3, Element 1
   * Indentation 3, Element 2
  * Indentation 2, Element 1
  * Indentation 2, Element 2
   * Indentation 3, Element 1 (not 3)
 * Indentation 1, Element 3
```

I have chosen the following semantics, because to me that it's the less nonsense I have ever thought about:

##### Semantics
Let N be the indentation level of the current element.
- If N is equal to the previous indentation, then it's still the same list as the current one.
- If N is greater than the previous indentation, then it's a new list.
- If N is lesser than the previous indentation, then I check its predecessors: 
  * if N is the level of a predecessor and no other level inbetween is lesser, then it means that the current item belongs to a list that hasn't been closed yet, so I close the current list and I delegate the rest to the closest parent (which does mean that the current item will be processed _again_).
  * else, it means that it's a kind of wrong level (don't forget N is smaller than the previous indentation), so I close the current list and open a new one with the current item.


```
 * Indentation 1, Element 1
 * Indentation 1, Element 2
% here I do not close (I1), and I open for the next one (I3)
   * Indentation 3, Element 1
   * Indentation 3, Element 2
% here I close (I3) and open for the next one (I2)
  * Indentation 2, Element 1
  * Indentation 2, Element 2
% here I close (I2) and open for the next one (I3)
   * Indentation 3, Element 1 (not 3)
% here I close (I3) and continue for the next one (I1)
 * Indentation 1, Element 3
```


-----
file implementation_notes.md

