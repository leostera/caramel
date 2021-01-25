# Contributing to the Manual

Thanks for contributing to the 📙 Caramel Manual!

We'd love this manual to be useful, inclusive, and friendly, so make sure your
tone matches this.

The manual sources live in `<root>/manual/src`, and it is written in Markdown.

**Topics** 
* [Prerequisites](#prerequisites)
* [Manual Structure](#manual-structure)
* [Editing Existing Pages](#editing-existing-pages)
* [Adding New Pages](#adding-new-pages)
* [Previewing Your Changes](#previewing-your-changes)
* [Publishing Changes](#publishing-changes)

### Prerequisites

* [mdbook](https://rust-lang.github.io/mdBook/) 
* make


## Manual Structure

The structure of the manual is reflected in the `SUMMARY.md` file, so that the
links that go under the same heading are within the same folder.

Here you can see that everything under the "Getting Started" section is under
the `./getting_started` folder.

```markdown
{{#include ../../../manual/src/SUMMARY.md}}
```

## Editing Existing Pages

If you find a typo in some page, you can look into the `SUMMARY.md` to see which
markdown file to modify.

You can edit the markdown files with any editor you choose, and submit a Pull
Request.

Try to stick to 80 characters per line or less.

## Adding New Pages

Start the server (see [Previewing Your Changes](#previewing-your-changes)) and
add a new page in `SUMMARY.md`.

Once you save it, the server will have created the right folders and files for
you.

Now you can edit the new empty Markdown files as if they were pre-existing
pages.

**If you want help writing a section**, please draft your ideas and open a Draft
Pull Request so we can work on it together.

## Previewing Your Changes

`mdbook` comes with a built in server with hot-reloading. This lets you see the
results automatically in the browser.

Start the server with `mdbook serve` and open the URL it lists. If your port
3000 is already taken, you can call `mdbook serve -p PORT` to use a different
one.

## Publishing Changes

Once your changes are ready for publishing, you can run `make manual` to
generate the final version of the sources.

These new ones live in `./docs/manual`. Commit that entire folder before you
open a Pull Request.
