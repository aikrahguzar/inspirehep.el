# Moved to: https://codeberg.org/rahguzar/inspirehep.el

# inspirehep.el: An Emacs package for High Enegry Physics literature search
[![GPL 3](https://img.shields.io/badge/license-GPLv3-blue.svg)](COPYING)

*inspirehep.el* provides an Emacs interface to the literature search for high energy physics using the JSON api provided by [INSPIRE](https://inspirehep.net/).

## Usage

Quick start: `M-x inspirehep-lookup`.

This displays a list of results in `inspirehep-mode`.  In that mode, use:

* `i` (`inspirehep-insert-bibtex`) to insert the BibTeX record of the current entry. 
* `b` (`inspirehep-select-target-buffer`) to change the file in which the BibTeX files will be inserted.
* `d` (`inspirehep-download-pdf`) to download the pdf file associated with current entry. If an `arXiv` record for the entry is found, the pdf is downloaded
from arXiv. Otherwise the first document link from record received from INSPIRE is used. This should be improved.
* `s` (`inspirehep-insert-and-download`) to both insert the BibTeX entry and download the file.
* `c` (`inspirehep-search-citations`) to search for articles that cite the current article. 
* `e` (`inspirehep-view-entry`) to view the detailed record for the entry including reference in the article.
* `a` (`inspirehep-search-author`) to search for the articles by the author at the point. If there is an author at point, you will be prompted to select one of
  the authors of the current entry.
* `r` (`inspirehep-lookup`) to start a new search.

## Customization

There are a few options to change how this package works. 

### Downloading PDF

To use the pdf download functionality the variable `inspirehep-download-directory` must be set to an existing directory.

### Pagination

INSPIRE's api returns the search results in pages. By default this package sets the number of results per page to 25. The value can be changed using `inspirehep-search-parameters` variable. Setting this variable to a high values increases the time it takes a response from INSPIRE to arrive.

By default this package will fetch the pages in background and insert the results in the buffer as they arrive. This behavior can be disabled by changing `inspirehep-insert-all` to `nil`. You can still use `n` to fetch the next page of the results and insert them into the buffer.

### Target buffer selection

  The variable `inspirehep-target-buffer-function` determines the buffer where the BibTeX will be inserted. By default, the buffer is chosen based on the major
  mode of the buffer that was current when `inspirehep-lookup` was called. If it was a `LaTeX-mode` buffer, the entry will be inserted in the corresponding
  `BibTeX` file, if it was a `bibtex-mode` buffer the entry will be inserted in the file it was visiting, if it was an `inspirehep-mode` buffer, the target
  buffer is inherited from it. In other cases, the entry isn't inserted in any file unless the target buffer is selected manually.

## Example

* To search for new papers in the `hep-th` category use `M-x inspirehep-lookup dadd today + primarch hep-th RET`.

The syntax for search queries is explained at [https://help.inspirehep.net/knowledge-base/inspire-paper-search](https://help.inspirehep.net/knowledge-base/inspire-paper-search).

## Acknowledgments

This package started as an attempt to add INSPIRE as a backend to [biblio.el](https://github.com/cpitclaudel/biblio.el) which is still a much better option for
general reference search. But I wanted to make enough minor changes to the internals of it that I eventually extracted the parts I needed but they still make
majority of the code in this package.
