cl-arxiv-api
------------

A tiny wrapping layer on top of web-API of www.arXiv.org
http://arxiv.org/help/api/index

The main API function is ARXIV-GET, parameters of which literally parallel
parameters of http query (except some parameters are keywords and are keyword-valued).

```lisp
(arxiv-get '(:author "Popolitov") :max-results 1 :sort-order :asc :sort-by :submit)

((:SELF-LINK
  . "http://arxiv.org/api/query?search_query%3Dau%3A%20Popolitov%26id_list%3D%26start%3D0%26max_results%3D1")
 (:TITLE
  . "ArXiv Query: search_query=au: Popolitov&id_list=&start=0&max_results=1")
 (:ID . "http://arxiv.org/api/eqh0wUfSoqUlr9BWqd8knMzmeUA")
 (:UPDATED . "2015-08-16T00:00:00-04:00") (:TOTAL-RESULTS . 13)
 (:START-INDEX . 0) (:ITEMS-PER-PAGE . 1)
 (:ENTRY (:ID . "http://arxiv.org/abs/0710.2073v2")
  (:UPDATED . "2007-10-12T11:18:49Z") (:PUBLISHED . "2007-10-10T17:58:06Z")
  (:TITLE . "On coincidence of Alday-Maldacena-regularized $σ$-model and
  Nambu-Goto areas of minimal surfaces")
  (:SUMMARY . "  For the $\\sigma$-model and Nambu-Goto actions, values of the
Alday-Maldacena-regularized actions are calculated on solutions of the
equations of motion with constant non-regularized Lagrangian. It turns out that
these values coincide up to a factor, independent of boundary conditions.
")
  (:AUTHOR . "A. Popolitov") (:DOI . "10.1134/S0021364007210011")
  (:RELATED-LINK . "http://dx.doi.org/10.1134/S0021364007210011")
  (:COMMENT . "3 pages, submitted to JETP Letters, corrected some misprints and
  missing factors in formulas (10) and (11)")
  (:JOURNAL-REF . "JETP Lett.86:559-561,2008")
  (:ALTERNATE-LINK . "http://arxiv.org/abs/0710.2073v2")
  (:RELATED-LINK . "http://arxiv.org/pdf/0710.2073v2")
  (:PRIMARY-CATEGORY . "hep-th") (:CATEGORY . "hep-th")))
```

As is clear from the example, ARXIV-GET parses XML returned by arXiv into simple assoc-list.
CL-ARXIV-API exports two more functions:
  -- ARXIV-GET-RAW, which does not do the parsing, returing raw XML
  -- PARSE-ARXIV-RESPONSE, which actually does parsing of XML to assoc-list

