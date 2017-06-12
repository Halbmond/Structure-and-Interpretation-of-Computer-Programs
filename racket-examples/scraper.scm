#lang racket ; Simple web scraper
(require net/url net/uri-codec)
    (define (let-me-google-that-for-you str)
        (let* ([g "http://www.google.com/search?q="]
                [u (string-append g (uri-encode str))]
                [rx #rx"(?<=<h3 class=\"r\">).*?(?=</h3>)"])
           (regexp-match* rx (get-pure-port (string->url u)))))
