
(in-package :m2cl-test)

(in-suite main)

(test url-decode
  (macrolet ((test-url (url expected-result)
               `(is (string= (m2cl:url-decode ,url) ,expected-result))))
    (test-url "" "")
    (test-url "abc" "abc")
    (test-url "foo+bar" "foo bar")
    (test-url "foo%20bar" "foo bar")
    (test-url "%C3%A9tat" "état")
    (test-url "%c3%a9tat" "état")
    (test-url "%E2%88%A7" "∧"))
  (signals error (m2cl:url-decode "%"))
  (signals error (m2cl:url-decode "%2"))
  (signals error (m2cl:url-decode "%e9")))

(test query-decode
  (macrolet ((test-query (query expected-result)
               `(is (equal (m2cl::query-parse ,query) ',expected-result))))
    (test-query "" ())
    (test-query "foo=" (("foo" . nil)))
    (test-query "foo=bar" (("foo" . "bar")))
    (test-query "foo=&foo2=bar" (("foo" . nil) ("foo2" . "bar")))
    (test-query "foo=&foo2=baz" (("foo" . nil) ("foo2" . "baz")))
    (test-query "foo=;foo2=baz" (("foo" . nil) ("foo2" . "baz")))
    (test-query "foo=bar&foo2=baz" (("foo" . "bar") ("foo2" . "baz"))))
  (signals error (m2cl::query-parse "foo=ok&bar")))
