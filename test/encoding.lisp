
(in-package :m2cl-test)

(in-suite main)

(test url-decode
  (is (string= (m2cl:url-decode "") ""))
  (is (string= (m2cl:url-decode "abc") "abc"))
  (is (string= (m2cl:url-decode "foo+bar") "foo bar"))
  (is (string= (m2cl:url-decode "foo%20bar") "foo bar"))
  (is (string= (m2cl:url-decode "%C3%A9tat") "état"))
  (is (string= (m2cl:url-decode "%c3%a9tat") "état"))
  (is (string= (m2cl:url-decode "%E2%88%A7") "∧"))
  (signals error (m2cl:url-decode "%"))
  (signals error (m2cl:url-decode "%2"))
  (signals error (m2cl:url-decode "%e9")))

(test query-decode
  (is (equal (m2cl::query-parse "")
             '()))
  (is (equal (m2cl::query-parse "foo=")
             (list '("foo" . nil))))
  (is (equal (m2cl::query-parse "foo=bar")
             (list '("foo" . "bar"))))
  (is (equal (m2cl::query-parse "foo=&foo2=baz")
             (list '("foo" . nil) '("foo2" . "baz"))))
  (is (equal (m2cl::query-parse "foo=;foo2=baz")
             (list '("foo" . nil) '("foo2" . "baz"))))
  (is (equal (m2cl::query-parse "foo=bar&foo2=baz")
             (list '("foo" . "bar") '("foo2" . "baz"))))
  (signals error (m2cl::query-parse "foo=ok&bar")))
