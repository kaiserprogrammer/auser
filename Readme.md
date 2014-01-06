# A|User

I found that through dynamic scope user authentication can be an
orthagonal concept in lisp, so I put out an earlier implementation of
mine and here you go.

NOTE: I do not mean the special variable *user-db*

## Usage

```lisp
(let ((*user-db* (make-instance 'memory-db)))
  (add "blub" "secret")
  (assert-true (verify "blub" "secret"))
  (assert-false (verify "blub" "wrong")))
```
