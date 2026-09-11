# binary-protocol

Python [`struct`](https://docs.python.org/3/library/struct.html) pack / unpack for [cl-stack](https://github.com/egao1980/cl-stack). Pure Lisp. Encodings stay in [`encoding-protocol`](https://github.com/egao1980/encoding-protocol) (RFC 4648 / QP).

```lisp
(asdf:load-system "binary-protocol")

(stack-binary:pack "<I" 1)
;; => #(1 0 0 0)

(stack-binary:unpack ">hhl" (stack-binary:pack ">hhl" 1 2 3))
;; => (1 2 3)

(stack-binary:calcsize "<2hI")
;; => 8
```

Endian: `<` `>` `!` `=` `@`. `@` / `=` use native endian and **standard** sizes (no C alignment).

Codes: `x` `c` `b` `B` `?` `h` `H` `i` `I` `l` `L` `q` `Q` `f` `d` `s`. Not `e` `n` `N` `P` `p`.

## License

MIT — see [LICENSE](LICENSE).
