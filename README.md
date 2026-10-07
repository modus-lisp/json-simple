# json-simple

Simple JSON for the modus-lisp stack: plain Common Lisp functions, no dependencies.

`parse` and `stringify` are drop-in replacements for
[jzon](https://github.com/Zulu-Inuoe/jzon)'s, which is also pure Common Lisp. The difference is in how it's built.
- **Same mapping, same output:** the same value mapping, and byte-for-byte the same output, compact or pretty.
- **Simple, so fast on modus:** only plain functions, with no generic functions, CLOS or Gray streams. jzon writes every character through all three, and on modus each costs microseconds per call.

```lisp
(json-simple:parse "{\"a\":[1,2.5,true,null]}")   ; => #<HASH-TABLE :TEST EQUAL>
(json-simple:stringify (json-simple:parse "[1,{\"b\":false}]") :pretty t)
```

## Mapping

| JSON   | Lisp (default)            |
|--------|---------------------------|
| object | hash-table, `:test equal` |
| array  | simple-vector             |
| string | simple-string             |
| number | integer or double-float   |
| true   | `t`                       |
| false  | `nil`                     |
| null   | the symbol `cl:null`      |

`parse` can produce other representations too.

```lisp
(json-simple:parse text :object-type :alist :true :true :false :false :null :null)
```

That call gives objects as `((key . value) ...)` in source order, with false and null distinct from the empty list. This is what a Nostr relay wants.

`stringify` also accepts:
- other symbols (written as their name);
- characters, pathnames, ratios (as the nearest double);
- lists and multi-dimensional arrays (as nested arrays).

Object keys follow jzon's coercion: symbols are downcased unless they contain lowercase letters.

## API

- **`(parse in &key max-depth object-type true false null start end)`**
  - `in`: a string, a `(vector (unsigned-byte 8))` of UTF-8, a stream, or a pathname.
  - `start`/`end`: parse only that part of a string or octet vector, such as one frame in a network buffer. Error positions count from `start`.
  - Parsing is strict RFC 8259.
  - Signals `json-parse-error`; `json-parse-error-position` gives the character index where parsing failed.
- **`(stringify value &key stream pretty)`**
  - `stream`: `nil` (return a string), `t`, a stream, a string with a fill pointer, or a pathname.
  - Signals `json-write-error` for values it can't write.
- **`(write-json-string string stream &key canonical)`**
  - Writes one string literal.
  - `:canonical t` is NIP-01's event-id serialization: only `\" \\ \b \f \n \r \t` are escaped, so the id you compute is the id the author signed.

## Speed

`test/bench.lisp` runs both libraries on a 3.7 KB message shaped like an LLM chat request: model, temperature, 12 messages and 8 tool schemas.

| per message        | SBCL      | modus      |
|--------------------|-----------|------------|
| jzon stringify     | 0.072 ms  | 17.9 ms    |
| json-simple stringify| 0.034 ms  | **1.4 ms** |
| jzon parse         | 0.051 ms  | 2.5 ms     |
| json-simple parse    | 0.041 ms  | **1.25 ms**|

## Compatibility with jzon

`test/oracle.lisp` runs json-simple against jzon itself:
- **JSONTestSuite:** all 318 files are accepted or rejected the same way, and accepted ones parse to equal values, except for the five listed below.
- **Random doubles:** 20,000 (subnormals included) print identically and read back exactly.
- **Random value trees:** 3,000, compact and pretty, stringify identically, and jzon's text parses back to equal values.
- **Number edge cases:** range limits, `-0`, leading zeros.

Run it on SBCL with `./run-tests.sh`.

Known differences:
- **Surrogate pairs above U+1FFFF:** jzon combines a `\uD8xx\uDCxx` pair with `logior` instead of `+`. Planes 2–16 come out 0x10000 short, so `"\uDBFF\uDFFF"` reads as U+FFFFF. json-simple decodes these correctly; the oracle checks those two suite files against the right answer.
- **`replacer`, `coerce-key` and the streaming writer/parser APIs:** not provided.
- **CLOS instances and structures:** not walked into objects; build a hash table.
- **Lone surrogates:** every lone surrogate escape is an error, so each string json-simple returns is valid Unicode and encodes to UTF-8 (as octet input already must). jzon errors on a lone high surrogate but keeps a lone low one; the oracle checks that json-simple rejects those three suite files.

## License

MIT
