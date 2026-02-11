# json-query

`json-query` is a small Haskell library and command-line tool inspired by [`jq`](https://github.com/jqlang/jq) for parsing JSON and running structured queries against it. It includes:

- A JSON parser and AST
- A query language with its own parser and evaluator
- A colorized pretty-printer for JSON output in the terminal

## Features

### JSON Support

- Objects, arrays, strings, numbers, booleans, and null.
- Pretty-printed, syntax-highlighted output in the terminal.
- Fully [RFC](https://www.rfc-editor.org/rfc/rfc8259) compliant.
- Duplicate keys are rejected.

### Query Language

The following query constructs are supported:

**Identity (`.`).** It takes the input and produces the same value as output.

- Query: `.`
- Input: `{"foo": 1, "bar": null}`
- Output: `{"foo": 1, "bar": null}`

**Iterator (`.[]`).** Iterate over an array's or object's values. Example:

- Query: `.[]`
- Input: `{"foo": 1, "bar": null}`
- Output:

```text
1
null
```

- Query: `.[]`
- Input: `[1,2,3]`
- Output:

```text
1
2
3
```

**Index an object (`.key` or `."key"` or `.["key"]`).** Access an object's field by key.

The simple `.key` syntax only works if the key is only made from alphanumeric characters or underscores, but don't start with a digit, otherwise, use any of the other two.

- Query: `.name`
- Input: `{"name": "Alice", "age": 30}`
- Output: `"Alice"`

Quoted key.

- Query: `.["name"]`
- Input: `{"name": "Alice", "age": 30}`
- Output: `"Alice"`

Keys can also be chained.

- Query: `.address."geo".["lat"]`
- Input: `"address": { "geo": { "lat": "-37.3159", "lng": "81.1496" } }`
- Output: `"-37.3159"`

If the indexed element is not an object, it will fail.

- Query: `.foo`
- Input: `null`
- Output: `"Error while performing query. Cannot index null with a string key."`

If a question mark `?` is added, it will not fail if it's not an object.

- Query: `.foo?`
- Input: `null`
- Output:

**Array index (`.[<number>]`).** Index an array.

Arrays are zero-based indexed. If the index is out of bounds, it returns an empty result.

- Query: `.[1]`
- Input: `[1,2,3,4,5]`
- Output: `2`

The index can be negative, `-1` is the last element, `-2` is the next to last element, etc.

- Query: `.[-2]`
- Input: `[1,2,3,4,5]`
- Output: `4`

If the indexed element is not an array, it will fail.

- Query: `.[1]`
- Input: `null`
- Output: `"Error while performing query. Cannot index null with a number."`

If a question mark `?` is added, it will not fail if it's not an array.

- Query: `.[1]?`
- Input: `null`
- Output:

**Pipe (`|`).** Pipes the result from one query into another. It is fail-fast: if any operation fails, the entire pipeline fails.

- Query: `.[] | .id`
- Input: `[{"id": 1}, {"id": 2}, {"foo": 3}]`
- Output:

```text
1
2

```

**Comma (`,`).** Executes two operations with the same input and concatenates the results. It has precedence over pipes. It is fail-fast: if any operation fails, the entire pipeline fails. It has precedence over pipes.

- Query: `.[1], .[0] | .id`
- Input: `[{"id": 1}, {"id": 2}, {"foo": 3}]`
- Output:

```text
{
  "id": 2
}
1
```

**Array and string slice (`.[<number>:<number>]`)**. Return a subarray of an array or substring of a string, from `.[start:end]`. Both `start` and `end` are optional, but one must be present.

- Query: `.[1:4]`
- Input: `"abcdef"`
- Output: `"bcd"`

If the start index is omitted, it goes from index `0` to `end`

- Query: `.[:3]`
- Input: `"abcdef"`
- Output: `"abc"`

If the end index is omitted, it goes from index `start` to the end of the list or string

- Query: `.[1:]`
- Input: `"abcdef"`
- Output: `"bcdef"`

**Parentheses (`(` `)`).** Group queries.

- Query: `(.[1], .[0]) | .id`
- Input: `[{"id": 1}, {"id": 2}, {"foo": 3}]`
- Output:

```text
2
1
```

## Building

Requires `GHC` and `Cabal`.

```shell
cabal build
```

## Usage

The input is taken from the standard input

```shell
echo '{"name": "Alice", "age": 30}' | json-query ".name"
json-query ".name" < user.json
```

## Running Tests

```shell
cabal test
```

### License

GPL-3.0-or-later
