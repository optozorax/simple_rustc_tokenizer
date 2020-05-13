# simple rustc tokenizer

Based on [rustc_lexer](https://crates.io/crates/rustc_lexer)

# usage

```rust
use Token::*;
let string = r#"ident  = "string\n\u{55}";"#;
assert_eq!(tokenize(string), Ok(vec![
    TokenWithPos { range: 0..5, token: Ident("ident") },
    TokenWithPos { range: 5..7, token: Whitespace },
    TokenWithPos { range: 7..8, token: Eq },
    TokenWithPos { range: 8..9, token: Whitespace },
    TokenWithPos { range: 9..25, token: UnescapedString("string\n\u{55}".to_string()) },
    TokenWithPos { range: 25..26, token: Semi },
]));
```
