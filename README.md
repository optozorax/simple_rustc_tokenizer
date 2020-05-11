# simple rustc tokenizer

Based on [rustc_lexer](https://crates.io/crates/rustc_lexer)

# usage

```rust
use Token::*;
let string = r#"ident  = "string\n\u{55}";"#;
assert_eq!(tokenize(string), Ok(vec![
	Ident("ident"),
	Whitespace, 
	Eq,
	Whitespace, 
	UnescapedString("string\n\u{55}".to_string()),
	Semi,
]));
```
