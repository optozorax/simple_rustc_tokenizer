use simple_rustc_tokenizer::{
	ClonedToken,
	tokenize,
};

use simple_rustc_tokenizer::peg::*;

peg::parser!{ grammar list_parser() for ClonedTokensWithPos {
	use ClonedToken::*;

	pub(super) rule main() -> (String, Vec<i64>)
		= name:ident() __ [Eq] __ [OpenBracket] x:(__ x:int() __ { x }) ** [Comma] __ [CloseBracket] { (name, x) }

	// Wait for https://github.com/kevinmehall/rust-peg/issues/227
	rule int() -> i64
		= n:$[Int(_)] { 
			match &n[0].token {
				Int(inner) => *inner,
				_ => unreachable!(),
			}
		}

	rule ident() -> String
		= n:$[Ident(_)]  { 
			match &n[0].token {
				Ident(inner) => inner.clone(),
				_ => unreachable!(),
			}
		}

	rule _() 
		= [Whitespace]
		/ [LineComment(_)]
		/ [BlockComment(_)]

	rule __() = _*
} }

#[test]
fn peg_parsing() {
	let string = "five_шесть = [5/* comment */  ,6  // привет\n]";
	let arr = tokenize(string)
		.unwrap()
		.into_iter()
		.map(|x| ClonedTokenWithPos {
			token: x.token.into(),
			range: x.range,
		})
		.collect::<Vec<_>>();
	let result = list_parser::main(&ClonedTokensWithPos(arr));
	if let Err(err) = &result {
		println!("{}", &string[err.location.0.clone()]);
	}
	assert_eq!(result, Ok(("five_шесть".to_string(), vec![5, 6])));
}
