use std::ops::Range;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Token<'a> {
	// Idents
	Ident(&'a str),
	RawIdent(&'a str),
	Lifetime(&'a str),

	// Comments
	LineComment(&'a str),
	BlockComment(&'a str),

	// String and char
	UnescapedString(String),
	Char(char),
	Bytes(Vec<u8>),
	Byte(u8),

	// Numbers
	Int(i64),
	Float(f64),

	// Symbols
	Whitespace,
	Semi,
	Comma,
	Dot,
	OpenParen,
	CloseParen,
	OpenBrace,
	CloseBrace,
	OpenBracket,
	CloseBracket,
	At,
	Pound,
	Tilde,
	Question,
	Colon,
	Dollar,
	Eq,
	Not,
	Lt,
	Gt,
	Minus,
	And,
	Or,
	Plus,
	Star,
	Slash,
	Caret,
	Percent,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Error {
	pub range: Range<usize>,
	pub kind: ErrorKind,
}

#[derive(Debug, Eq, PartialEq)]
pub struct EscapeError {
	pub range: Range<usize>,
	pub kind: rustc_lexer::unescape::EscapeError,
}

#[derive(Debug, Eq, PartialEq)]
pub enum ErrorKind {
	UnknownToken,
	StringNotTerminated,
	StringNotStarted,
	SuffixNotSupported,
	NumParseError,
	CharEscapeError(EscapeError),
	StringEscapeErrors(Vec<EscapeError>),
}

pub fn tokenize(string: &str) -> Result<Vec<Token>, Error> {
	use rustc_lexer::unescape;
	use rustc_lexer::TokenKind::*;
	use rustc_lexer::LiteralKind::*;
	use ErrorKind::*;

	rustc_lexer::tokenize(string)
		.map(|x| (x.kind, x.len))
		.scan((0, Gt, 0), |(pos, _, _), (kind, len)| {
			*pos += len;
			Some((*pos - len, kind, len))
		})
		.map(|(pos, token_kind, len)| {
			let range = pos..pos+len;
			let current_text = &string[range.clone()];

			macro_rules! terminated_or {
				($terminated:ident, $($x:tt)*) => {
					if !$terminated {
						Err(Error { range, kind: ErrorKind::StringNotTerminated })
					} else {
						$($x)*
					}
				};
			}
			
			match token_kind {
				Ident => Ok(Token::Ident(current_text)),
				RawIdent => Ok(Token::RawIdent(&current_text[2..])),
				Lifetime { .. } => Ok(Token::Lifetime(&current_text[1..])),

				LineComment => Ok(Token::LineComment(&string[pos+2..pos+len])),
				BlockComment { terminated } => {
					terminated_or! {
						terminated,
						Ok(Token::BlockComment(&string[pos+2..pos+len-2]))
					}
				},

				Literal { kind, suffix_start } => {
					let text_before_suffix = &string[pos..pos+suffix_start];
					let text_after_suffix = &string[pos+suffix_start..pos+len];

					macro_rules! process_str {
						($range:expr, $container:ident, $method:ident, $constructor:ident) => {
							let mut result = $container::new();
							let mut errors = Vec::new();
							let range = $range;
							rustc_lexer::unescape::$method(&string[range.clone()], &mut |range1, c| {
								match c.map_err(|kind| EscapeError { kind, range: range.start+range1.start..range.start+range1.end }) {
									Ok(c) => result.push(c),
									Err(e) => errors.push(e),
								}
							});
							if errors.is_empty() {
								Ok(Token::$constructor(result))	
							} else {
								Err(Error { range, kind: ErrorKind::StringEscapeErrors(errors) })
							}
						};
					}

					macro_rules! process_raw_str {
						($started:ident, $range:expr, $container:ident, $method:ident, $constructor:ident) => {
							if !$started {
								Err(Error { range, kind: ErrorKind::StringNotStarted })
							} else {
								let mut result = $container::new();
								let mut errors = Vec::new();
								let range = $range;
								rustc_lexer::unescape::$method(&string[range.clone()], &mut |range1, c| {
									match c.map_err(|kind| EscapeError { kind, range: range.start+range1.start..range.start+range1.end }) {
										Ok(c) => result.push(c),
										Err(e) => errors.push(e),
									}
								});
								if errors.is_empty() {
									Ok(Token::$constructor(result))	
								} else {
									Err(Error { range, kind: ErrorKind::StringEscapeErrors(errors) })
								}
							}
						};
					}

					macro_rules! process_number {
						($constructor:ident) => {
							if !text_after_suffix.is_empty() {
								Err(Error { range, kind: ErrorKind::SuffixNotSupported })
							} else {
								match text_before_suffix.parse() {
									Ok(val) => Ok(Token::$constructor(val)),
									Err(_) => Err(Error { range, kind: ErrorKind::NumParseError }),
								}
							}
						};
					}

					macro_rules! process_char {
						($range:expr, $constructor:ident, $method:ident) => {
							if !text_after_suffix.is_empty() {
								Err(Error { range, kind: ErrorKind::SuffixNotSupported })
							} else {
								let range = $range;
								match unescape::$method(&string[range.clone()]) {
									Ok(val) => Ok(Token::$constructor(val)),
									Err((_, kind)) => Err(Error { 
										range: range.clone(), 
										kind: ErrorKind::CharEscapeError(EscapeError { range, kind } ) 
									}),
								}
							}
						};
					}

					match kind {
						Str { terminated } => {
							terminated_or! { 
								terminated,
								process_str!{
									range.start+1..range.end-1,
									String,
									unescape_str,
									UnescapedString
								}
							}
						},
						ByteStr { terminated } => {
							terminated_or! {
								terminated,
								process_str! {
									range.start+1+1..range.end-1,
									Vec,
									unescape_byte_str,
									Bytes
								}
							}
						},
						RawStr { n_hashes, started, terminated } => {
							terminated_or! { 
								terminated,
								process_raw_str! {
									started,
									range.start+2+n_hashes..range.end-1-n_hashes,
									String,
									unescape_raw_str,
									UnescapedString
								}
							}
						},
						RawByteStr { n_hashes, started, terminated } => {
							terminated_or! { 
								terminated,
								process_raw_str! {
									started,
									range.start+2+n_hashes+1..range.end-1-n_hashes,
									Vec,
									unescape_raw_byte_str,
									Bytes
								}
							}
						},
						Int { .. } => process_number!(Int),
						Float { .. } => process_number!(Float),
						Char { terminated } => {
							terminated_or! { 
								terminated,
								process_char!{
									range.start+1..range.end-1,
									Char,
									unescape_char
								}
							}
						},
						Byte { terminated } => {
							terminated_or! { 
								terminated,
								process_char!{
									range.start+1+1..range.end-1,
									Byte,
									unescape_byte
								}
							}
						},
					}
				},
				
				Whitespace => Ok(Token::Whitespace),
				Semi => Ok(Token::Semi),
				Comma => Ok(Token::Comma),
				Dot => Ok(Token::Dot),
				OpenParen => Ok(Token::OpenParen),
				CloseParen => Ok(Token::CloseParen),
				OpenBrace => Ok(Token::OpenBrace),
				CloseBrace => Ok(Token::CloseBrace),
				OpenBracket => Ok(Token::OpenBracket),
				CloseBracket => Ok(Token::CloseBracket),
				At => Ok(Token::At),
				Pound => Ok(Token::Pound),
				Tilde => Ok(Token::Tilde),
				Question => Ok(Token::Question),
				Colon => Ok(Token::Colon),
				Dollar => Ok(Token::Dollar),
				Eq => Ok(Token::Eq),
				Not => Ok(Token::Not),
				Lt => Ok(Token::Lt),
				Gt => Ok(Token::Gt),
				Minus => Ok(Token::Minus),
				And => Ok(Token::And),
				Or => Ok(Token::Or),
				Plus => Ok(Token::Plus),
				Star => Ok(Token::Star),
				Slash => Ok(Token::Slash),
				Caret => Ok(Token::Caret),
				Percent => Ok(Token::Percent),
				Unknown => Err(Error { range, kind: UnknownToken }),
			}
		})
		.collect()
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum ClonedToken {
	// Idents
	Ident(String),
	RawIdent(String),
	Lifetime(String),

	// Comments
	LineComment(String),
	BlockComment(String),

	// String and char
	UnescapedString(String),
	Char(char),
	Bytes(Vec<u8>),
	Byte(u8),

	// Numbers
	Int(i64),
	Float(f64),

	// Symbols
	Whitespace,
	Semi,
	Comma,
	Dot,
	OpenParen,
	CloseParen,
	OpenBrace,
	CloseBrace,
	OpenBracket,
	CloseBracket,
	At,
	Pound,
	Tilde,
	Question,
	Colon,
	Dollar,
	Eq,
	Not,
	Lt,
	Gt,
	Minus,
	And,
	Or,
	Plus,
	Star,
	Slash,
	Caret,
	Percent,
}

impl From<Token<'_>> for ClonedToken {
    fn from(item: Token<'_>) -> Self {
    	use Token::*;
		match item {
			Ident(inner) => ClonedToken::Ident(inner.to_string()),
			RawIdent(inner) => ClonedToken::RawIdent(inner.to_string()),
			Lifetime(inner) => ClonedToken::Lifetime(inner.to_string()),

			LineComment(inner) => ClonedToken::LineComment(inner.to_string()),
			BlockComment(inner) => ClonedToken::BlockComment(inner.to_string()),

			UnescapedString(inner) => ClonedToken::UnescapedString(inner),
			Char(inner) => ClonedToken::Char(inner),
			Bytes(inner) => ClonedToken::Bytes(inner),
			Byte(inner) => ClonedToken::Byte(inner),

			Int(inner) => ClonedToken::Int(inner),
			Float(inner) => ClonedToken::Float(inner),

			Whitespace => ClonedToken::Whitespace,
			Semi => ClonedToken::Semi,
			Comma => ClonedToken::Comma,
			Dot => ClonedToken::Dot,
			OpenParen => ClonedToken::OpenParen,
			CloseParen => ClonedToken::CloseParen,
			OpenBrace => ClonedToken::OpenBrace,
			CloseBrace => ClonedToken::CloseBrace,
			OpenBracket => ClonedToken::OpenBracket,
			CloseBracket => ClonedToken::CloseBracket,
			At => ClonedToken::At,
			Pound => ClonedToken::Pound,
			Tilde => ClonedToken::Tilde,
			Question => ClonedToken::Question,
			Colon => ClonedToken::Colon,
			Dollar => ClonedToken::Dollar,
			Eq => ClonedToken::Eq,
			Not => ClonedToken::Not,
			Lt => ClonedToken::Lt,
			Gt => ClonedToken::Gt,
			Minus => ClonedToken::Minus,
			And => ClonedToken::And,
			Or => ClonedToken::Or,
			Plus => ClonedToken::Plus,
			Star => ClonedToken::Star,
			Slash => ClonedToken::Slash,
			Caret => ClonedToken::Caret,
			Percent => ClonedToken::Percent,
		}    
    }
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn all_tokens() {
		let string = r###"
		ident идент
		r#rawident
		'1 'лайфтайм
		// comment
		/// doc-comment
		/* multi-line comment */
		/*! multi-line doc comment */
		"string\u{55}\n"
		r#".""."#
		r##"."#"."##
		'ё'
		b"hello"
		br#".""."#
		br##"."#"."##
		b'h'
		13
		2.55
		;
		,
		.
		(
		)
		{
		}
		[
		]
		@
		~
		?
		:
		$
		=
		!
		<
		>
		-
		&
		|
		+
		*
		/
		^
		%
		_
		"###;
		use Token::*;
		assert_eq!(tokenize(string), Ok(vec![
			Whitespace, Ident("ident"),
			Whitespace, Ident("идент"),
			Whitespace, RawIdent("rawident"),
			Whitespace, Lifetime("1"),
			Whitespace, Lifetime("лайфтайм"),
			Whitespace, LineComment(" comment"),
			Whitespace, LineComment("/ doc-comment"),
			Whitespace, BlockComment(" multi-line comment "),
			Whitespace, BlockComment("! multi-line doc comment "),
			Whitespace, UnescapedString("string\u{55}\n".to_string()),
			Whitespace, UnescapedString(".\"\".".to_string()),
			Whitespace, UnescapedString(".\"#\".".to_string()),
			Whitespace, Char('ё'),
			Whitespace, Bytes(br#"hello"#.to_vec()),
			Whitespace, Bytes(br#".""."#.to_vec()),
			Whitespace, Bytes(br##"."#"."##.to_vec()),
			Whitespace, Byte(b'h'),
			Whitespace, Int(13),
			Whitespace, Float(2.55),
			Whitespace, Semi,
			Whitespace, Comma,
			Whitespace, Dot,
			Whitespace, OpenParen,
			Whitespace, CloseParen,
			Whitespace, OpenBrace,
			Whitespace, CloseBrace,
			Whitespace, OpenBracket,
			Whitespace, CloseBracket,
			Whitespace, At,
			// Whitespace, Pound, // What is this?
			Whitespace, Tilde,
			Whitespace, Question,
			Whitespace, Colon,
			Whitespace, Dollar,
			Whitespace, Eq,
			Whitespace, Not,
			Whitespace, Lt,
			Whitespace, Gt,
			Whitespace, Minus,
			Whitespace, And,
			Whitespace, Or,
			Whitespace, Plus,
			Whitespace, Star,
			Whitespace, Slash,
			Whitespace, Caret,
			Whitespace, Percent,
			Whitespace, Ident("_"),
			Whitespace,
		]));
	}

	#[test]
	fn simple() {
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
	}
}