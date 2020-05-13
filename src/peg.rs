use crate::ClonedToken;
use std::ops::Range;

use ::peg::{
	Parse,
	ParseElem,
	RuleResult,
	ParseSlice,
};

#[derive(Clone)]
pub struct ClonedTokenWithPos {
	pub token: ClonedToken,
	pub range: Range<usize>,
}

pub struct ClonedTokensWithPos(pub Vec<ClonedTokenWithPos>);

#[derive(Debug, PartialEq, Eq)]
pub struct CustomRange(pub Range<usize>);

impl std::fmt::Display for CustomRange {
	fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::result::Result<(), ::std::fmt::Error> {
		write!(fmt, "{}..{}", self.0.start, self.0.end)
	}
}

impl Parse for ClonedTokensWithPos {
	type PositionRepr = CustomRange;
	fn start(&self) -> usize {
		0
	}

	fn position_repr(&self, pos: usize) -> Self::PositionRepr {
		CustomRange(self.0[pos].range.clone())
	}
}

impl ParseElem for ClonedTokensWithPos {
	type Element = ClonedToken;

	fn parse_elem(&self, pos: usize) -> RuleResult<Self::Element> {
		match self.0.get(pos) {
			Some(c) => RuleResult::Matched(pos + 1, c.token.clone()),
			None => RuleResult::Failed,
		}
	}
}

impl<'input> ParseSlice<'input> for ClonedTokensWithPos {
	type Slice = &'input [ClonedTokenWithPos];

	fn parse_slice(&'input self, p1: usize, p2: usize) -> Self::Slice {
		&self.0[p1..p2]
	}
}

impl ClonedTokensWithPos {
	pub fn len(&self) -> usize {
		self.0.len()
	}

	pub fn is_empty(&self) -> bool {
		self.0.is_empty()
	}
}
