// This file is Copyright its original authors, visible in version control history.
//
// This file is licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. You may not use this file except in
// accordance with one or both of these licenses.

#![crate_name = "minurl"]
#![deny(missing_docs)]
#![deny(rustdoc::broken_intra_doc_links)]
#![deny(rustdoc::private_intra_doc_links)]
#![allow(bare_trait_objects)]
#![allow(ellipsis_inclusive_range_patterns)]
#![cfg_attr(docsrs, feature(doc_cfg))]

//! A minimal library for parsing and validating URLs.

/// Errors that can occur during URL parsing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
	/// The input string is empty.
	EmptyInput,
	/// The input contains invalid characters (control characters or non-ASCII).
	InvalidCharacter(char),
	/// The URL is missing a scheme.
	MissingScheme,
	/// The URL has an invalid scheme format.
	InvalidScheme,
	/// The port number is invalid.
	InvalidPort,
}

/// A parsed URL.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Url {
	scheme: String,
	base_url: String,
	port: Option<u16>,
}

impl Url {
	/// Parses a URL string and returns a `Url` instance.
	///
	/// Validates that the input contains only valid non-control ASCII characters.
	pub fn parse(url_str: &str) -> Result<Self, ParseError> {
		if url_str.is_empty() {
			return Err(ParseError::EmptyInput);
		}

		// Validate: only non-control ASCII characters allowed
		for c in url_str.chars() {
			if !c.is_ascii() || c.is_ascii_control() {
				return Err(ParseError::InvalidCharacter(c));
			}
		}

		// Find the scheme (everything before "://")
		let scheme_end = url_str.find("://").ok_or(ParseError::MissingScheme)?;

		if scheme_end == 0 {
			return Err(ParseError::InvalidScheme);
		}

		let scheme = &url_str[..scheme_end];

		// Validate scheme: must start with a letter and contain only
		// letters, digits, '+', '-', or '.'
		let mut scheme_chars = scheme.chars();
		let first_char = scheme_chars.next().ok_or(ParseError::InvalidScheme)?;
		if !first_char.is_ascii_alphabetic() {
			return Err(ParseError::InvalidScheme);
		}

		for c in scheme_chars {
			if !c.is_ascii_alphanumeric() && c != '+' && c != '-' && c != '.' {
				return Err(ParseError::InvalidScheme);
			}
		}

		// Parse the rest after "://"
		let after_scheme = &url_str[scheme_end + 3..];

		// Extract the authority (host:port) - everything before '/', '?', or '#'
		let authority_end =
			after_scheme.find(|c| c == '/' || c == '?' || c == '#').unwrap_or(after_scheme.len());

		let authority = &after_scheme[..authority_end];

		// Parse host and optional port from authority
		let (host, port) = if let Some(colon_pos) = authority.rfind(':') {
			let potential_port = &authority[colon_pos + 1..];
			// Check if this is actually a port (all digits) or part of IPv6
			if !potential_port.is_empty()
				&& potential_port.chars().all(|c| c.is_ascii_digit())
				&& !authority.starts_with('[')
			{
				let port_num: u16 = potential_port.parse().map_err(|_| ParseError::InvalidPort)?;
				(&authority[..colon_pos], Some(port_num))
			} else {
				(authority, None)
			}
		} else {
			(authority, None)
		};

		Ok(Url { scheme: scheme.to_lowercase(), base_url: host.to_string(), port })
	}

	/// Returns the scheme of the URL (e.g., "http", "https").
	pub fn scheme(&self) -> &str {
		&self.scheme
	}

	/// Returns the base URL (host portion).
	pub fn base_url(&self) -> &str {
		&self.base_url
	}

	/// Returns the port number if specified.
	pub fn port(&self) -> Option<u16> {
		self.port
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn parse_simple_url() {
		let url = Url::parse("http://example.com").unwrap();
		assert_eq!(url.scheme(), "http");
		assert_eq!(url.base_url(), "example.com");
		assert_eq!(url.port(), None);
	}

	#[test]
	fn parse_url_with_port() {
		let url = Url::parse("https://example.com:8080").unwrap();
		assert_eq!(url.scheme(), "https");
		assert_eq!(url.base_url(), "example.com");
		assert_eq!(url.port(), Some(8080));
	}

	#[test]
	fn parse_url_with_path() {
		let url = Url::parse("http://example.com/path/to/resource").unwrap();
		assert_eq!(url.scheme(), "http");
		assert_eq!(url.base_url(), "example.com");
		assert_eq!(url.port(), None);
	}

	#[test]
	fn empty_input_returns_error() {
		assert_eq!(Url::parse(""), Err(ParseError::EmptyInput));
	}

	#[test]
	fn missing_scheme_returns_error() {
		assert_eq!(Url::parse("example.com"), Err(ParseError::MissingScheme));
	}

	#[test]
	fn invalid_character_returns_error() {
		// Control character
		assert!(matches!(
			Url::parse("http://example\x00.com"),
			Err(ParseError::InvalidCharacter('\x00'))
		));

		// Non-ASCII character
		assert!(matches!(Url::parse("http://exämple.com"), Err(ParseError::InvalidCharacter('ä'))));
	}

	#[test]
	fn scheme_is_lowercased() {
		let url = Url::parse("HTTP://EXAMPLE.COM").unwrap();
		assert_eq!(url.scheme(), "http");
	}
}
